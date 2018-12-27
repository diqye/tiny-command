{-# LANGUAGE OverloadedStrings,DeriveDataTypeable,QuasiQuotes #-}
module Main where

import Lib


import Data.String
import Control.Monad(msum,guard)
import Control.Monad.Trans.Maybe(runMaybeT,MaybeT(..))
import Control.Monad.IO.Class(liftIO)
import System.IO
import qualified Data.Yaml as Y
import Data.Yaml((.:))
import System.Directory
import System.FilePath.Posix
import System.Environment(getArgs)
import Control.Monad(mzero)
import qualified Data.Text as T
import Control.Applicative((<|>))
import Control.Exception(catch,IOException)
import System.Environment(getArgs)
import NeatInterpolation(text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text.Encoding as E
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Scientific(Scientific)
import Control.Concurrent
import Network.HTTP.Client.MultipartFormData(partFileSource,formDataBody)



data ConfKey = ConfKey { ck_key :: String
                       , ck_name :: String
                       , ck_email :: String
                       } deriving Show
data ConfUpload = ConfUpload { cu_api :: String
                             } deriving Show
data Config = Config { c_api :: String
                     , c_keys :: [ConfKey]
                     , c_upload :: ConfUpload
                     } deriving Show

instance Y.FromJSON ConfKey where
  parseJSON  = Y.withObject "key" $ \v ->
    pure ConfKey
    <*>
    v .: "key"
    <*>
    (v .: "name" <|> return "noname")
    <*>
    (v .: "email" <|> return "noemail")

instance Y.FromJSON ConfUpload where
  parseJSON = Y.withObject "upload" $ \v -> 
    pure ConfUpload <*> v .: "api"

instance Y.FromJSON Config where
  parseJSON = Y.withObject "config" $ \v -> do
     api <- v .: "api"
     keys <- v .: "keys"
     upload <- v .: "upload"
     return Config { c_api = api
                   , c_keys = keys
                   , c_upload = upload
                   }
data CmdArg = CmdV String
  | CmdHelp String
  | CmdUpload String
  | CmdZipUpload String
  | CmdZip String
  deriving Show

cmdV = CmdV "V1.0.0"
cmdHelp = CmdHelp $ T.unpack $ [text|
蜂校FE-图片压缩上传

output目录 ./fe-output/

fx-image path               仅压缩 
fx-image --cUpload path     压缩并上传 
fx-image --onlyUpload path  仅上传
  |]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runMaybeT dologic 
  return ()

myprint :: Show a => a -> MaybeT IO ()
myprint = liftIO . putStrLn . show

printCmd cmd = putStrLn help
  where help = case cmd of (CmdV v) -> v
                           (CmdHelp h) -> h
                           _ -> "undefined"

myprintStr :: String -> MaybeT IO ()
myprintStr = liftIO . putStrLn

dologic :: MaybeT IO ()
dologic = do
  cmdArg <- parsingCmd
  myprintStr "1. 解析yaml配置"
  config <- parsingConfig
  myprintStr "2. 压缩图片"
  let api = c_api config
  let ckeys = c_keys config
  compress <- compressImg api ckeys cmdArg
  myprintStr "3. 上传图片"
  let uploadApi = cu_api $ c_upload $ config
  url <- uploadImg compress uploadApi
  liftIO $ createDirectoryIfMissing False "fe-output"
  appendFileLogic "fe-output/output.log" (compress,url)

appendFileLogic :: String -> ((String,String),String) -> MaybeT IO ()
appendFileLogic path ((origin,target),url) = MaybeT $ do
  let msg =  origin ++ ">压缩后>" ++ target ++ ">上传后>" ++ url ++ "\n"
  appendFile path msg
  putStrLn $ "写入文件:" ++ path
  putStrLn $ "成功:" ++ msg
  return mzero

parsingCmd :: MaybeT IO CmdArg
parsingCmd = MaybeT $ do
  args <- getArgs
  logicfn args
  where logicfn [] = printCmd cmdHelp >> return Nothing
        logicfn ["--version"] = printCmd cmdV >> return Nothing
        logicfn ["--help"] = printCmd cmdHelp >> return Nothing
        logicfn ["--cUpload",path] = createWith path CmdZipUpload
        logicfn ["--onlyUpload",path] = createWith path CmdUpload
        logicfn (('-':'-':_):_)  = printCmd cmdHelp >> return Nothing
        logicfn [path] = createWith path CmdZip
        logicfn _ = printCmd cmdHelp >> return Nothing

        createWith ('~':_:path) construct = do
          homePath <- getHomeDirectory
          return $ Just $ construct $ homePath </> path
        createWith path construct = do
          currPath <- getCurrentDirectory
          return $ Just $ construct $ currPath </> path

parsingConfig :: MaybeT IO Config
parsingConfig = MaybeT $ do
  currentPath <- getHomeDirectory
  let configFile = currentPath ++ "/fe-image.yaml"
  either <- (Y.decodeFileEither configFile) :: IO (Either Y.ParseException Config)
  case either of Left e -> (putStrLn $ show e) >> return Nothing
                 Right c -> return $ Just c


type CompressInfo = (String,String)
compressImg :: String -> [ConfKey] -> CmdArg -> MaybeT IO CompressInfo
compressImg api ckeys cmdArg = do
  compress <- msum $ map (\ckey->compressImg' api ckey cmdArg) ckeys
  continue' cmdArg
  return compress
  where
    continue' (CmdZip _) = mzero
    continue' _          = return ()

compressImg' :: String -> ConfKey -> CmdArg -> MaybeT IO CompressInfo
compressImg' api ckey cmdArg   = MaybeT $ do
  patternCmd cmdArg
  where
    patternCmd (CmdZip path) = ci path >>= tojust path
    patternCmd (CmdZipUpload path) = ci path >>= tojust path
    patternCmd (CmdUpload path) = do
      putStrLn "不需要压缩"
      return $ Just (path,path) 
    patternCmd _ = return Nothing

    tojust :: String -> Maybe String -> IO (Maybe (String,String))
    tojust path target = return $ do
      t <- target
      return (path,t)

    ci :: String -> IO (Maybe String)
    ci path = do
      putStrLn $ "尝试使用" ++ show ckey ++ "压缩"
      manager <- newManager tlsManagerSettings
      request <- parseRequest api
      body <- LB.readFile path
      let rq = applyBasicAuth "api" (fromString $ ck_key ckey)
             $ request { method = "POST"
                       , requestBody = RequestBodyLBS body
                       }
      res <- httpLbs rq manager
      let r = lookup "Location" $ responseHeaders $ res
      let body = responseBody $ res
      let bodyMaybeV = A.decode body
      let r'' = do r' <- r
                   return $ T.unpack $ E.decodeUtf8 r'
 
      let parseName = reverse . takeWhile (/='/') . reverse
      compressPath <- download r'' $ parseName path
      let mratio = do bv <- bodyMaybeV
                      output <- getObject "output" bv
                      ratio <- getNumber "ratio" output
                      rv <- compressPath
                      let ratio' = (1 - ratio)*100
                      let tpath = T.pack path
                      let tratio = T.pack $ show ratio'
                      let trv = T.pack $ rv
                      return $ [text|
                        原文件  : $tpath
                        压缩比例: $tratio %
                        输出文件: $trv |]
      case mratio of Just msg -> putStrLn $ T.unpack msg
                     Nothing  -> LC.putStrLn body
      return compressPath


download :: Maybe String -> String -> IO (Maybe String)
download (Just uri) name = do
  manager <- newManager tlsManagerSettings
  rq <- parseRequest uri
  res <- httpLbs rq manager
  createDirectoryIfMissing False "fe-output"
  LB.writeFile ("fe-output/" ++ name) $ responseBody res
  return $ Just $ "fe-output/" ++ name
download _ _ = return Nothing

{-
jsonv :: Maybe A.Value
jsonv = A.decode  "{\"input\":{\"size\":13960,\"type\":\"image/png\"},\"output\":{\"size\":3771,\"type\":\"image/png\",\"width\":742,\"height\":88,\"ratio\":0.2701,\"url\":\"https://api.tinify.com/output/b9gu9dmr3zzxgtzg7kyx57qdzw86r01g\"}}"
-}

getObject :: T.Text -> A.Value -> Maybe A.Value
getObject key (A.Object o) = HM.lookup key o
getObject _ _= mzero

getNumber :: T.Text -> A.Value -> Maybe Scientific
getNumber  key (A.Object o) = do
  v <- HM.lookup key o
  case v of A.Number o -> return o
            _ -> mzero
getNumber _ _= mzero

getString :: T.Text -> A.Value -> Maybe String
getString  key (A.Object o) = do
  v <- HM.lookup key o
  case v of A.String o -> return $ T.unpack $ o
            _ -> mzero
getString _ _ = mzero

uploadImg :: CompressInfo -> String -> MaybeT IO String
uploadImg (originFile,targetFile) api = MaybeT up1
 where up1 :: IO (Maybe String)
       up1 = do
               manager <- newManager tlsManagerSettings
               request <- parseRequest api
               rq <- formDataBody [partFileSource "diqyefile" targetFile] $ request { method = "POST" }
               res <- httpLbs rq manager
               let body = responseBody $ res
               -- LC.putStrLn body
               let bodyMaybeV = A.decode body
               let murl = do
                    json <- bodyMaybeV
                    r <- getString "data" json
                    return r
               return murl
