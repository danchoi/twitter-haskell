{-# LANGUAGE NoMonomorphismRestriction, StandaloneDeriving #-}
module Main  where

import Data.Maybe (fromJust)
import Control.Monad.Trans
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.PercentEncoding
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Binary as Binary
import Control.Monad (liftM)
import qualified Data.Text as T   -- cabal install text
import qualified Data.Text.Encoding as E   -- cabal install text
import qualified Data.Char as Char
import Data.ConfigFile -- ConfigFile 1.1.1
import Control.Monad.Error

deriving instance Show Application
deriving instance Show Token

reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"

serviceUrl = fromJust . parseURL $ "http://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=danchoi"

authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++)
            . findWithDefault ("oauth_token","") . oauthParams

data Consumer = Consumer { key :: String , secret :: String } deriving (Show, Eq)

authenticate :: Application -> IO Token
authenticate app = do 
  runOAuthM (fromApplication app) $ do
    liftIO $ putStrLn "Step 1"

    -- todo: add oauth_callback here
    s1 <- signRq2 HMACSHA1 Nothing reqUrl 
    liftIO $ putStrLn (show s1)
    oauthRequest CurlClient s1
    -- should store the token 
    -- oauth_token, oauth_token_secret

    liftIO $ putStrLn "Step 2"
    cliAskAuthorization authUrl
    -- change this to redirect to 
    -- e.g. https://api.twitter.com/oauth/authenticate?oauth_token=NPcudxy0yU5T3tBzho7iCotZ3cnetKwcTIRlX0iwRl0
    --
    liftIO $ putStrLn "Getting Access Token"

    accessToken <- (signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient)
    liftIO $ putStrLn $ show (oauthParams accessToken)
    return accessToken
{- notes
 - A successful response contains the oauth_token, oauth_token_secret, user_id, and screen_name parameters. The token and token secret should be stored and used for future authenticated requests to the Twitter API.
 - -}

tokenFromFile :: IO Token
tokenFromFile = do
  a <- B.readFile "mytoken"
  let b = Binary.decode a :: Token
  return b


getProfile :: Application -> Token -> String -> IO Response
getProfile app token message = runOAuthM (fromApplication app) $ do
    putToken token
    sr <- signRq2 HMACSHA1 Nothing serviceUrl
    r <- serviceRequest CurlClient sr
    return r

main = do

  Right rv@(status,(k,s,c)) <- runErrorT $ do 
      cp' <- join $ liftIO $ readfile emptyCP "twitter.cfg"
      key <- get cp' "app" "key"
      secret <- get cp' "app" "secret"
      callback <- get cp' "app" "callback"
      return ("done",(key,secret,callback))
  print rv
  let app = Application k s (URL c)
  token <- tokenFromFile
  r <- getProfile app token "test"
  putStrLn (show r)
  -- putStrLn (E.decodeUtf8 $ rspPayload r)
  let p = (rspPayload r)   -- [Word8]
  putStrLn  $ map (Char.chr . fromIntegral) (B.unpack p)
  return ()

{-
main = do
  token <- authenticate app2
  let bytestring = Binary.encode token
  B.writeFile "mytoken" bytestring
  return ()
-}

