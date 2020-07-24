{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (parseURI)
import qualified System.Random as SR
import Web.Scotty

-- List containing alphanumeric characters

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

-- Pick a random char from the above list

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex = length xs - 1
  randomIndex <- SR.randomRIO (0, maxIndex)
  return (xs !! randomIndex)

-- Generate a random string with 7 characters

shortGen :: IO String
shortGen =
  replicateM 7 (randomElement alphaNum)

-- Store a URI on database

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

-- Retrieve a original URI from database

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

-- HTML templates for the shortened link

linkShort :: String -> String
linkShort short =
  "<a href=\"" ++ short ++ "\">Copy and paste your short URL</a>"

shortCreated :: String -> TL.Text
shortCreated short =
  TL.pack $ "Shortened: " <> linkShort short

invalidURI :: TL.Text -> TL.Text
invalidURI uri =
  TL.concat [ uri
            , " wasn't a url,"
            , " did you forget \"http://\" ?"]

foundURI :: TL.Text -> TL.Text
foundURI uri =
  TL.concat [ "<a href=\""
            , uri, "\">"
            , uri, "</a>"
            ]

-- Routes

generateURI :: R.Connection -> ActionM ()
generateURI rConn = do
  uri <- param "uri"

  let parsedURI = parseURI (TL.unpack uri)

  case parsedURI of
    Just _ -> do
      shortStr <- liftIO shortGen

      let shortBc = BC.pack shortStr
          uri' = encodeUtf8 (TL.toStrict uri)

      _ <- liftIO (saveURI rConn shortBc uri')

      html (shortCreated shortStr)
    Nothing -> text (invalidURI uri)

retrieveURI :: R.Connection -> ActionM ()
retrieveURI rConn = do
  short <- param "short"

  uri <- liftIO (getURI rConn short)

  case uri of
    Left reply      -> text . TL.pack $ show reply
    Right Nothing   -> text "uri not found"
    Right (Just bs) -> html (foundURI tbs)
      where tbs = TL.fromStrict (decodeUtf8 bs)

-- Main application

app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ generateURI rConn

  get "/:short" $ retrieveURI rConn

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
