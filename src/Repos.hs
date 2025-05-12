{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Repos
  ( API,
    server,
  )
where

import Data.String.Conversions (cs)

import AppM (AppM, HasConfiguration (..), MonadDB (..), getConfiguration, getPool)
import Configuration
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.List (break, intercalate)
import Network.URI (uriToString)
import Data.Maybe
import Data.Pool (withResource)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Redis as R
import Servant
import Servant.Server
import Servant.HTML.Blaze
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html ( (!))
import qualified Text.Blaze.Html5.Attributes as A

import GitHub as GH

type API = (Capture "owner" String :> Capture "reponame" String :> CaptureAll "fullpath" String :> Get '[HTML] Html)
       :<|> ("assets" :> Capture "sha" String :> "bundle.js" :> Get '[PlainText] LBS.ByteString)

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  ServerT API m
server = ximeraPageHandler :<|> shaBundleHandler

ximeraPageHandler ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  String -> String -> [String] -> m Html
ximeraPageHandler owner reponame pathSegments = do
  let fullPath = intercalate "/" pathSegments
  result <- GH.getPage owner reponame fullPath
  config <- asks getConfiguration
  case result of
    Left err -> throwError $ err500 { errBody = cs ("Internal error: " ++ err) }
    Right page ->
      case page of
        GH.PageMissing ->
          return $ H.docTypeHtml $ do
                      H.head $ H.title "404 Not Found"
                      H.body $ do
                        H.h1 "404 Not Found"
                        H.p "The requested page was not found."
        GH.RedirectTo uri ->
          throwError $ err302 { errHeaders = [("Location", cs (uriToString id uri ""))] }
        GH.HTMLContent bs ->
          let bodyContent = extractBody (cs bs)
           in return $ H.docTypeHtml $ do
                H.head $ do
                  H.title "Ximera Page"
                  H.script ! A.src (H.toValue ("/assets/" ++ unSHA (jsBundleSHA config) ++ "/bundle.js")) $ mempty
                H.body $ H.preEscapedToMarkup bodyContent

-- Extract the content between <body> and </body> tags.
extractBody :: String -> String
extractBody content =
  let startTag = "<body>"
      endTag = "</body>"
      startIdx = findSubstring startTag content
      endIdx = findSubstring endTag content
  in case (startIdx, endIdx) of
       (Just s, Just e) -> take (e - (s + length startTag)) (drop (s + length startTag) content)
       _ -> content

-- Simple implementation of findSubstring (returns the index of the first occurrence of pat in str)
findSubstring :: String -> String -> Maybe Int
findSubstring pat str = search 0
  where
    patLen = length pat
    strLen = length str
    search i | i > strLen - patLen = Nothing
             | take patLen (drop i str) == pat = Just i
             | otherwise = search (i + 1)

-- New endpoint: Serve bundle.js if provided SHA matches the configuration.
shaBundleHandler ::
  ( MonadIO m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  String -> m LBS.ByteString
shaBundleHandler providedSHA = do
  config <- asks getConfiguration
  if providedSHA == unSHA (jsBundleSHA config)
    then liftIO $ LBS.readFile "static/js/bundle.js"
    else throwError err404 { errBody = "File not found" }
