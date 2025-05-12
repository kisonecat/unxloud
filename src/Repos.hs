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
import Data.List (break)
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
import qualified Text.Blaze.Html5.Attributes as A

import GitHub as GH

type API = "books" :> Get '[HTML] Html
      :<|> Capture "owner" String :> Capture "reponame" String :> "the" :> CaptureAll "fullpath" String :> Get '[HTML] Html

getBooks :: (MonadError ServerError m, MonadIO m, MonadDB m, MonadReader r m, HasConfiguration r) => m Html
getBooks = return $ H.docTypeHtml $ do
    H.head $ H.title "Books"
    H.body $ do
      H.h1 "Books"
      H.p "This is a simple HTML page produced by a Blaze template."

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadError ServerError m
  ) =>
  ServerT API m
server = getBooks :<|> ximeraPageHandler

-- Handler for the new endpoint:
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
  result <- GitHub.getPage owner reponame fullPath
  case result of
    Left err -> throwError $ err500 { errBody = cs ("Internal error: " ++ err) }
    Right page ->
      case page of
        PageMissing ->
          return $ H.docTypeHtml $ do
                      H.head $ H.title "404 Not Found"
                      H.body $ do
                        H.h1 "404 Not Found"
                        H.p "The requested page was not found."
        RedirectTo uri ->
          throwError $ err302 { errHeaders = [("Location", cs (uriToString id uri ""))] }
        HTMLContent bs ->
          let bodyContent = extractBody (cs bs)
           in return $ H.docTypeHtml $ do
                H.head $ H.title "Ximera Page"
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
