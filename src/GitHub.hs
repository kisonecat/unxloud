{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GitHub
  ( getRepo,
    getRepoMain,
    getTree,
    getPage,
    XimeraPage(..)
  )
where

import GHC.Generics

import AppM (AppM, HasConfiguration (..), MonadDB (..))
import Configuration
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson (FromJSON, eitherDecode)
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 (pack)
import Control.Monad              (unless)
import Data.ByteString.Lazy (ByteString)
import Data.List (break, isPrefixOf)
import Data.Maybe
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Redis qualified as R
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeaders)
import Network.HTTP.Types.Header (hAuthorization, hUserAgent, hAccept, hContentType)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Client (responseHeaders, responseBody, responseStatus)
import Network.URI

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data Owner = Owner
  { login      :: String
  , ownerId    :: Integer
  , avatarUrl  :: String
  , gravatarId :: String
  , url        :: String
  } deriving (Show, Generic)

instance FromJSON Owner where
  parseJSON = withObject "Owner" $ \v -> Owner
    <$> v .: "login"
    <*> v .: "id"
    <*> v .: "avatar_url"
    <*> v .: "gravatar_id"
    <*> v .: "url"

data Author = Author
  { name  :: String
  , email :: String
  , date  :: String
  } deriving (Show, Generic)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \v -> Author
    <$> v .: "name"
    <*> v .: "email"
    <*> v .: "date"

data Commit = Commit
  { sha :: String
  , author :: Author
  , committer :: Author
  , message :: String
  , commitTreeSha :: String
  , commitTreeUrl :: String
  , githubAuthor :: Owner
  , githubCommitter :: Owner
  } deriving (Show, Generic)

instance FromJSON Commit where
  parseJSON = withObject "Commit" $ \v -> Commit
    <$> v .: "sha"
    <*> (v .: "commit" >>= (.: "author"))
    <*> (v .: "commit" >>= (.: "committer"))
    <*> (v .: "commit" >>= (.: "message"))
    <*> (v .: "commit" >>= (.: "tree") >>= (.: "sha"))
    <*> (v .: "commit" >>= (.: "tree") >>= (.: "url"))
    <*> v .: "author"
    <*> v .: "committer"

data Repository = Repository
  { repositoryId      :: Integer
  , repositoryName    :: String
  , fullName          :: String
  , private           :: Bool
  , owner             :: Owner
  , description       :: Maybe String
  , defaultBranch     :: String
  } deriving (Show, Generic)

instance FromJSON Repository where
  parseJSON = withObject "Repository" $ \v -> Repository
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "full_name"
    <*> v .: "private"
    <*> v .: "owner"
    <*> v .:? "description"
    <*> v .: "default_branch"

data Entry = Entry
  { entrySha :: String
  , entryUrl :: String
  , entryType :: String
  , entryPath :: String
  } deriving (Show, Generic)

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \v -> Entry
      <$> v .: "sha"
      <*> v .: "url"
      <*> v .: "type"
      <*> v .: "path"

data Pages = Pages
  { pageHtmlUrl    :: String
  , pageStatus     :: Maybe String
  , pageCustom404 :: Bool
  , pageCName :: Maybe String
  , pagePublic :: Bool
  } deriving (Show, Generic)

instance FromJSON Pages where
  parseJSON = withObject "Pages" $ \v ->
    Pages
      <$> v .: "html_url"
      <*> v .:? "status"
      <*> v .: "custom_404"
      <*> v .:? "cname"
      <*> v .: "public"

listToMap :: [Entry] -> Map String Entry
listToMap = Map.fromList . map (\item -> (entryPath item, item))

data Tree = Tree
  { treeSha :: String
  , treeUrl :: String
  , entries :: Map String Entry
  , truncated :: Bool
  } deriving (Show, Generic)

-- Instance to parse the GitHubTree from JSON, with "tree" as a Map
instance FromJSON Tree where
  parseJSON = withObject "Tree" $ \v -> Tree
      <$> v .: "sha"
      <*> v .: "url"
      <*> (listToMap <$> v .: "tree")
      <*> v .: "truncated"

data DirectoryOrFile = Directory [DirectoryOrFile] | File Text

-- Function to fetch from a URL, including a User-Agent header and a GitHub token
fetch :: (MonadIO m, MonadReader r m, HasConfiguration r) => String -> m LBS.ByteString
fetch url = do
    token <- asks (githubAccessToken . getConfiguration)
    initialRequest <- liftIO $ parseRequest ("https://api.github.com/" ++ url)
    let request =
          setRequestHeaders
          [ (hAuthorization, pack ("Bearer " ++ token))
          , (hAccept,       "application/vnd.github+json")
          , (hUserAgent, "ximera-tex-to-worksheet-app")
          , ("X-GitHub-Api-Version", "2022-11-28")
          ]
          initialRequest
    response <- liftIO $ httpLBS request
    return $ getResponseBody response

-- Function to fetch and decode JSON from a URL
fetchJsonWithCache :: (FromJSON a, MonadIO m, MonadReader r m, HasConfiguration r, MonadDB m) => String -> m (Either String a)
fetchJsonWithCache url = do
  let key :: BS.ByteString = pack ("github:" ++ url)
  cached <- rget key
  case cached of
    Right (Just x) -> return $ eitherDecode (LBS.fromStrict x)
    _ -> do
      body <- fetch url
      _ <- rset key (BS.toStrict body)
      _ <- expire key 60
      return $ eitherDecode body

getRepo :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> m (Either String Repository)
getRepo username reponame = do
  let url = "repos/" ++ username ++ "/" ++ reponame
  fetchJsonWithCache url

getPages :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> m (Either String Pages)
getPages username reponame = do
  let url = "repos/" ++ username ++ "/" ++ reponame ++ "/pages"
  fetchJsonWithCache url

getPageUrl :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> String -> m (Either String URI)
getPageUrl username reponame path = runExceptT $ do
  pages <- ExceptT $ getPages username reponame
  base <- ExceptT . pure $
            maybe (Left "Invalid base URI") Right (parseURI (pageHtmlUrl pages))
  rel  <- ExceptT . pure $
            maybe (Left "Invalid relative URI") Right (parseRelativeReference path)
  pure $ rel `relativeTo` base
  -- return $ uriToString id combined ""

data XimeraPage = RedirectTo URI | HTMLContent ByteString | PageMissing
    deriving (Show)

fetchUri
  :: ( MonadIO m )
  => URI
  -> m (Either String XimeraPage)
fetchUri uri = do
  let url = uriToString id uri ""
  initReq    <- liftIO $ parseRequest url
  let req     = setRequestHeaders
                  [ (hUserAgent, "ximera-tex-to-worksheet-app")
                  ]
                  initReq
  response   <- liftIO $ httpLBS req
  let hdrs   = responseHeaders response
      cType  = fromMaybe "" (lookup hContentType hdrs)
      code = statusCode (responseStatus response)
  if code /= 200
    then
      -- any non‑200 → MissingPage
      pure $ Right PageMissing
    else
      if "text/html" `BS.isPrefixOf` cType
        then do
          let result = responseBody response
          pure $ Right (HTMLContent result)
        else do
          pure $ Right (RedirectTo uri)

getPage :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> String -> m (Either String XimeraPage)
getPage username reponame path = runExceptT $ do
  url <- ExceptT $ getPageUrl username reponame path
  page <- ExceptT $ fetchUri url
  pure page

getCommit :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> String -> m (Either String Commit)
getCommit username reponame sha = runExceptT $ do
  let url = "repos/" ++ username ++ "/" ++ reponame ++ "/commits/" ++ sha
  ExceptT $ fetchJsonWithCache url

getRepoMain :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> m (Either String Commit)
getRepoMain username reponame = runExceptT $ do
  r <- ExceptT $ getRepo username reponame
  let branch = defaultBranch r
  ExceptT $ getCommit username reponame branch

getTree :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> m (Either String Tree)
getTree url = runExceptT $ do
  let httpsGithub = "https://api.github.com/"
  unless (httpsGithub `isPrefixOf` url) $
    throwError $ "tree URL must begin with " ++ httpsGithub
  ExceptT $ fetchJsonWithCache $ drop (length httpsGithub) url

getDirectoryOrFileFromTree :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => Tree -> [String] -> m (Either String (Maybe DirectoryOrFile))
getDirectoryOrFileFromTree tree [] = pure $ Left "getDirectoryOrFileFromTree requires nonempty path"
getDirectoryOrFileFromTree tree [x] = do
  case Map.lookup x (entries tree) of
    Nothing -> pure $ Right Nothing
    Just e -> if (entryType e) == "tree"
      then runExceptT $ do t <- ExceptT $ getTree $ (entryUrl e)
                           ExceptT $ getDirectoryOrFileFromTree t []
      else pure $ Right Nothing

getDirectoryOrFileFromTree tree (x:xs) =
  case Map.lookup x (entries tree) of
    Nothing -> pure $ Right Nothing
    Just e -> if (entryType e) == "tree"
      then runExceptT $ do t <- ExceptT $ getTree $ (entryUrl e)
                           ExceptT $ getDirectoryOrFileFromTree t xs
      else pure $ Right Nothing

getDirectoryOrFile :: (MonadDB m, MonadReader r m, HasConfiguration r, MonadIO m) => String -> String -> String -> [String] -> m (Either String DirectoryOrFile)
getDirectoryOrFile username reponame sha xs = runExceptT $ do
  c <- ExceptT $ getCommit username reponame sha
  t <- ExceptT $ getTree $ commitTreeUrl c
  mres <- ExceptT $ getDirectoryOrFileFromTree t xs
  case mres of
    Nothing   -> throwError "DirectoryOrFile not found"
    Just res  -> return res
