module Configuration where

data SHA = SHA { unSHA :: String }
  deriving (Show, Eq)

data Configuration = Configuration
  { githubRoot :: String,
    githubAccessToken :: String,
    getHostname :: String,
    jsBundleSHA :: SHA,
    cssMainSHA :: SHA
  }
  deriving (Show)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { githubRoot = "http://localhost:4000/github/",
      githubAccessToken = "",
      getHostname = "",
      jsBundleSHA = SHA "",
      cssMainSHA = SHA ""
    }

updateGithubRoot :: Maybe String -> Configuration -> Configuration
updateGithubRoot (Just s) config = config {githubRoot = s}
updateGithubRoot Nothing config = config

updateGithubAccessToken :: Maybe String -> Configuration -> Configuration
updateGithubAccessToken (Just s) config = config {githubAccessToken = s}
updateGithubAccessToken Nothing config = config

updateHostname :: Maybe String -> Configuration -> Configuration
updateHostname (Just s) config = config {getHostname = s}
updateHostname Nothing config = config
