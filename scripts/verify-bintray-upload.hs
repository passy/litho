#!/usr/bin/env stack
-- stack --resolver lts-9.0 --install-ghc runghc --package turtle --package system-filepath --package pseudomacros --package megaparsec --package bifunctors

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (FilePath)
import Turtle

import Control.Arrow ((>>>))
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Text.Megaparsec as M
import Text.Megaparsec.Text as MT
import PseudoMacros (__FILE__)

import qualified Filesystem.Path.CurrentOS as Path
import qualified Control.Monad.Managed as Managed
import qualified Data.Text as T

-- * Global settings

data RemoteRepository = RemoteRepository
  { repoId :: Text
  , repoLayout :: Maybe Text
  , repoUrl :: Text }

remoteRepositoryToString :: RemoteRepository -> Text
remoteRepositoryToString RemoteRepository{..} =
  T.intercalate "::"
    [ repoId
    , fromMaybe "" repoLayout
    , repoUrl
    ]

remoteRepositories :: [RemoteRepository]
remoteRepositories =
  [ RemoteRepository "jcenter" Nothing "https://jcenter.bintray.com/"
  ]

-- * Application logic

parser :: Turtle.Parser Text
parser = argText "VERSION" "Version number to verify"

data MvnArtifact = MvnArtifact
  { mvnName :: Text
  , mvnPackaging :: Text
  } deriving Show

-- | Provide a path to the directory this very file resides in through some
-- arcane magic.
thisDirectory :: IO FilePath
thisDirectory = do
  let filePath :: FilePath = $__FILE__
  currentDir <- pwd
  return . Path.parent $ currentDir </> filePath

mkFakeMavenSettings
  :: Managed.MonadManaged m
  => m FilePath
mkFakeMavenSettings = do
  mavenTmp <- using (mktempdir "/tmp" "fbm2")
  output (mavenTmp </> "settings.xml") $
    "<settings>" <|> "</settings>"
  return mavenTmp

parseMvnArtifact :: Text -> Either Text MvnArtifact
parseMvnArtifact = M.parse (parser) "<input>" >>> first (T.pack . parseErrorPretty)
  where
    pomParser :: Text -> MT.Parser Text
    pomParser identifier = do
      _ <- M.string $ T.unpack identifier
      M.space
      _ <- M.char '='
      M.space
      name <- T.pack <$> M.some M.printChar
      _ <- M.eol
      return name

    nameParser = pomParser "POM_NAME"
    packagingParser = pomParser "POM_PACKAGING"

    parser :: MT.Parser MvnArtifact
    parser = do
      mvnName <- nameParser
      mvnPackaging <- (try packagingParser) <|> (skipMany any)
      return MvnArtifact{..}

mvnArtifactToVersionedIdentifier :: MvnArtifact -> Text -> Text
mvnArtifactToVersionedIdentifier MvnArtifact{..} version =
  format ("com.facebook.litho:%s:%s:%s") mvnName version mvnPackaging

buildMvnGetCommand :: MvnArtifact -> Text -> FilePath -> (T.Text, [T.Text])
buildMvnGetCommand artifact version configDir =
  ( "mvn"
  , [ "dependency:get"
    , "-gs"
    , format fp (configDir </> "settings.xml")
    , "-Dartifact=" <> mvnArtifactToIdentifier
    , "-DremoteRepositories=" <> T.intercalate "," (remoteRepositoryToString <$> remoteRepositories)
    , "-Dtransitive=true"]
  )

main :: IO ()
main = do
  version <- options "Bintray Upload Verifier" parser
  this <- thisDirectory
  let rootDir = this </> ".."

  whichMvn <- which "mvn"
  case whichMvn of
    Just _ -> return ()
    Nothing -> die "This tool requires `mvn` (Apache Maven) to be on your $PATH."

  sh $ do
    mavenTmp <- mkFakeMavenSettings
    gradleProperties <- find (suffix "/gradle.properties") rootDir
    line <- grep (has "POM_ARTIFACT_ID") (input gradleProperties)
    case parseMvnArtifact line of
      Left err -> printf ("Skipping unsupported mvn line '"%s%"' because of error "%s%".\n") (T.strip (format l line)) err
      Right mvnArtifact -> do
        printf ("Downloading Maven artifact for "%w%" ...\n") mvnArtifact
        let (cmd, args) = buildMvnGetCommand mvnArtifact version mavenTmp
        ret <- proc cmd args empty
        case ret of
          ExitSuccess -> return ()
          ExitFailure _ -> die $ format ("Couldn't download Maven artifact "%w%". Looks like something went screwy.") mvnArtifact

  echo "Done!"
