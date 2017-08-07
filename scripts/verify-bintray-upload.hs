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
import Data.Maybe (fromMaybe, catMaybes)
import Data.List.NonEmpty (fromList)
import PseudoMacros (__FILE__)

import qualified Filesystem.Path.CurrentOS as Path
import qualified Control.Monad.Managed as Managed
import qualified Text.Megaparsec.Text as MT
import qualified Text.Megaparsec as M
import qualified Data.Text as T
import qualified Control.Foldl as Fold

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
  , RemoteRepository "google" Nothing "https://maven.google.com/"
  ]

-- * Application logic

parser :: Turtle.Parser Text
parser = argText "VERSION" "Version number to verify"

data MvnArtifact = MvnArtifact
  { mvnArtifactId :: Text
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
parseMvnArtifact = M.parse (mvnParser <* M.eof) "<input>" >>> first (T.pack . M.parseErrorPretty)
  where
    pomParser :: MT.Parser (Text, Text)
    pomParser = do
      identifier <- T.strip . T.pack <$> M.someTill M.printChar (M.char '=')
      M.space
      value <- T.strip . T.pack <$> M.some M.printChar

      return (identifier, value)

    emptyLineParser :: forall a. MT.Parser (Maybe a)
    emptyLineParser = M.some M.spaceChar >> M.optional M.newline *> pure Nothing

    mvnParser :: MT.Parser MvnArtifact
    mvnParser = do
      pomItems <- M.many (((Just <$> pomParser) <* M.eol) <|> emptyLineParser)
      case reducePomTokens (catMaybes pomItems) of
        Just a -> return a
        Nothing -> M.unexpected (M.Label $ fromList "Missing POM identifiers.")

    reducePomTokens :: [(Text, Text)] -> Maybe MvnArtifact
    reducePomTokens ts = do
      mvnArtifactId <- lookup "POM_ARTIFACT_ID" ts
      mvnPackaging <- lookup "POM_PACKAGING" ts
      return MvnArtifact{..}

mvnArtifactToVersionedIdentifier :: MvnArtifact -> Text -> Text
mvnArtifactToVersionedIdentifier MvnArtifact{..} version =
  format ("com.facebook.litho:"%s%":"%s%":"%s) mvnArtifactId version mvnPackaging

buildMvnGetCommand :: MvnArtifact -> Text -> FilePath -> (T.Text, [T.Text])
buildMvnGetCommand artifact version configDir =
  ( "mvn"
  , [ "dependency:get"
    , "-gs"
    , format fp (configDir </> "settings.xml")
    , "-Dartifact=" <> (mvnArtifactToVersionedIdentifier artifact version)
    , "-DremoteRepositories=" <> T.intercalate "," (remoteRepositoryToString <$> remoteRepositories)
    -- Would be nice to also check transitive deps, but mvn get doesn't support resolving transitive AARs.
    , "-Dtransitive=false"]
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

  let prog = do
      mavenTmp <- mkFakeMavenSettings
      gradleProperties :: FilePath <- find (suffix "/gradle.properties") rootDir
      contents <- liftIO $ readTextFile gradleProperties
      case parseMvnArtifact contents of
        Left err' -> do
          printf ("Skipping unsupported file '"%fp%"' because of error "%s%".\n") gradleProperties err'
          return (gradleProperties, True)
        Right mvnArtifact -> do
          printf ("Downloading Maven artifact for "%w%" ...\n") mvnArtifact
          let (cmd, args) = buildMvnGetCommand mvnArtifact version mavenTmp
          printf ("Executing "%s%" "%w%" ...\n") cmd args
          ret <- proc cmd args empty
          case ret of
            ExitSuccess ->
              return (gradleProperties, True)
            ExitFailure code -> do
              printf ("Download of Maven artifact "%w%" failed with status code "%d%". Looks like something went screwy.\n") mvnArtifact code
              return (gradleProperties, False)

  fold prog (Fold.all $ (== True) . snd) >>= \case
    True -> do
      echo "All artifacts seem to have been uploaded. Sweet!"
      exit ExitSuccess
    False -> do
      err "ERROR: Some artifacts are missing from Bintray!"
      exit $ ExitFailure 1
