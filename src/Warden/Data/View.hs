{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.View(
    DirTree(..)
  , DirName(..)
  , FileName(..)
  , MaxDepth(..)
  , NonViewFile(..)
  , View(..)
  , ViewFile(..)
  , directoryDirs
  , directoryFiles
  , isViewFile
  , joinDir
  , joinFile
  , removeViewPrefix
  , renderNonViewFile
  , renderView
  , renderViewFile
) where

import           Data.List (stripPrefix)
import           Data.String (IsString)
import qualified Data.Text as T
import           Data.Text (Text)

import           Lane.Data (datePartitionParser)

import           System.FilePath (joinPath, splitDirectories, (</>))
import           System.IO (FilePath)

import           P

import           X.Data.Attoparsec.Text (startsWith)

newtype View =
  View {
    unView :: FilePath
  } deriving (Eq, Show, Ord)

renderView :: View -> Text
renderView = T.pack . unView

newtype ViewFile =
  ViewFile {
    unViewFile :: FilePath
  } deriving (Eq, Show, Ord)

renderViewFile :: ViewFile -> Text
renderViewFile = T.pack . unViewFile

newtype NonViewFile =
  NonViewFile {
    unNonViewFile :: FilePath
  } deriving (Eq, Show, Ord)

renderNonViewFile :: NonViewFile -> Text
renderNonViewFile = T.pack . unNonViewFile

isViewFile :: View -> FilePath -> Bool
isViewFile v fp = maybe
  False
  (startsWith datePartitionParser)
  (T.pack <$> removeViewPrefix v fp)

removeViewPrefix :: View -> FilePath -> Maybe FilePath
removeViewPrefix (View v) fp =
  joinPath <$> stripPrefix v' fp'
  where
    v' = splitDirectories v

    fp' = splitDirectories fp

data DirTree = DirTree DirName [DirTree] [FileName]
  deriving (Eq, Show)

newtype DirName =
  DirName {
    unDirName :: FilePath
  } deriving (Eq, Show, Ord, IsString)

newtype FileName =
  FileName {
    unFileName :: FilePath
  } deriving (Eq, Show, Ord)

newtype MaxDepth =
  MaxDepth {
    unMaxDepth :: Int
  } deriving (Eq, Show)

joinDir :: [DirName] -> FilePath
joinDir = joinPath . fmap unDirName

joinFile :: [DirName] -> FilePath -> FilePath
joinFile ds f = (joinDir ds) </> f

-- | Penultimate nodes in the tree (the deepest non-leaf for each path).
directoryDirs :: DirTree -> [FilePath]
directoryDirs = go []
  where
    go preds (DirTree root [] _) =
      pure $ joinDir (reverse (root : preds))
    go preds (DirTree root branches _) =
      concatMap (go (root : preds)) branches

directoryFiles :: DirTree -> [FilePath]
directoryFiles = go []
  where
    go preds (DirTree root branches leaves) =
      let xs = (joinFile (reverse (root : preds)) . unFileName) <$> leaves
          ys = concatMap (go (root : preds)) branches in
      xs <> ys

