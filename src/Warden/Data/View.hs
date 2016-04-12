{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Warden.Data.View(
    DirTree(..)
  , DirName(..)
  , FileName(..)
  , FilePart(..)
  , MaxDepth(..)
  , NonViewFile(..)
  , View(..)
  , ViewFile(..)
  , directoryDirs
  , directoryFiles
  , joinDir
  , joinFile
  , removeViewPrefix
  , renderNonViewFile
  , renderView
  , renderViewFile
  , viewFile
  , viewFilePath
) where

import           Data.Attoparsec.Text (Parser, IResult(..))
import           Data.Attoparsec.Text (parse, anyChar, char)
import           Data.List (stripPrefix)
import           Data.String (IsString)
import qualified Data.Text as T

import           Delorean.Local.Date (Date)

import           GHC.Generics (Generic)

import           Lane.Data (datePartitionParser, dateAsPartition)

import           System.FilePath (joinPath, splitDirectories, (</>))
import           System.IO (FilePath)

import           P

newtype View =
  View {
    unView :: FilePath
  } deriving (Eq, Show, Ord, Generic)

instance NFData View

renderView :: View -> Text
renderView = T.pack . unView

newtype FilePart =
  FilePart {
    unFilePart :: Text
  } deriving (Eq, Show, Ord, Generic)

instance NFData FilePart

data ViewFile =
  ViewFile {
      vfView :: !View
    , vfDate :: !Date
    , vfFilePart :: !FilePart
  } deriving (Eq, Show, Ord, Generic)

instance NFData ViewFile

viewFilePath :: ViewFile -> FilePath
viewFilePath (ViewFile (View v) d (FilePart f)) =
  v </> T.unpack (dateAsPartition d) </> T.unpack f

renderViewFile :: ViewFile -> Text
renderViewFile = T.pack . viewFilePath

manyAnd' :: Parser a -> Parser b -> Parser ([a], b)
manyAnd' p end = go
  where
    go = (end >>= (pure . ((,) []))) `mplus` liftM2' f p go

    f x (xs, y) = (x : xs, y)
#ifndef NOINLINE
{-# INLINE manyAnd' #-}
#endif

viewFile :: FilePath -> Either FilePath ViewFile
viewFile fp =
  let vf = do (v, dPart, fPart) <- split' $ T.pack fp
              pure $ ViewFile v dPart fPart
  in case vf of
    Nothing -> Left fp
    Just vf' -> Right vf'
  where
    -- Split into components: view, date partition, filename.
    split' f = finalize $ parse viewDateP f

    finalize (Partial c)    = finalize $ c ""
    finalize (Done rest (v, dc)) = if T.null rest
                                     then Nothing
                                     else Just (v, dc, FilePart rest)
    finalize _              = Nothing

    -- This parser is extremely slow, don't use it where that matters.
    viewDateP = do
      (v, dp) <- fmap (first View) $ manyAnd' anyChar (char '/' *> datePartitionParser <* char '/')
      pure (v, dp)

newtype NonViewFile =
  NonViewFile {
    unNonViewFile :: FilePath
  } deriving (Eq, Show, Ord)

renderNonViewFile :: NonViewFile -> Text
renderNonViewFile = T.pack . unNonViewFile

removeViewPrefix :: View -> FilePath -> Maybe FilePath
removeViewPrefix (View v) fp =
  joinPath <$> stripPrefix v' fp'
  where
    v' = splitDirectories v

    fp' = splitDirectories fp

data DirTree = DirTree !DirName ![DirTree] ![FileName]
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
    go !preds !(DirTree !root [] _) =
      pure $ joinDir (reverse (root : preds))
    go !preds !(DirTree root branches _) =
      concatMap (go (root : preds)) branches

directoryFiles :: DirTree -> [FilePath]
directoryFiles = go []
  where
    go !preds !(DirTree root branches leaves) =
      let xs = (joinFile (reverse (root : preds)) . unFileName) <$> leaves
          ys = concatMap (go (root : preds)) branches in
      xs <> ys
