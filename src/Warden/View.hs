{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.View(
    traverseView
  , traverseView'
  , traverseDirectory
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.List (zip, partition)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           P

import           System.Directory (getDirectoryContents)
import           System.IO (IO)
import           System.Posix.Files (getSymbolicLinkStatus, isRegularFile, isDirectory)

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT, left)

traverseView :: View -> EitherT WardenError (ResourceT IO) (NonEmpty ViewFile)
traverseView v = do
  (goods, bads) <- traverseView' v
  when (not $ null bads) $
    left . WardenTraversalError $ NonViewFiles bads
  when (null goods) $
    left $ WardenTraversalError EmptyView
  pure $ NE.fromList goods

traverseView' :: View -> EitherT WardenError (ResourceT IO) ([ViewFile], [NonViewFile])
traverseView' v = do
  fs <- directoryFiles <$> traverseDirectory (MaxDepth 5) [] (DirName $ unView v)
  pure . bimap (fmap ViewFile) (fmap NonViewFile) $ (partition (isViewFile v) fs)

-- | Traverse a directory tree to a maximum depth, ignoring hidden files.
traverseDirectory :: MaxDepth -> [DirName] -> DirName -> EitherT WardenError (ResourceT IO) DirTree
traverseDirectory (MaxDepth 0) _ _ = left $ WardenTraversalError MaxDepthExceeded
traverseDirectory (MaxDepth depth) preds dn =
  let preds' = preds <> [dn] in do
  ls <- liftIO . getDirectoryContents $ joinDir preds'
  sts <- liftIO . mapM getSymbolicLinkStatus $ (joinFile preds') <$> ls
  let branches = fmap (DirName . fst) $
                   filter (uncurry visitable) $ zip ls sts
  let leaves = fmap (FileName . fst) $ filter (uncurry validLeaf) $ zip ls sts
  subtrees <- mapM (traverseDirectory (MaxDepth $ depth - 1) preds') branches
  pure $ DirTree dn subtrees leaves
  where
    visitable ('.':_) _ = False
    visitable ('_':_) _ = False
    visitable _ st      = isDirectory st

    validLeaf ('.':_) _ = False
    validLeaf ('_':_) _ = False
    validLeaf _ st      = isRegularFile st
