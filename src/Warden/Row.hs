{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Row (
    readView
  , readViewFile
  ) where


import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.ByteString (ByteString)
import           Data.Conduit (Source, Conduit, (=$=), await, yield)
import           Data.Csv ()
import           Data.Csv (DecodeOptions(..), HasHeader(..))
import           Data.Csv (defaultDecodeOptions)
import           Data.Csv.Incremental (Parser(..), decodeWith)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)

import           Data.String (String)

import           P

import           System.IO

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT, left)
import           X.Data.Conduit.Binary (slurp)

readView :: Separator
         -> LineBound
         -> NonEmpty ViewFile
         -> Source (EitherT WardenError (ResourceT IO)) Row
readView sep lb vfs =
  sequence_ $ (readViewFile sep lb) <$> vfs

-- FIXME: actually enforce LineBound
readViewFile :: Separator
             -> LineBound
             -> ViewFile
             -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile (Separator sep) (LineBound _lb) (ViewFile fp) =
  slurp fp 0 Nothing =$= decodeRows (decodeWith opts NoHeader)
  where
    opts = defaultDecodeOptions { decDelimiter = sep }

decodeRows :: Parser (Vector Text) -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRows (Fail _ e) =
  lift . left . WardenLoadError . RowDecodeFailed $ T.pack e
decodeRows (Many rs cont) = do
  mapM_ yield $ toRow <$> rs
  more <- await
  maybe (pure ()) (decodeRows . cont) more
decodeRows (Done rs) =
  mapM_ yield $ toRow <$> rs

toRow :: Either String (Vector Text) -> Row
toRow (Right !r) = SVFields r
toRow (Left !e)  = RowFailure $ T.pack e
{-# INLINE toRow #-}
