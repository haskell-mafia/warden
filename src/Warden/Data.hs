{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Warden.Data (
    Row(..)
  , WardenStatus(..)
  , CheckResult(..)
  , SVParseState(..)
  , ParsedField(..)
  , FieldLooks(..)
  , RowSchema(..)
  , WardenCheck(..)
  , renderParsedField
  , field
  , countFields
  , totalRows
  , badRows
  , numFields
  , fieldCounts

  , module Warden.Data.Numeric
  ) where

import           Control.Lens
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           P
import           Pipes
import qualified Pipes.Prelude              as PP

import Warden.Data.Numeric

-- | Raw record. Can be extended to support JSON objects as well as xSV if
--   needed.
data Row = SVFields (Vector Text)
           | RowFailure Text
           | SVEOF
  deriving (Eq, Show)

data WardenStatus = Green
                    -- ^ No issues detected.
                  | Yellow
                    -- ^ Some values are concerning and should be investigated
                    --   by a human.
                  | Red
                    -- ^ At least one check failed, processing should not
                    --   proceed without human intervention.
                  | Unknown
                    -- ^ We broke, or weren't given enough data.
  deriving (Eq, Show, Ord)

-- FIXME(sharif): this could use more structure once we have a better idea of
--                what failures look like - row(s) affected, et cetera
data CheckResult = CheckResult WardenStatus Text

data RowSchema a = RowSchema
  { fromRow :: Row -> Maybe a }

data WardenCheck a b = WardenCheck
  { initial  :: a
  , update   :: a -> RowSchema b -> a
  , finalize :: a -> CheckResult
  }

-- | We try parsing a field as each of these in order until we find one that
--   works.
data FieldLooks = LooksEmpty
                | LooksIntegral
                | LooksReal
                | LooksText
                | LooksBroken   -- ^ Not valid UTF-8.
  deriving (Eq, Show, Ord)

-- | We keep track of the number of unique values we get in each field; if
--   it's greater than a certain percentage of the total number of records
--   it's probably freeform, otherwise it's probably categorical. We
--   make the distinction here so we don't have to drag around every
--   freeform record in memory.
data TextCount = TextCount     (Map Text Integer)
               | LooksFreeform
  deriving (Eq, Show)

data SVParseState = SVParseState
  { _badRows     :: Integer
  , _totalRows   :: Integer
  , _numFields   :: [Int]
  , _fieldCounts :: Maybe (Vector (Map FieldLooks Integer, TextCount))
  } deriving (Eq, Show)

makeLenses ''SVParseState

data ParsedField = IntegralField Integer
                 | RealField Double
                 | TextField Text
  deriving (Eq, Show)

renderParsedField :: ParsedField
                  -> Text
renderParsedField (IntegralField i) = T.pack $ show i
renderParsedField (RealField d)     = T.pack $ show d
renderParsedField (TextField t)     = t

initialSVParseState :: SVParseState
initialSVParseState = SVParseState 0 0 [] Nothing

-- | Accumulator for field/row counts on tokenized raw data.
updateSVParseState :: SVParseState
                   -> Row
                   -> SVParseState
updateSVParseState st row =
  let good = countGood row
      bad  = countBad row  in
    (totalRows %~ ((good + bad) +))
  . (badRows %~ (bad +))
  . (numFields %~ (updateNumFields row))
  . (fieldCounts %~ (updateFieldCounts row))
  $ st
 where
  countGood (SVFields _)   = 1
  countGood (RowFailure _) = 0
  countGood SVEOF          = 0

  countBad (SVFields _)    = 0
  countBad (RowFailure _)  = 1
  countBad SVEOF           = 0

  updateNumFields (SVFields v) ns
    | not (elem (V.length v) ns) = (V.length v) : ns
    | otherwise                  = ns
  updateNumFields _ ns           = ns

  updateFieldCounts (SVFields v) Nothing   = Just $ V.zipWith updateFieldCount v $
    V.replicate (V.length v) (M.empty, TextCount M.empty)
  updateFieldCounts (SVFields v) (Just fc) = Just $ V.zipWith updateFieldCount v fc
  updateFieldCounts _ fc                   = fc

  updateFieldCount t fc = bimap (updateFieldLooks t) (updateTextCount t) fc

field :: Parser ParsedField
field = choice
  [ IntegralField <$> signed decimal <* endOfInput
  , RealField     <$> double <* endOfInput
  , TextField     <$> takeText
  ]

updateFieldLooks :: Text
                 -> Map FieldLooks Integer
                 -> Map FieldLooks Integer
updateFieldLooks "" m = M.insertWith (+) LooksEmpty 1 m
updateFieldLooks t m  =
  let looksLike = case parseOnly field t of
                    Left _                  -> LooksBroken   -- Not valid UTF-8.
                    Right (IntegralField _) -> LooksIntegral
                    Right (RealField _)     -> LooksReal
                    Right (TextField _)     -> LooksText
  in M.insertWith (+) looksLike 1 m

updateTextCount :: Text
                -> TextCount
                -> TextCount
updateTextCount _ LooksFreeform = LooksFreeform
updateTextCount t (TextCount tc) = case M.lookup t tc of
  Nothing -> TextCount $ M.insert t 1 tc
  Just n  -> if n > freeformTextThreshold
               then LooksFreeform
               else TextCount $ M.insert t (n+1) tc

-- | Number of occurrences of a value before we conclude that it's
--   not likely to be an enumerated field. May need to make this
--   tunable per-dataset at some point.
freeformTextThreshold :: Integer
freeformTextThreshold = 100

countFields :: (Monad m)
            => Producer Row m ()
            -> m SVParseState
countFields = PP.fold updateSVParseState initialSVParseState id
