{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Warden.Data (
    Row(..)
  , WardenStatus(..)
  , CheckResult(..)
  , SVParseState(..)
  , Minimum(..)
  , Maximum(..)
  , Mean(..)
  , Median(..)
  , Variance(..)
  , NumericSummary(..)
  , RowSchema(..)
  , WardenCheck(..)
  , countFields
  , totalRecords
  , badRecords
  , numFields
  , fieldCounts
  ) where

import           P

import           Control.Lens
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Pipes
import qualified Pipes.Prelude              as PP

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

newtype Minimum = Minimum { getMininum :: Double }
  deriving (Eq, Show)

newtype Maximum = Maximum { getMaximum :: Double }
  deriving (Eq, Show)

newtype Mean = Mean { getMean :: Double }
  deriving (Eq, Show)

newtype Median = Median { getMedian :: Double }
  deriving (Eq, Show)

newtype Variance = Variance { getVariance :: Double }
  deriving (Eq, Show)

-- | So we can cheaply keep track of long-term change in numeric datasets.
--   Will probably also end up in brandix.

-- NB(sharif): I'm not sure if the median is worth calculating as it
--             can't be done cheaply, but it's handy for tests like
--             S-H-ESD; should rethink once we know more about which
--             tests actually work.
data NumericSummary = NumericSummary
  { _min    :: Minimum
  , _max    :: Maximum
  , _mean   :: Mean
  , _var    :: Maybe Variance
  , _median :: Maybe Median
  }
  deriving (Eq, Show)

-- | We try parsing a field as each of these in order until we find one that
--   works.
data FieldLooks = LooksEmpty
                | LooksIntegral
                | LooksReal
                | LooksText
                | LooksBroken -- ^ Can't happen.
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
  { _badRecords   :: Integer
  , _totalRecords :: Integer
  , _numFields    :: [Int]
  , _fieldCounts  :: Maybe (Vector (Map FieldLooks Integer, TextCount))
  } deriving (Eq, Show)

makeLenses ''SVParseState

data Field = IntegralField Integer
           | RealField Double
           | TextField Text

initialSVParseState :: SVParseState
initialSVParseState = SVParseState 0 0 [] Nothing

updateSVParseState :: SVParseState
                   -> Row
                   -> SVParseState
updateSVParseState st row =
  let good = countGood row
      bad  = countBad row  in
    (totalRecords %~ ((good + bad) +))
  . (badRecords %~ (bad +))
  . (numFields %~ (updateNumFields row))
  . (fieldCounts %~ (updateFieldCounts row))
  $ st
 where
  countGood (SVFields _)  = 1
  countGood _             = 0

  countBad (RowFailure _) = 1
  countBad _              = 0

  updateNumFields (SVFields v) ns
    | elem (V.length v) ns == False = (V.length v) : ns
    | otherwise                     = ns
  updateNumFields _ ns              = ns

  updateFieldCounts (SVFields v) Nothing   = Just $ V.zipWith updateFieldCount v $
    V.replicate (V.length v) (M.empty, TextCount M.empty)
  updateFieldCounts (SVFields v) (Just fc) = Just $ V.zipWith updateFieldCount v fc
  updateFieldCounts _ fc                   = fc

  updateFieldCount t fc = bimap (updateFieldLooks t) (updateTextCount t) fc

field :: Parser Field
field = choice
  [ IntegralField <$> decimal
  , RealField     <$> double
  , TextField     <$> takeWhile1 (\_ -> True)
  ]

increment :: (Ord a, Integral b)
          => a
          -> Map a b
          -> Map a b
increment k m = case M.lookup k m of
  Nothing -> M.insert k (fromIntegral one) m
  Just n  -> M.insert k ((fromIntegral one) + n) m
 where
  one :: Integer
  one = 1

updateFieldLooks :: Text
                 -> Map FieldLooks Integer
                 -> Map FieldLooks Integer
updateFieldLooks "" m = increment LooksEmpty m
updateFieldLooks t m  = case (parseOnly (field <* endOfInput) t) of
  Left _                  -> increment LooksBroken m -- can't happen
  Right (IntegralField _) -> increment LooksIntegral m
  Right (RealField _)     -> increment LooksReal m
  Right (TextField _)     -> increment LooksText m

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
