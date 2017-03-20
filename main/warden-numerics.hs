{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_warden
import           DependencyInfo_ambiata_warden

import qualified Data.Vector.Unboxed as VU

import           Options.Applicative (Parser)
import           Options.Applicative (subparser, flag', long)
import           Options.Applicative (option, auto, value)

import           P

import           System.IO (IO, BufferMode(..))
import           System.IO (stdout, stderr, hSetBuffering)
import           System.IO (hPutStrLn)
import qualified System.Random.MWC as R

import           Test.Numeric.Warden.Gen
import           Test.Numeric.Warden.Simulate

import           X.Options.Applicative (cli, command')

data Command =
    Simulate !Computation !TestRange
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "warden-numerics" buildInfoVersion dependencyInfo commandP $ \cmd ->
    case cmd of
      Simulate _c (TestRange l u) -> do
        g <- R.createSystemRandom
        xv <- genUniformWithin 1000 l u g
        VU.mapM_ (hPutStrLn stderr . show) xv

commandP :: Parser Command
commandP = subparser $
     command' "simulate" "Run simulations of numerical computations for accuracy evaluation." simulateP

simulateP :: Parser Command
simulateP =
  Simulate
    <$> computationP
    <*> testRangeP

testRangeP :: Parser TestRange
testRangeP =
  fmap (uncurry TestRange) $
    option auto $
         long "test-range"
      <> value (0.0, 1.0)

computationP :: Parser Computation
computationP =
  let
    meanP = flag' DataMean $ long "mean"
  in
  meanP
  
