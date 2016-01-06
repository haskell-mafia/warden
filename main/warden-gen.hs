{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.Csv (EncodeOptions(..), defaultEncodeOptions, encodeWith)
import           Data.List (nub)
import qualified Data.Text as T

import           Disorder.Corpus (muppets)

import           P

import           System.Exit
import           System.FilePath ((</>))
import           System.IO (IOMode(..), IO, FilePath, print, hClose, openFile)

import           Test.IO.Warden
import           Test.QuickCheck (generate, arbitrary, suchThat, elements, resize)
import           Test.Warden.Arbitrary

import           Warden.Data

import           X.Options.Applicative

newtype RecordCount =
  RecordCount Int
  deriving (Eq, Show)

newtype GenSize =
  GenSize Int
  deriving (Eq, Show)

data Command = Generate RecordCount GenSize
  deriving (Eq, Show)

main :: IO ()
main = do
  dispatch (safeCommand wardenGenP) >>= \case
    VersionCommand -> do
      exitSuccess
    RunCommand DryRun c -> do
      print c
      exitSuccess
    RunCommand RealRun (Generate c s) -> do
      generateView c s

generateView :: RecordCount -> GenSize -> IO ()
generateView (RecordCount n) (GenSize s) = do
  dt <- generate . resize s . fmap unValidDirTree $
          arbitrary `suchThat` ((> 0) . length . directoryFiles . unValidDirTree)
  tok <- generate $ elements muppets
  fieldCount <- generate arbitrary
  let viewRoot = "./warden-gen-" <> T.unpack tok
  let dfs = nub . fmap (viewRoot </>) $ directoryFiles dt
  let fileLines = n `div` (length dfs)
  writeView viewRoot dt
  mapM_ (writeViewFile fileLines fieldCount) dfs

writeViewFile :: Int -> FieldCount -> FilePath -> IO ()
writeViewFile c fc fp = do
  fh <- openFile fp WriteMode
  replicateM_ c (writeRow fh)
  hClose fh
  where
    sep = Separator . fromIntegral $ ord '|'

    writeRow h = do
      r <- generate $ validSVRow sep fc
      BL.hPutStr h $ encodeWith opts [r]

    opts = defaultEncodeOptions { encDelimiter = unSeparator sep }

wardenGenP :: Parser Command
wardenGenP = subparser $
  command' "gen" "Generate a view for testing/benchmarking." generateP

generateP :: Parser Command
generateP = Generate <$> recordCountP <*> genSizeP

recordCountP :: Parser RecordCount
recordCountP = RecordCount <$> (option auto $
     long "record-count"
  <> short 'c'
  <> metavar "COUNT"
  <> value 1000000
  <> help "Number of records to generate (default 10^6).")

genSizeP :: Parser GenSize
genSizeP = GenSize <$> (option auto $
     long "gen-size"
  <> short 's'
  <> metavar "SIZE"
  <> value 4
  <> help "Generator size parameter, default 4.")

