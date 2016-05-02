{-# LANGUAGE NoImplicitPrelude #-}

module Test.IO.Warden where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.Csv (EncodeOptions(..), defaultEncodeOptions, encodeWith)
import           Data.List (nub, zip)
import qualified Data.Text as T

import           Disorder.Core.IO (testIO)
import           Disorder.Corpus (muppets)

import           Lane.Data (dateAsPartition)

import           P

import           System.Directory (createDirectoryIfMissing, getDirectoryContents)
import           System.FilePath ((</>), takeDirectory)
import           System.IO
import           System.IO.Temp (withTempFile, withTempDirectory)
import           System.Posix.Directory (getWorkingDirectory)
import           System.Posix.Files (getSymbolicLinkStatus, isDirectory, isRegularFile)

import           Test.QuickCheck (Gen, Testable, Property, forAll, arbitrary)
import           Test.QuickCheck (suchThat, elements, generate, resize)
import           Test.QuickCheck.Gen (Gen(..))
import           Test.QuickCheck.Random
import           Test.Warden.Arbitrary

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either

withTestFile :: (FilePath -> Handle -> IO a) -> IO a
withTestFile a = do
  d <- getWorkingDirectory
  withTempFile d "warden-test-" (\f h -> a f h)

withTestViewFile :: (ViewFile -> IO a) -> IO a
withTestViewFile a = do
  d <- getWorkingDirectory
  vf <- generate . resize 2 $ arbitrary :: IO ViewFile
  withTempDirectory d "warden-viewfile-test-" $ \d' -> do
    let vd = (d' </>) . unView $ vfView vf
    let dd = T.unpack . dateAsPartition $ vfDate vf
    let fd = takeDirectory . T.unpack . unFilePart $ vfFilePart vf
    createDirectoryIfMissing True $ vd </> dd </> fd
    a $ vf { vfView = View vd }

withTestView :: (View -> IO a) -> IO a
withTestView a = do
  d <- getWorkingDirectory
  withTempDirectory d "warden-view-test-" (\f -> a (View f))

testWarden :: Testable a => EitherT WardenError (ResourceT IO) a -> Property
testWarden = testIO . unsafeWarden

unsafeWarden :: EitherT WardenError (ResourceT IO) a -> IO a
unsafeWarden tst = orDie =<< (runEitherT $ mapEitherT runResourceT tst)
  where
    orDie (Right a) = pure a
    orDie (Left e) = fail . T.unpack $ renderWardenError e

writeView :: FilePath -> DirTree -> IO ()
writeView r t =
  let dirs = fmap (r </>) $ directoryDirs t
      files = fmap (r </>) $ directoryFiles t in do
  mapM_ (createDirectoryIfMissing True) dirs
  mapM_ touch files
  where
    touch fp = writeFile fp ""

withValidDirTree :: Testable a => (View -> IO a) -> Property
withValidDirTree a = forAll (arbitrary :: Gen ValidDirTree) $ \(ValidDirTree dt) ->
  withDirTree dt a

withInvalidDirTree :: Testable a => (View -> IO a) -> Property
withInvalidDirTree a = forAll (arbitrary :: Gen InvalidDirTree) $ \(InvalidDirTree dt) ->
  withDirTree dt a

withDirTree :: Testable a => DirTree -> (View -> IO a) -> Property
withDirTree dt a = testIO . withTempDirectory "." "test-view" $ \tmp -> do
  let (DirTree v _ _) = dt
  let v'              = View $ tmp </> unDirName v
  writeView tmp dt
  a v'

newtype LineSize =
  LineSize {
    unLineSize :: Int
  } deriving (Eq, Show)

newtype RecordCount =
  RecordCount Int
  deriving (Eq, Show)

newtype GenSize =
  GenSize Int
  deriving (Eq, Show)

data GenType =
    NonDeterministic
  | Deterministic Int
  deriving (Eq, Show)

generate' :: GenType -> GenSize -> Gen a -> IO a
generate' NonDeterministic (GenSize size) g =
  generate . resize size $ g
generate' (Deterministic seed) (GenSize size) (MkGen g) =
  let r = mkQCGen seed in
  pure $ g r size

generateView :: GenType -> FilePath -> RecordCount -> GenSize -> LineSize -> IO View
generateView gt root (RecordCount n) s ll = do
  dt <- generate' gt s . fmap unValidDirTree $
          arbitrary `suchThat` ((> 0) . length . directoryFiles . unValidDirTree)
  tok <- generate' gt s $ elements muppets
  fieldCount <- generate' gt (GenSize $ unLineSize ll) $ arbitrary
  let viewRoot = root </> ("warden-gen-" <> T.unpack tok)
  let dfs = nub . fmap (viewRoot </>) $ directoryFiles dt
  let fileLines = n `div` (length dfs)
  writeView viewRoot dt
  void $ mapConcurrently (writeViewFile gt fileLines fieldCount ll) dfs
  pure . View $ viewRoot </> (viewBase dt)
  where
    viewBase (DirTree b _ _) = unDirName b

writeViewFile :: GenType -> Int -> FieldCount -> LineSize -> FilePath -> IO ()
writeViewFile gt c fc (LineSize ll) fp = do
  fh <- openFile fp WriteMode
  replicateM_ c (writeRow fh)
  hClose fh
  where
    sep = Separator . fromIntegral $ ord '|'

    writeRow h = do
      r <- generate' gt (GenSize ll) $ validSVRow sep fc
      BL.hPutStr h $ encodeWith (wardenEncodeOpts sep) [r]

wardenEncodeOpts :: Separator -> EncodeOptions
wardenEncodeOpts sep = defaultEncodeOptions {
    encDelimiter = unSeparator sep
  , encUseCrLf = False
  }

traverseTestDirectory :: DirName -> EitherT WardenError (ResourceT IO) [FilePath]
traverseTestDirectory dn =
  fmap directoryFiles $ traverseTestDirectory' (MaxDepth 10) [] dn

traverseTestDirectory' :: MaxDepth
                       -> [DirName]
                       -> DirName
                       -> EitherT WardenError (ResourceT IO) DirTree
traverseTestDirectory' (MaxDepth 0) _ _ = left $ WardenTraversalError MaxDepthExceeded
traverseTestDirectory' (MaxDepth depth) preds dn =
  let preds' = preds <> [dn] in do
  ls <- liftIO . getDirectoryContents $ joinDir preds'
  sts <- liftIO . mapM getSymbolicLinkStatus $ (joinFile preds') <$> ls
  let branches = fmap (DirName . fst) $
                   filter (uncurry visitable) $ zip ls sts
  let leaves = fmap (FileName . fst) $ filter (uncurry validLeaf) $ zip ls sts
  subtrees <- mapM (traverseTestDirectory' (MaxDepth $ depth - 1) preds') branches
  pure $ DirTree dn subtrees leaves
  where
    visitable ('.':_) _ = False
    visitable _ st      = isDirectory st

    validLeaf _ st      = isRegularFile st

