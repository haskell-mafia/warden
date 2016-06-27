{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_warden
import           DependencyInfo_ambiata_warden

import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Char (ord)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T
import qualified Data.Text as T

import           Options.Applicative

import           P

import           System.Exit (exitSuccess, exitFailure)
import           System.IO (IO, FilePath, print, putStrLn)

import           Warden.Commands
import           Warden.Data
import           Warden.Error
import           Warden.Param
import           Warden.Schema

import           X.Control.Monad.Trans.Either (mapEitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative

data Command =
    Check !View !CheckParams
  | SingleFileCheck !ViewFile !CheckParams
  | Infer !Verbosity !FieldMatchRatio !InferUsingFailedChecks !SchemaFile ![FilePath]
  | Sanity !View !SanityParams
  | SummarizeMarkers ![FilePath]
  | FailedMarkers ![FilePath]
  | SchemaValidate !SchemaFile
  deriving (Eq, Show)

main :: IO ()
main = do
  dispatch (safeCommand wardenP) >>= \case
    VersionCommand -> do
      putStrLn ("warden: " <> buildInfoVersion)
      exitSuccess
    DependencyCommand -> do
      mapM_ putStrLn dependencyInfo
    RunCommand DryRun c -> do
      print c
      exitSuccess
    RunCommand RealRun cmd -> do
      wps <- buildWardenParams . WardenVersion $ T.pack buildInfoVersion
      run wps cmd

run :: WardenParams -> Command -> IO ()
run wps (Check v ps) = do
  r <- orDie renderWardenError . mapEitherT runResourceT $ check wps v ps
  finishCheck (checkVerbosity ps) (checkExitType ps) r
run wps (SingleFileCheck vf ps) = do
  r <- orDie renderWardenError . mapEitherT runResourceT $ fileCheck wps vf ps
  finishCheck (checkVerbosity ps) (checkExitType ps) r
run _wps (Infer v fmr fc sf fs) = do
  s <- orDie renderWardenError . mapEitherT runResourceT $ infer v fmr fc fs
  T.writeFile (unSchemaFile sf) $ renderSchema s
run _wps (SummarizeMarkers fs) = do
  void . orDie renderWardenError . mapEitherT runResourceT $ summarizeMarkers fs
run _wps (FailedMarkers fs) = do
  ms <- orDie renderWardenError . mapEitherT runResourceT $ failedMarkers fs
  mapM_ putStrLn ms
run wps (Sanity v sps) = do
  r <- orDie renderWardenError . mapEitherT runResourceT $ sanity wps v sps
  finishCheck (sanityVerbosity sps) (sanityExitType sps) r
run _wps (SchemaValidate sf) = do
  void . orDie renderWardenError . mapEitherT runResourceT $ validateSchema sf

finishCheck :: Verbosity -> ExitType -> NonEmpty CheckResult -> IO ()
finishCheck verb xt rs = do
  when (verb == Verbose) $
    mapM_ T.putStrLn . NE.toList . (=<<) renderCheckResult $ rs
  if checkHasFailures rs && xt == ExitWithCheckStatus
    then exitFailure
    else exitSuccess

wardenP :: Parser Command
wardenP = subparser $
     command' "check" "Run checks over a view." checkP
  <> command' "check-file" "Run checks over a single file." fileCheckP
  <> command' "infer" "Attempt to infer a schema from a set of metadata files." inferP
  <> command' "marker" "Commands for dealing with marker files." markerCommandP
  <> command' "sanity" "Run pre-extract sanity checks over a view." sanityP
  <> command' "schema" "Commands for dealing with schema files." schemaCommandP

checkP :: Parser Command
checkP = Check <$> viewP <*> checkParamsP

fileCheckP :: Parser Command
fileCheckP = SingleFileCheck <$> viewFileP <*> checkParamsP

sanityP :: Parser Command
sanityP = Sanity <$> viewP <*> sanityParamsP

inferP :: Parser Command
inferP = Infer <$> verbosityP
               <*> fieldMatchRatioP
               <*> inferUsingFailedChecksP
               <*> outputSchemaP
               <*> some markerFileP

markerCommandP :: Parser Command
markerCommandP = subparser $
     command'
       "summarize"
       "Summarize view markers."
       (SummarizeMarkers <$> some markerFileP)

  <> command'
    "failed"
    "Given a list of view marker paths, output all those containing failed checks."
    (FailedMarkers <$> some markerFileP)

schemaCommandP :: Parser Command
schemaCommandP = subparser $
     command'
       "validate"
       "Validate a schema file."
       (SchemaValidate <$> schemaPathP)

checkParamsP :: Parser CheckParams
checkParamsP = CheckParams <$> separatorP
                           <*> schemaFileP
                           <*> lineBoundP
                           <*> verbosityP
                           <*> forceP
                           <*> textFreeformThresholdP
                           <*> exitTypeP
                           <*> includeDotFilesP
                           <*> samplingTypeP
                           <*> fileFormatP
                           <*> piiTypeP

sanityParamsP :: Parser SanityParams
sanityParamsP = SanityParams <$> verbosityP
                             <*> forceP
                             <*> exitTypeP
                             <*> includeDotFilesP

textFreeformThresholdP :: Parser TextFreeformThreshold
textFreeformThresholdP = TextFreeformThreshold <$> (option auto $
     long "text-freeform-threshold"
  <> metavar "FREEFORM-THRESHOLD"
  <> help "Number of unique text hashes to store before deciding a field is freeform (affects memory usage and check performance). Defaults to 50."
  <> value 50)

markerFileP :: Parser FilePath
markerFileP = strArgument $
     metavar "MARKER-FILE"
  <> help "Path to view marker file(s) from `warden check`."

viewP :: Parser View
viewP = View <$> (strArgument $
     metavar "VIEW"
  <> help "Path to local copy of view.")

viewFileP :: Parser ViewFile
viewFileP = argument (eitherReader viewFileR) $
      metavar "VIEW-FILE"
   <> help "Path to local view file."
  where
    viewFileR fp = case viewFile fp of
      Left _ -> Left $ "invalid view file: " <> fp
      Right vf -> Right vf

separatorP :: Parser Separator
separatorP = option (eitherReader separator) $
     long "separator"
  <> short 's'
  <> metavar "SEPARATOR"
  <> value (Separator . fromIntegral $ ord '|')
  <> help "Field separator for view (e.g., pipe or comma). Tab is '\\t'. Defaults to '|'."
  where
    separator x = maybeToRight ("Invalid separator " <> x) $ valid' x
    valid' ('\\':'t':[]) = Just $ Separator 0x09 -- convenient notation for tab
    valid' [x] = if (ord x >= 32 && ord x < 128) || (ord x == 9) -- printable or tab
      then Just . Separator . fromIntegral $ ord x
      else Nothing
    valid' _   = Nothing
      
lineBoundP :: Parser LineBound
lineBoundP = LineBound <$> (option auto $
     long "max-line-length"
  <> short 'b'
  <> metavar "LINE_LENGTH"
  <> value 65536
  <> help "Maximum line length. Defaults to 65536.")

verbosityP :: Parser Verbosity
verbosityP =
  flag Quiet Verbose $
       long "verbose"
    <> short 'v'
    <> help "Verbose output."

forceP :: Parser Force
forceP =
  flag NoForce Force $
       long "force"
    <> short 'f'
    <> help "Overwrite existing marker files."

schemaFileP :: Parser (Maybe SchemaFile)
schemaFileP = maybe Nothing (Just . SchemaFile) <$> (optional . strOption $
     long "schema"
  <> metavar "SCHEMA-FILE"
  <> help "JSON-format schema against which to validate the dataset.")

outputSchemaP :: Parser SchemaFile
outputSchemaP = SchemaFile <$> (strOption $
     long "output-schema"
  <> short 'o'
  <> metavar "SCHEMA-FILE"
  <> value "schema.json"
  <> help "File to which to write generated schema (defaults to \"schema.json\").")

fieldMatchRatioP :: Parser FieldMatchRatio
fieldMatchRatioP = FieldMatchRatio <$> (option auto $
     long "schema-match-ratio"
  <> short 'm'
  <> metavar "MATCH-RATIO"
  <> value 0.95
  <> help "Minimum threshold of compatible observations to accept a field-type candidate, as a ratio to total number of rows. Defaults to 0.95.")

exitTypeP :: Parser ExitType
exitTypeP =
  flag ExitWithCheckStatus ExitWithSuccess $
       long "exit-success"
    <> short 'e'
    <> help "Exit with success status if no errors occur, even if checks have failures."

includeDotFilesP :: Parser IncludeDotFiles
includeDotFilesP =
  flag NoIncludeDotFiles IncludeDotFiles $
       long "include-dot-files"
    <> short 'd'
    <> help "Don't ignore dotfiles when traversing view."

inferUsingFailedChecksP :: Parser InferUsingFailedChecks
inferUsingFailedChecksP =
  flag NoInferUsingFailedChecks InferUsingFailedChecks $
       long "include-failed-checks"
    <> short 'i'
    <> help "Allow use of metadata from failed checks (useful for updating obsolete schemas)."

samplingTypeP :: Parser SamplingType
samplingTypeP = maybe NoSampling (ReservoirSampling . ReservoirSize) <$> (optional . option auto $
     long "reservoir-sampling"
  <> short 'r'
  <> metavar "RESERVOIR-SIZE"
  <> help "Reservoir size for sampling, enables quantile computation. Defaults to disabled.")

fileFormatP :: Parser FileFormat
fileFormatP = (option (eitherReader fileFormat) $
     long "file-format"
  <> metavar "FILE-FORMAT"
  <> value DelimitedText
  <> help ("File format to parse, one of [" <> fmts <> "], see documentation for descriptions. Defaults to \"delimited-text\"."))
  where
    fileFormat s = case parseFileFormat (T.pack s) of
      Nothing -> Left $ "Invalid file format (valid file formats are [" <> fmts <> "]): " <> s
      Just f -> Right f

    fmts = T.unpack . T.intercalate ", " $ fmap renderFileFormat [minBound..maxBound]

piiTypeP :: Parser PIICheckType
piiTypeP = maybe (PIIChecks $ MaxPIIObservations 1000) (PIIChecks . MaxPIIObservations) <$> (optional . option auto $
     long "max-pii-observations"
  <> metavar "PII-OBSERVATIONS"
  <> help "Maximum number of instances of potential PII to store in memory before discarding specific details of each instance. Defaults to 1000.")

schemaPathP :: Parser SchemaFile
schemaPathP = fmap SchemaFile . strArgument $
     metavar "SCHEMA-FILE"
  <> help "Path to schema file to validate."
