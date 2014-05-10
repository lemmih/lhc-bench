{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Ord
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import           Control.Exception
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.Digest.Pure.SHA
import           Data.Time
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO                  (IOMode (..), hClose, hFileSize,
                                             openFile)
import           System.Process

import           Html

type WallTime = Double
type Size = Integer

data BuildResults
    = BuildFailed String
    | BuildSuccess Size [WallTime]
    | BuildTimeout
    | BuildMissing
    deriving ( Show, Read )

buildResultsWallTime :: BuildResults -> [WallTime]
buildResultsWallTime (BuildSuccess _ timings) = take 10 timings
buildResultsWallTime _ = []

data RunResults
    = RunSuccess [RuntimeStats]
    | CompileError String
    | RuntimeError String
    | DiffError
    | RunDisabled
    | RunIncomplete
    deriving ( Read, Show )

runResultsStats :: RunResults -> [RuntimeStats]
runResultsStats (RunSuccess stats) = take 10 stats
runResultsStats _ = []

data RuntimeStats = RuntimeStats
    { runtimeWalltime        :: WallTime
    , runtimeContextSwitches :: Integer
    , runtimeCpuMigrations   :: Integer
    , runtimePageFaults      :: Integer
    , runtimeCycles          :: Integer
    , runtimeStalledCycles   :: Integer
    , runtimeInstructions    :: Integer
    , runtimeBranches        :: Integer
    , runtimeBranchMisses    :: Integer
    } deriving ( Show, Read )

data Flavor = GHC | JHC | AJHC | UHC | LHC
    deriving ( Show, Eq )

flavorBuildArgs :: Flavor -> FilePath -> FilePath -> [String]
flavorBuildArgs flavor src dst =
    case flavor of
        GHC ->
            [ src, "-o", dst, "-hidir", dstDirectory
            , "-odir", dstDirectory, "-fforce-recomp" ]
        AJHC ->
            [ src, "-o", dst ]
        UHC ->
            [ src, "-o", dst ]
  where
    dstDirectory = takeDirectory dst

flavorRunArgs :: Flavor -> [String]
flavorRunArgs GHC = ["+RTS","-s","-RTS"]
flavorRunArgs AJHC = []
flavorRunArgs UHC = []

configurationVersion :: Configuration -> IO (Either String String)
configurationVersion config = do
    mbExec <- findExecutable cmd
    case mbExec of
        Nothing -> return $ Left $ "Missing program: " ++ cmd
        Just path -> do
            (code, stdout, stderr) <- case configurationFlavor config of
                GHC -> readProcessWithExitCode path ghcOptions ""
                AJHC -> readProcessWithExitCode path ajhcOptions ""
                UHC -> readProcessWithExitCode path uhcOptions ""
            case code of
                ExitSuccess   -> return $ Right stdout
                ExitFailure{} -> return $ Left stderr
  where
    ghcOptions = ["--version"]
    ajhcOptions = ["--version"]
    uhcOptions = ["--version"]
    cmd = configurationCompiler config

data Instance = Instance
    { instanceBenchmark       :: Benchmark
    , instanceConfiguration   :: Configuration
    , instanceCompilerVersion :: String
    } deriving ( Show )

data Configuration = Configuration
    { configurationFlavor       :: Flavor
    , configurationLabel        :: String
    , configurationCompiler     :: String
    , configurationBuildOptions :: [String]
    , configurationRunOptions   :: [String]
    } deriving ( Show )

data Benchmark = Benchmark
    { benchmarkRoot     :: FilePath
    , benchmarkCategory :: String
    , benchmarkName     :: String
    , benchmarkExtension:: String
    , benchmarkStdout   :: Bool
    , benchmarkStdin    :: Bool
    , benchmarkDisabled :: Bool
    , benchmarkArgs     :: [String]
    , benchmarkHash     :: Integer
    } deriving ( Show, Eq )

readBuildResults :: Instance -> IO BuildResults
readBuildResults inst = handle err $ do
    ret <- readFile =<< instanceBuildResultsFile inst
    return $! read ret
  where
    err :: SomeException -> IO BuildResults
    err _ = return BuildMissing

-- XXX: Make the write atomic using renameFile
writeBuildResults :: Instance -> BuildResults -> IO ()
writeBuildResults inst results = do
    file <- instanceBuildResultsFile inst
    createDirectoryIfMissing True (takeDirectory file)
    writeFile file (show results)

readRunResults :: Instance -> IO RunResults
readRunResults inst = handle err $ do
    results <- readFile =<< instanceRunResultsFile inst
    return $! read results
  where
    err :: SomeException -> IO RunResults
    err _ = return RunIncomplete

writeRunResults :: Instance -> RunResults -> IO ()
writeRunResults inst results = do
    file <- instanceRunResultsFile inst
    createDirectoryIfMissing True (takeDirectory file)
    writeFile file (show results)

benchmarkSource :: Benchmark -> FilePath
benchmarkSource Benchmark{..} =
    benchmarkRoot </> benchmarkCategory </> benchmarkName <.>
    benchmarkExtension

findBenchmarks :: FilePath -> IO [Benchmark]
findBenchmarks root = do
    dirs <- getDirectoryContents root
    fmap concat $ forM (filter isCategory dirs) $ \dir -> do
        files <- getDirectoryContents (root </> dir)
        let isBenchmark file = takeExtension file `elem` [".hs",".lhs"]
            benchFiles = filter isBenchmark files
        mapM (mkBench files root dir) benchFiles
  where
    isCategory "." = False
    isCategory ".." = False
    isCategory _ = True

hashFiles :: [FilePath] -> IO Integer
hashFiles = worker []
  where
    worker acc [] = return $ integerDigest (sha1 $ Lazy.concat acc)
    worker acc (file:files) = do
        exist <- doesFileExist file
        if exist
            then do inp <- Lazy.readFile file
                    worker (inp : acc) files
            else worker acc files

mkBench :: [FilePath] -> FilePath -> FilePath
            -> FilePath -> IO Benchmark
mkBench allFiles root category file = do
    args <- if haveArgs
        then fmap read $ readFile (root </> category </> name <.> "args")
        else return []
    hash <- hashFiles
                [ root </> category </> name <.> extension
                , root </> category </> name <.> "stdin"
                , root </> category </> name <.> "stdout" ]
    return Benchmark
        { benchmarkRoot     = root
        , benchmarkCategory = category
        , benchmarkName     = name
        , benchmarkExtension = extension
        , benchmarkStdout   = haveStdout
        , benchmarkStdin    = haveStdin
        , benchmarkArgs     = args
        , benchmarkDisabled = isDisabled
        , benchmarkHash     = hash
        }
  where
    name = takeBaseName file
    extension = takeExtension file
    isDisabled = name <.> "disabled" `elem` allFiles
    haveArgs   = name <.> "args" `elem` allFiles
    haveStdout = name <.> "stdout" `elem` allFiles
    haveStdin  = name <.> "stdin" `elem` allFiles


instanceName :: Instance -> String
instanceName inst =
    benchmarkCategory benchmark ++ "/" ++ benchmarkName benchmark ++
    " " ++
    show flavor ++ "/" ++
    configurationLabel config
  where
    config = instanceConfiguration inst
    benchmark = instanceBenchmark inst
    flavor = configurationFlavor config

    


buildInstanceIdentifier :: Instance -> String
buildInstanceIdentifier inst =
    showDigest (sha1 $ Lazy.pack $ show $ clearBuildInvariantData inst)

runInstanceIdentifier :: Instance -> String
runInstanceIdentifier inst =
    showDigest (sha1 $ Lazy.pack $ show $ clearRunInvariantData inst)


clearBuildInvariantData :: Instance -> Instance
clearBuildInvariantData inst = inst
    { instanceBenchmark = clearBenchmark (instanceBenchmark inst)
    , instanceConfiguration = clearConfig (instanceConfiguration inst) }
  where
    clearBenchmark bench = bench{ benchmarkArgs = [] }
    clearConfig config = config
        { configurationLabel = ""
        , configurationRunOptions = [] }

clearRunInvariantData :: Instance -> Instance
clearRunInvariantData inst = inst
    { instanceBenchmark = clearBenchmark (instanceBenchmark inst)
    , instanceConfiguration = clearConfig (instanceConfiguration inst) }
  where
    clearBenchmark bench = bench
    clearConfig config = config
        { configurationLabel = "" }


instantiate :: Configuration -> [Benchmark] -> IO [Instance]
instantiate configuration benchmarks = do
    mbVersion <- configurationVersion configuration
    case mbVersion of
        Left errMsg -> do
            putStrLn $ "Skipping configuration: " ++ errMsg
            return []
        Right version ->
            forM benchmarks $ \benchmark ->
                return Instance
                    { instanceBenchmark = benchmark
                    , instanceConfiguration = configuration
                    , instanceCompilerVersion = version
                    }

-- [app dir]/builds/id/benchmark
data InstanceType = BuildInstance | RunInstance
instanceFile :: InstanceType -> String -> Instance -> IO FilePath
instanceFile ty name inst = do
    appDir <- getAppUserDataDirectory "lhc-bench"
    return $ appDir </> root </> ident </> name
  where
    root = case ty of
        BuildInstance -> "builds"
        RunInstance -> "runs"
    ident = case ty of
        BuildInstance -> buildInstanceIdentifier inst
        RunInstance   -> runInstanceIdentifier inst

instanceProgramFile :: Instance -> IO FilePath
instanceProgramFile = instanceFile BuildInstance "benchmark"

instanceBuildResultsFile :: Instance -> IO FilePath
instanceBuildResultsFile = instanceFile BuildInstance "results"

instanceRunResultsFile :: Instance -> IO FilePath
instanceRunResultsFile = instanceFile RunInstance "results"

getFileSize :: FilePath -> IO Integer
getFileSize path = bracket (openFile path ReadMode) hClose hFileSize

hasBuiltInstance :: Instance -> IO Bool
hasBuiltInstance inst = do
    results <- readBuildResults inst
    case results of
        BuildMissing -> return False
        _            -> return True

-- in seconds
buildTimeout :: Int
buildTimeout = 60 * 5

buildInstance :: Instance -> IO ()
buildInstance inst = unless (benchmarkDisabled $ instanceBenchmark inst) $ do
    dst <- instanceProgramFile inst
    createDirectoryIfMissing True (takeDirectory dst)
    let args = concat
                [ flavorBuildArgs flavor src dst
                , configurationBuildOptions config ]
    --putStrLn $ "Running: " ++ showCommandForUser cmd args
    putStrLn $ "[" ++ ident ++ "] Building"
    let whenTimeout = do
            putStrLn $ "[" ++ ident ++ "] Timeout"
            writeBuildResults inst BuildTimeout
    timeout buildTimeout whenTimeout $ do
        (wallTime, (code, _stdout, stderr)) <- timeIO $
            readProcessWithExitCode cmd args ""
        case code of
            ExitSuccess -> do
                callProcess "strip" ["-s", dst]
                benchmarkSize <- getFileSize dst
                oldResults <- readBuildResults inst
                let compileTimes = wallTime : buildResultsWallTime oldResults
                writeBuildResults inst $
                    BuildSuccess benchmarkSize compileTimes
                --putStrLn $ "[" ++ ident ++ "] Success"
            ExitFailure{} -> do
                writeRunResults inst (CompileError stderr)
                putStrLn $ "[" ++ ident ++ "] Failure:"
                putStrLn stderr
  where
    config = instanceConfiguration inst
    benchmark = instanceBenchmark inst
    src = benchmarkSource benchmark
    cmd = configurationCompiler config
    flavor = configurationFlavor config

    ident = instanceName inst

readBenchmarkStdin :: Benchmark -> IO ByteString
readBenchmarkStdin benchmark | not (benchmarkStdin benchmark) = return B.empty
readBenchmarkStdin benchmark = do
    B.readFile (replaceExtension (benchmarkSource benchmark) "stdin")

readBenchmarkStdout:: Benchmark -> IO ByteString
readBenchmarkStdout benchmark | not (benchmarkStdout benchmark) = return B.empty
readBenchmarkStdout benchmark = do
    B.readFile (replaceExtension (benchmarkSource benchmark) "stdout")

runTimeout :: Int
runTimeout = 60*5

readProcessWithExitCode' :: FilePath -> [String] -> ByteString
                        -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode' cmd args stdin = do
    (stdinH, stdoutH, stderrH, pid) <- runInteractiveProcess cmd args Nothing Nothing
    forkIO $ do
        B.hPut stdinH stdin
        hClose stdinH
    stdoutVar <- newEmptyMVar
    stderrVar <- newEmptyMVar
    forkIO $ putMVar stdoutVar =<< B.hGetContents stdoutH
    forkIO $ putMVar stderrVar =<< B.hGetContents stderrH
    exitCode <- waitForProcess pid
    stdout <- takeMVar stdoutVar
    stderr <- takeMVar stderrVar
    return (exitCode, stdout, stderr)

runAndMeasure :: FilePath -> [String] -> ByteString -> IO (ExitCode, ByteString, ByteString)
runAndMeasure cmd args stdin = do
    mbPerf <- findExecutable "perf"
    case mbPerf of
        Nothing -> readProcessWithExitCode' cmd args stdin
        Just perf ->
            readProcessWithExitCode' perf ("stat":"-x":" ":cmd:args) stdin

collectStats :: ByteString -> RuntimeStats -> RuntimeStats
collectStats output s0 = foldl worker s0 (map (words . B.unpack) (B.lines output))
  where
    worker stats line =
        case line of
            [n,"context-switches"] -> stats{ runtimeContextSwitches = read n }
            [n,"cpu-migrations"]   -> stats{ runtimeCpuMigrations = read n }
            [n,"page-faults"]      -> stats{ runtimePageFaults = read n }
            [n,"cycles"]           -> stats{ runtimeCycles = read n }
            [n,"stalled-cycles-frontend"] ->
                stats{ runtimeStalledCycles = read n }
            [n,"instructions"]     -> stats{ runtimeInstructions = read n }
            [n,"branches"]         -> stats{ runtimeBranches = read n }
            [n,"branch-misses"]    -> stats{ runtimeBranchMisses = read n }
            _ -> stats

-- [app dir]/runs/id/results
runInstance :: Instance -> IO ()
runInstance inst | benchmarkDisabled (instanceBenchmark inst) = do
    writeRunResults inst RunDisabled
runInstance inst = unless (benchmarkDisabled $ instanceBenchmark inst) $ do
    isBuilt <- hasBuiltInstance inst
    when isBuilt $ do
        putStrLn $ "[" ++ ident ++ "] Running"
        prog <- instanceProgramFile inst
        stdin <- readBenchmarkStdin benchmark
        let whenTimeout = do
                putStrLn $ "[" ++ ident ++ "] Timeout"
                writeRunResults inst (RuntimeError "timeout")
        timeout runTimeout whenTimeout $ do

            (wallTime, (code, stdout, stderr)) <- timeIO $
                runAndMeasure prog args stdin
            case code of
                ExitSuccess -> do
                    canonicalStdout <- readBenchmarkStdout benchmark
                    if canonicalStdout == stdout
                        then do
                            oldResults <- readRunResults inst
                            let oldStats = runResultsStats oldResults
                                newStats = collectStats stderr $ RuntimeStats
                                    { runtimeWalltime = wallTime
                                    , runtimeContextSwitches = 0
                                    , runtimeCpuMigrations = 0
                                    , runtimePageFaults = 0
                                    , runtimeCycles = 0
                                    , runtimeStalledCycles = 0
                                    , runtimeInstructions = 0
                                    , runtimeBranches = 0
                                    , runtimeBranchMisses = 0 }
                            writeRunResults inst $
                                RunSuccess (newStats : oldStats)
                            putStrLn $ "[" ++ ident ++ "] Success"

                        else do
                            writeRunResults inst DiffError
                            putStrLn $ "[" ++ ident ++ "] Invalid run"
                ExitFailure{} -> do
                    writeRunResults inst (RuntimeError $ B.unpack stderr)
                    putStrLn $ "[" ++ ident ++ "] Failure: "
                    B.putStrLn stderr
  where
    config = instanceConfiguration inst
    benchmark = instanceBenchmark inst
    flavor = configurationFlavor config
    args = flavorRunArgs flavor ++ benchmarkArgs benchmark ++
        configurationRunOptions config

    ident = instanceName inst


analyseWallTime :: [RunResults] -> [Cell]
analyseWallTime results =
    [ case result of
        RunSuccess timings ->
            let t = average (map runtimeWalltime timings) in
            SuccessCell (TimeCell t) (t/smallest)
        CompileError msg -> CompileErrorCell msg
        RuntimeError msg -> RuntimeErrorCell msg
        DiffError        -> DiffErrorCell
        RunDisabled      -> RunDisabledCell
        RunIncomplete    -> RunDisabledCell
    | result <- results ]
  where
    wallTimes =
        [ average (map runtimeWalltime timings)
        | RunSuccess timings <- results ]
    smallest =
        case wallTimes of
            [] -> 1
            (x:xs) -> foldr min x xs
    average [] = 0
    average lst = sum lst / fromIntegral (length lst)

analyseCounter :: (RuntimeStats -> Integer) -> [RunResults] -> [Cell]
analyseCounter counter results =
    [ case result of
        RunSuccess timings ->
            let t = average (map counter timings) in
            SuccessCell (CounterCell t) (ratio t)
        CompileError msg -> CompileErrorCell msg
        RuntimeError msg -> RuntimeErrorCell msg
        DiffError        -> DiffErrorCell
        RunDisabled      -> RunDisabledCell
        RunIncomplete    -> RunDisabledCell
    | result <- results ]
  where
    counters =
        [ average (map counter timings)
        | RunSuccess timings <- results ]
    ratio _ | smallest == 0 = 1
    ratio a = fromIntegral a / fromIntegral smallest
    smallest =
        case counters of
            [] -> 0
            (x:xs) -> foldr min x xs
    average [] = 0
    average lst = sum lst `div` fromIntegral (length lst)

runResultsToTable :: ([RunResults] -> [Cell])
                  -> [(Instance, RunResults)] -> Table
runResultsToTable analyse pairs =
    Table
        [ (show flavor, labels flavor) | flavor <- flavors ]
        
        [ Category category
            [ (prog,
                (analyse results)
                )
            | prog <- programs category
            , let results = rowResults category prog
            ]
        | category <- categories
        ]
  where
    instances = map fst pairs
    benchmarks = nub $ map instanceBenchmark instances
    categories = nub $ map benchmarkCategory benchmarks
    programs category = nub $
        [ benchmarkName benchmark
        | benchmark <- benchmarks
        , benchmarkCategory benchmark == category ]
    rowResults category name =
        [ result
        | flavor <- flavors
        , label <- labels flavor
        , (inst, result) <- pairs
        , let benchmark = instanceBenchmark inst
        , let config = instanceConfiguration inst
        , benchmarkCategory benchmark == category
        , benchmarkName benchmark == name
        , configurationFlavor config == flavor
        , configurationLabel config == label ]
    flavors = nub $ map (configurationFlavor . instanceConfiguration) instances
    labels flavor = nub
        [ configurationLabel config | inst <- instances
        , let config = instanceConfiguration inst
        , configurationFlavor config == flavor ]

writeReport :: [Instance] -> IO ()
writeReport instances = do
    results <- mapM readRunResults instances
    let rs = zip instances results
    let wallTime = runResultsToTable analyseWallTime rs
        cs f = runResultsToTable (analyseCounter f) rs

    let tables =
            [ ("Wall time", wallTime)
            , ("Cycles", cs runtimeCycles)
            , ("Stalled cycles", cs runtimeStalledCycles)
            , ("Instructions", cs runtimeInstructions)
            , ("Context switches", cs runtimeContextSwitches)
            , ("Minor page faults", cs runtimePageFaults)
            , ("Branches", cs runtimeBranches)
            , ("Branch misses", cs runtimeBranchMisses)
            , ("Cpu migrations", cs runtimeCpuMigrations)
            ]
    writeFile "report.html" (show $ mkAnalysis tables)


knownConfigurations :: [Configuration]
knownConfigurations =
    [
    Configuration
        { configurationFlavor       = GHC
        , configurationLabel        = "non-threaded"
        , configurationCompiler     = "ghc"
        , configurationBuildOptions = ["-O2","-rtsopts"]
        , configurationRunOptions   = []
        }
    , Configuration
        { configurationFlavor       = GHC
        , configurationLabel        = "threaded1"
        , configurationCompiler     = "ghc"
        , configurationBuildOptions = ["-O2", "-rtsopts", "-threaded"]
        , configurationRunOptions   = ["+RTS","-N1","-RTS"]
        }
    , Configuration
        { configurationFlavor       = GHC
        , configurationLabel        = "threadedN"
        , configurationCompiler     = "ghc"
        , configurationBuildOptions = ["-O2", "-rtsopts", "-threaded"]
        , configurationRunOptions   = ["+RTS","-N","-RTS"]
        }
    , Configuration
        { configurationFlavor       = GHC
        , configurationLabel        = "large-nursery"
        , configurationCompiler     = "ghc"
        , configurationBuildOptions = ["-O2","-rtsopts"]
        , configurationRunOptions   = ["+RTS","-A3M","-RTS"]
        }
    --, Configuration
    --    { configurationFlavor       = AJHC
    --    , configurationLabel        = "AJHC"
    --    , configurationCompiler     = "ajhc"
    --    , configurationBuildOptions = []
    --    , configurationRunOptions   = []
    --    }
    --, Configuration
    --    { configurationFlavor       = UHC
    --    , configurationLabel        = "UHC"
    --    , configurationCompiler     = "uhc"
    --    , configurationBuildOptions = ["-O2"]
    --    , configurationRunOptions   = []
    --    }
    ]


timeout :: Int -> IO a -> IO a -> IO a
timeout time whenTimeout action = do
    tid <- myThreadId
    killer <- forkIO $ do
        threadDelay (10^6 * time)
        killThread tid
    ret <- handle handleKilled action
    killThread killer
    return ret
  where
    handleKilled ThreadKilled = whenTimeout
    handleKilled e = throwIO e

timeIO :: IO a -> IO (Double, a)
timeIO ioa = do
    t1 <- getCurrentTime
    a <- ioa
    t2 <- getCurrentTime
    return (realToFrac $ diffUTCTime t2 t1, a)


main :: IO ()
main = do
    [path] <- getArgs
    benchmarks <- findBenchmarks path
    instances <- fmap concat $ mapM (flip instantiate benchmarks) knownConfigurations
    let sorted = sortBy (comparing instanceName) instances
    forM_ sorted $ \inst -> do
        isBuilt <- hasBuiltInstance inst
        unless isBuilt $ do
            buildInstance inst
            writeReport sorted
    forM_ sorted $ \inst -> do
        runInstance inst
        writeReport sorted
    writeReport sorted
    return ()
