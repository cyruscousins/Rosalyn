module Rosalyn.Executor where

--Logic imports
import Data.Hashable

--IO imports
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Process

--Process imports
import System.Process

--Other imports
import Control.Monad

-------------------
--Design Comments--
{-
Program execution should occur with the PWD set to the memoization directory (stronger sandboxing).
However this clashes with the reader/writer functions operating in the main process (and using the main program PWD).

--TODO Perhaps program should be a typeclass, and programs would be types (rather than functions)?
--We could then have the typeclass provide a convenient function for execution, and we'd get some convenient alternative easy ways to represent them.
-}

--This module provides an interface to perform RFI like tasks using arbitrary binaries in Rosalyn with memoization.

--A Program type.  This represents the type of the programs inputs and outputs, and how to read and write them.
--This includes program arguments, standard IO, and additional file IO.
data Program a b = Program String String (a -> String -> [String]) (a -> String -> IO ()) (a -> String -> IO b) (a -> [String]) -- (Maybe String)
--Name of the program binary.
--Subname (to prevent memoization conflicts).
--Function creating program arguments.
--Function writing program input to the file system (given a directory).
--Function reading program output from file (given a directory).
--Function generating the names of all program output files (not relative to directory), for memoization.


--TODO need noinline on unsafeIO functions.  Also use -fno-cse
--Consult https://hackage.haskell.org/package/base-4.0.0.0/docs/System-IO-Unsafe.html#v%3AunsafeInterleaveIO


--TODO this design is bad.  The cleaner should just be a generic IO action.
--Cleans up intermediate temporary files
cleanTmp :: [String] -> IO ()
{-# NOINLINE cleanTmp #-}
cleanTmp _ = return () --TODO Empty implementation

memoRoot = "./memo/" --TODO

{-
--Need noinline for unsafe IO
runProgram :: (Hashable a) => Program a b -> a -> b
{-# NOINLINE runProgram #-}
runProgram prog@(Program name pa pw pr outFiles) a = 
  --Set up a temporary environment
  let (phash, ahash) = (hash name, hash a) --TODO: this may be recalculated from caller.
      programDir = memoRoot ++ "/" ++ (show phash)
      parameterizationDir = programDir ++ "/" ++ (show ahash)
   in if (unsafePerformIO (doesDirectoryExist parameterizationDir))
      then --The directory exists.  Remove it and recur to run again.
        let actions = do
              removeDirectoryRecursive parameterizationDir
              runProgram prog a
         in unsafePerformIO actions
      else --Set up environment.  Run program.  Read results.
        let actions = do
              pw parameterizationDir a ;
              callProcess (name (pa a)) ; --TODO the clowns that designed the interface raise an exception instead of returning IO Int.  Need to handle the exception.
              pr parameterizationDir a ;
              cleanTmp a (outFiles a) ;
         in unsafePerformIO actions
          
-}
  --seq3 (unsafePerformIO (pw $ w a)) (execp (name (pa a))) (unsafePerformIO pr)
  
--TODO I have no idea what removeMemoization is supposed to do.  I forget what I was thinking when I wrote it...  This typechecks though!
removeMemoization = id

--TODO need to maintain locks on ahashes to make sure the same program is not run concurrently with itself.  This makes running a program from itself with identical parameterization not terminate (but this would happen anyway).  However hash collisions may result in deadlock.  TODO this could be resolved with a more sophisticated locking mechanism.

{-
runProgram :: (Eq a, Hashable a) => Program a b -> a -> IO b
runProgram mp@(Program name pa pw pr fileEnumerator) a =
  let (phash, ahash) = (hash name, hash a) --(Lazily calculated)
      programDir = memoRoot ++ "/" ++ (show phash)
      parameterizationDir = programDir ++ "/" ++ (show ahash)
    --If a memoization folder for the program exists, the memoization folder for the parameterization exists
   in
    if (unsafePerformIO (dExists programDir) && unsafePerformIO (dExists parameterizationDir)) then
      let fullFile = parameterizationDir ++ "/" ++ completionFileName
      --Check that some memoization is complete
       in
      if (unsafePerformIO (fExists fullFile)) then
        --Check that the memoization is of the same original problem (this is to resolve hash conflicts and ensure that the program terminated successfully, as this is the last thing that is created).
        --TODO may want to log hashes instead of the original problem so we don't have to redo this.
        --TODO If we save the input files of a run, we could just regenerate those and compare.
        let matches = (==) a (read $ unsafePerformIO)
        in
          if matches then
            --Read the result from the FS.
            pr a parameterizationDir
          else
            --The memoization didn't match.  Delete the folder and rerun.
            --TODO print a hash collision warning warning?
            do
              rmDirRecursive parameterizationDir ;
              runProgram (removeMemoization mp) a) ;
      --Check that the memoization is complete and for identical parameterization (given by a file containing a that is written at the end).
      else --The directory was created, but for some reason no memoization file was created.  Can't happen unless I messed up the concurrency locking logic OR a program was terminated with the memoization code in an inconsistent state.  Either way, the way we recover is to start over.
        do
          rmDirRecursive parameterizationDir ; 
          runProgram (removeMemoization mp) a ;
    else --The directory or program directory was not found.
      --TODO mkdirp is overkill
      --Create an outpt directory
      do
        mkdirp parametirizationDir ;
        runProgram (removeMemoization mp) a ;
-}

mkdirp :: String -> IO ()
mkdirp = createDirectoryIfMissing True

--Perform the actual execution of a program.  This leaves the file system in a state such that the result is ready to be read.
executeProgram :: (Eq a, Hashable a) => Program a b -> a -> IO ()
executeProgram (Program name subname pa pw pr fileEnumerator) a =
  let h = hash a
      programDir = memoRoot ++ "/" ++ name ++ "/" ++ subname ++ "/" --(show phash)
      parameterizationDir = programDir ++ (show h) ++ "/"
      stdOutFile = parameterizationDir ++ "stdout"
      stdErrFile = parameterizationDir ++ "stderr"
      completeFile = parameterizationDir ++ "complete"
   in do
      mkdirp parameterizationDir ;
      pw a parameterizationDir ;
      stdOutStream <- openFile stdOutFile WriteMode ;
      stdErrStream <- openFile stdErrFile WriteMode ;
      --(_, _, _, process) <- createProcess (CreateProcess (RawCommand name (pa a)) Nothing Nothing NoStream stdOutStream stdErrStream True False False False False False Nothing Nothing) ;
      (_, _, _, process) <- createProcess (CreateProcess { cmdspec = (RawCommand name (pa a parameterizationDir)), cwd = Nothing, env = Nothing, std_in = Inherit, std_out = (UseHandle stdOutStream), std_err = (UseHandle stdErrStream), close_fds = False, create_group = False, delegate_ctlc = False }) ; --TODO Inherit should be NoHandle
      _ <- waitForProcess process ; --Don't care about the exit code.
      writeFile completeFile ""

readProgramResult :: (Eq a, Hashable a) => Program a b -> a -> IO b
readProgramResult (Program name subname pa pw pr fileEnumerator) a =
  let h = hash a
      programDir = memoRoot ++ "/" ++ name ++ "/" ++ subname ++ "/" --(show phash)
      parameterizationDir = programDir ++ (show h) ++ "/"
   in do
      pr a parameterizationDir ;

checkProgramResult :: (Eq a, Hashable a) => Program a b -> a -> IO Bool
checkProgramResult (Program name subname pa pw pr fileEnumerator) a =
  let h = hash a
      programDir = memoRoot ++ "/" ++ name ++ "/" ++ subname ++ "/" --(show phash)
      parameterizationDir = programDir ++ (show h) ++ "/"
      completeFile = parameterizationDir ++ "complete"
   in do
      doesFileExist completeFile
      
--Top level function for running a program.  Checks for a memo, runs the program if unavailable, and reads the result.
runProgram :: (Eq a, Hashable a) => Program a b -> a -> IO b
runProgram mp@(Program name subname pa pw pr fileEnumerator) a =
  let (phash, ahash) = (hash name, hash a) --(Lazily calculated)
      programDir = memoRoot ++ "/" ++ name ++ "/" ++ subname ++ "/" --(show phash)
      parameterizationDir = programDir ++ (show ahash) ++ "/"
      --Make a CreateProcess record specifying the parameterization of the process.
   in do
      memoExists <- checkProgramResult mp a ;
      if memoExists then (return ()) else executeProgram mp a ;
      readProgramResult mp a ;

--data Program a b = Program String (a -> String -> [String]) (a -> String -> IO ()) (a -> String -> IO b) (a -> [String])

sedSArgs :: (String, String, String) -> String -> [String]
sedSArgs (str, find, replace) dir = ["-e", "s/" ++ find ++ "/" ++ replace ++ "/", dir ++ "input", "w"]

sedSWriter :: (String, String, String) -> String -> IO ()
sedSWriter (str, _, _) dir = writeFile (dir ++ "input") str

sedSReader :: (String, String, String) -> String -> IO String
sedSReader a d =
  do readFile (d ++ "stdout")

sedS :: Program (String, String, String) String
sedS = Program "sed" "substitute" sedSArgs sedSWriter sedSReader (const ["input", "stdout", "stderr"])

memoizerProgram :: (Hashable a) => String -> (a -> b) -> (b -> String) -> (String -> b) -> Program a b
memoizerProgram name f bs sb = Program "true" name (\ _ _ -> []) (\ a b -> writeFile (b ++ "value") ((bs . f) a)) (\ _ b -> (liftM sb) (readFile (b ++ "value"))) (const [])

memoizer :: (Hashable a, Eq a) => String -> (a -> b) -> (b -> String) -> (String -> b) -> (a -> b)
memoizer name f bs sb = \a -> unsafePerformIO (runProgram (memoizerProgram name f bs sb) a)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = (+) (fib ((-) x 1)) (fib ((-) x 2))

fibMemo :: Int -> Int
fibMemo 0 = 1
fibMemo 1 = 1
fibMemo x =
  let mfib = memoizer "fib" fibMemo show read
   in (+) (mfib ((-) x 1)) (mfib ((-) x 2))


{-
--Example of the use of Keith Noto's AUROC program:

--auroc :: (RealFrac r) => Program [r, Bool] r --Generic version
auroc :: Program [(Double, Bool)] Double
auroc = MemoizedProgram "python" (\ _ -> "auroc.py")  

--Name of the program binary.  Function creating program arguments.  Function writing program input to the file system.  Function generating the names of all program output files.  Function reading program output from file.  Function specifying output files (for memoization).
  MemoizedProgram a b = String -> (a -> [String]) -> (a -> IO ()) -> (IO b) -> (a -> [String]) :: Program a b
-}

{-
data GHCInput = GHCInput [Decl] [String] Int

instance Hashable GHCInput where
  hash (GHCInput _ _ i) = i

ghcArguments :: GHCInput -> String -> [String]
ghcArguments (GHCInput _ args _) dir = [(dir ++ "program.hs"), "-o", (dir + "program")] ++ args

ghcInput :: GHCInput -> String -> IO ()
ghcInput = undefined
--Write source code of the program to (dir ++ "program.hs")

ghcOutput :: GHCInput -> String -> Program
ghcOutput _ dir = Program (dir ++ "program") "" (\ a b -> []) (\ a b -> return ()) (\ a b -> return ())


--Alternatively, output intermediate format to file (.ho or similar?).  Need to change ghcArguments to invoke ghc accordingly.

ghc :: Program GHCInput -> (Program () ())
ghc = Program "ghc" "compile" ghcArguments ghcInput ghcOutput
-}


