{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}
module Rosalyn.Executor where

--Logic imports
import Data.Hashable
import Data.List
import Data.Word
import Numeric (showHex)

import Rosalyn.ListUtils

--IO imports
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Posix.Files

--Process imports
import System.Exit
import System.Process

--Other imports
import Control.Monad

--This module provides utilities for interfacing with external programs.
--Ideally, with minimal interfacing code, external programs may be executed and memoized with syntax identical to that of ordinary function execution.

--Global constant pointing to the directory where memos are stored.
memoizationRoot :: String
memoizationRoot = "memo"

--class Executable p where
--  type a :: * -> * --input type.
--  type b :: * -> * --output type.

class Executable p a b | p -> a b where
  --Inherent properties of a program (constants).
  binaryName :: p -> String --StringConstant (a, b)
  subName :: p -> String --StringConstant (a, b)
  --isStochastic :: Bool
  --isStochastic = False
  programDirectory :: p -> String --StringConstant a
  programDirectory p = (intercalate "/" [memoizationRoot, binaryName p, subName p]) ++ "/"
  executionSubdirectory :: p -> a -> String
  default executionSubdirectory :: (Hashable p, Hashable a) => p -> a -> String
  executionSubdirectory p a =
    let p32 :: Word32
        p32 = fromIntegral $ hash p
        a32 :: Word32
        a32 = fromIntegral $ hash a
     in (showHex p32 "") ++ " " ++ (showHex a32 "")
  fullDirectory :: p -> a -> String
  fullDirectory p a = (programDirectory p) ++ (executionSubdirectory p a) ++ "/"
  --Filesystem interaction to interface with the program.
  --TODO generalized writeOutput and output functions.
  arguments :: p -> a -> [String]
  arguments _ _ = [] --Default is no arguments.
  --Convenience function for use with writeInput.
  inputToString :: p -> a -> String
  default inputToString :: (Show a) => p -> a -> String
  inputToString _ = show
  writeInput :: p -> a -> IO ()
  writeInput p a = writeFile "stdin" (inputToString p a)
  outputFromString :: p -> String -> b
  default outputFromString :: (Read b) => p -> String -> b
  outputFromString _ = read
  readOutput :: p -> a -> IO b
  readOutput p _ = fmap (outputFromString p) (readFile "stdout")
  install :: p -> IO ()
  install = fail "No installation protocol defined."
  checkInstallation :: p -> IO Bool
  checkInstallation p =
    do (_, _, _, process) <- createProcess (CreateProcess { cmdspec = (RawCommand "which" [binaryName p]), cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False }) ; --TODO replace Inherit with NoStream.
       code <- waitForProcess process ;
       return (code == ExitSuccess) ;
  --Program execution.  Defaults provided in terms of the above information.
  checkForMemo :: p -> a -> IO Bool
  checkForMemo p a = doesFileExist ((fullDirectory p a) ++ "complete")
  executeProgram :: p -> a -> IO ()
  executeProgram p a =
    let binName = (binaryName p)
        dir = fullDirectory p a
        pdir = programDirectory p
        stdInFile = dir ++ "stdin"
        stdOutFile = dir ++ "stdout"
        stdErrFile = dir ++ "stderr"
        completeFile = dir ++ "complete"
        originSymLink = dir ++ "origin"
        args = arguments p a
     in do cwd <- getCurrentDirectory ;
           --Ensure the directory exists.
           (createDirectoryIfMissing True dir) ;
           --Switch to the program directory (assumed to be a parent of dir).
           setCurrentDirectory (cwd ++ "/" ++ pdir) ;
           --See if the program is installed, and if it is not, attempt to install it .
           installed <- checkInstallation p ;
           unless installed $ install p ;
           --Switch to the subdirectory for this particular execution.
           setCurrentDirectory (cwd ++ "/" ++ dir) ;
           --Perform necessary IO in the execution subdirectory.
           writeInput p a ;
           --Reset the CWD of this process.
           setCurrentDirectory cwd ;
           --Create file handles to redirect IO.
           stdInStream <- openFile stdInFile ReadMode ;
           stdOutStream <- openFile stdOutFile WriteMode ;
           stdErrStream <- openFile stdErrFile WriteMode ;
           --TODO here we hack in a path back to the calling directory under "origin".  This will cause some issues if multiple programs share an execution directory, and its use violates the encapsulation of Rosalyn.  Perhaps it should point to externalBin instead, or be handled with function signatures.
           exists <- doesFileExist originSymLink ;
           unless exists $ createSymbolicLink cwd originSymLink ;
           --Create the process, pass it arguments, file handles for IO, and execute it in the proper directory.
           (_, _, _, process) <- createProcess (CreateProcess { cmdspec = (RawCommand binName args), cwd = (Just (cwd ++ "/" ++ dir)), env = Nothing, std_in = (UseHandle stdInStream), std_out = (UseHandle stdOutStream), std_err = (UseHandle stdErrStream), close_fds = False, create_group = False, delegate_ctlc = False }) ; -- Should try to isolate the process as much as possible. --TODO may need to set the path.
           --Wait for the process to terminate.
           waitForProcess process ;
           --Create a file to signal that the irectory contains a valid memo.
           writeFile completeFile "" ;
  readProgramResult :: p -> a -> IO b
  readProgramResult p a = --TODO this allows data races when used with unsafeIO.  We need a way of encapsulating the CWD aspects of the code.
    let progDir = fullDirectory p a
     in do dir0 <- getCurrentDirectory ;
           setCurrentDirectory (dir0 ++ "/" ++ (progDir)) ;
           result <- readOutput p a ;
           setCurrentDirectory dir0 ;
           return result ;
  runProgram :: p -> a -> IO b
  runProgram p a =
    let dir = fullDirectory p a
     in do memoExists <- checkForMemo p a ;
           unless memoExists $ executeProgram p a ;
           readProgramResult p a ;
  runProgramUnsafe :: p -> a -> b
  runProgramUnsafe p a = unsafePerformIO (runProgram p a)
  --Clean a directory used in a single run.
  --cleanRun :: p -> a -> IO ()
  --TODO: Need to make sure this doesn't follow symlinks.  It isn't supposed to, but I've read some bug reports.  Seeing as this would wipe out the entire program directory due to origin, this is a risk I'd not take lightly.
  --cleanRun p a = removeDirectoryRecursive (fullDirectory p a)
  --Clean the directory used by an executable.  This removes all execution directories as well.
  --cleanProgram :: p -> a -> IO ()
  --cleanProgram p a = removeDirectoryRecursive (programDirectory p a)
  --TODO: Interface for removing intermediate results from a program directory.
{-
data DynamicExecutable = DynamicExecutable 

instance Executable DynamicExecutable where
  binaryName DynamicExecutable (name, _) _ _ _ = name
  subName DynamicExecutable (_, subname) _ _ _ = name
-}

