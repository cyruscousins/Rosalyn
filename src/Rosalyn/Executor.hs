{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Rosalyn.Executor where

--Logic imports
import Data.Hashable
import Data.List
import Numeric (showHex)

import Rosalyn.ListUtils

--IO imports
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Process
import System.Posix.Files

--Process imports
import System.Process

--Other imports
import Control.Monad

--This module provides utilities for interfacing with external programs.
--Ideally, with minimal interfacing code, external programs may be executed and memoized with syntax identical to that of ordinary function execution.

memoizationRoot :: String
memoizationRoot = "memo"

--Executable class.  Data structures 

--A bit of black magic to get allow constants in typeclases.
--newtype StringConstant a = StringConstant String
--newtype Void a = Void ()
--unpackSC :: StringConstant a -> String
--unpackSC (StringConstant a) = a

class (Eq a, Hashable a) => Executable p a b | p -> a b where
  --type (P (Executable a b)) = Executable a b
  --Inherent properties of a program (constants).
  binaryName :: p -> String --StringConstant (a, b)
  subName :: p -> String --StringConstant (a, b)
  --isStochastic :: Bool
  --isStochastic = False
  programDirectory :: p -> String --StringConstant a
  programDirectory p = (intercalate "/" [memoizationRoot, binaryName p, subName p]) ++ "/"
  executionSubdirectory :: p -> a -> String -- TODO needs p?
  executionSubdirectory p a = (showHex (hash a) "") --TODO show hex string instead of decimal string.
  --TODO use default to access hash, remove typeclass constraint.
  fullDirectory :: p -> a -> String
  fullDirectory p a = (programDirectory p) ++ (executionSubdirectory p a) ++ "/"
  --Filesystem interaction to interface with the program.
  --TODO generalized writeOutput and output functions.
  arguments :: p -> a -> [String]
  arguments _ _ = [] --Default is no arguments.
  writeInput :: p -> a -> IO ()
  readOutput :: p -> a -> IO b
  --TODO default versions of writeInput and readInput that operate on single files.  Provide nonmonadic functions to interface with these files.
  --Program installation
  --install :: IO () --TODO use void
  --install = return ()
  --checkInstallation :: IO Bool
  --checkInstallation = undefined --TODO default should check if the binary to be executed exists.
  --Program execution.  Defaults provided in terms of the above information.
  checkForMemo :: p -> a -> IO Bool
  checkForMemo p a = doesFileExist ((fullDirectory p a) ++ "complete")
  executeProgram :: p -> a -> IO ()
  executeProgram p a =
    let binName = (binaryName p)
        dir = fullDirectory p a
        stdOutFile = dir ++ "stdout"
        stdErrFile = dir ++ "stderr"
        completeFile = dir ++ "complete"
        originSymLink = dir ++ "origin"
        args = arguments p a
     in do (createDirectoryIfMissing True dir) ;
           cwd <- getCurrentDirectory ;
           setCurrentDirectory (cwd ++ "/" ++ (dir)) ;
           writeInput p a ;
           setCurrentDirectory cwd ;
           stdOutStream <- openFile stdOutFile WriteMode ;
           stdErrStream <- openFile stdErrFile WriteMode ;
           --TODO here we hack in a path back to the calling directory under "origin".  This will cause some issues if multiple programs share an execution directory, and its use violates the encapsulation of Rosalyn.  Perhaps it should point to externalBin instead, or be handled with function signatures.
           exists <- fileExist originSymLink ;
           if exists then return () else createSymbolicLink cwd originSymLink ; --TODO 
           (_, _, _, process) <- createProcess (CreateProcess { cmdspec = (RawCommand binName args), cwd = (Just (cwd ++ "/" ++ dir)), env = Nothing, std_in = Inherit, std_out = (UseHandle stdOutStream), std_err = (UseHandle stdErrStream), close_fds = False, create_group = False, delegate_ctlc = False }) ; --TODO Inherit should be NoHandle.  Should try to isolate the process as much as possible. --TODO may need to set the path.
           _ <- waitForProcess process ; --Don't care about the exit code.
           writeFile completeFile ""
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
           if memoExists then (return ()) else executeProgram p a ;
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

{-
data DynamicExecutable = DynamicExecutable 

instance Executable DynamicExecutable where
  binaryName DynamicExecutable (name, _) _ _ _ = name
  subName DynamicExecutable (_, subname) _ _ _ = name
-}

