{-# LANGUAGE MultiParamTypeClasses #-}
module Rosalyn.Executor2 where

--Logic imports
import Data.Hashable
import Data.List

--IO imports
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Process

--Process imports
import System.Process

--Other imports
import Control.Monad

--This module provides utilities for interfacing with external programs.
--Ideally, with minimal interfacing code, external programs may be executed and memoized with syntax identical to that of ordinary function execution.

memoizationRoot :: String
memoizationRoot = "memo"

--Program class.  Data structures 

--A bit of black magic to get allow constants in typeclases.
newtype StringConstant a = StringConstant String
newtype Void a = Void ()

unpackSC :: StringConstant a -> String
unpackSC (StringConstant a) = a

class (Eq a, Hashable a) => Program a b where
  --Inherent properties of a program (constants).
  binaryName :: StringConstant a
  subName :: StringConstant a
  --isStochastic :: Bool
  --isStochastic = False
  programDirectory :: StringConstant a
  programDirectory = StringConstant $ (intercalate "/" [memoizationRoot, unpackSC binaryName, unpackSC subName]) ++ "/"
  executionSubdirectory :: a -> String
  executionSubdirectory a = (show . hash) a --TODO show hex string instead of decimal string.
  fullDirectory :: a -> String
  fullDirectory a = (unpackSC programDirectory) ++ (unpackSC $ executionSubdirectory a) ++ "/"
  --Filesystem interaction to interface with the program.
  --TODO generalized input and output functions.
  arguments :: a -> [String]
  input :: a -> IO ()
  output :: a -> IO b
  --Program installation
  --install :: IO () --TODO use void
  --install = return ()
  --checkInstallation :: IO Bool
  --checkInstallation = undefined --TODO default should check if the binary to be executed exists.
  --Program execution.  Defaults provided in terms of the above information.
  checkForMemo :: a -> IO Bool
  checkForMemo a = doesFileExist ((fullDirectory a) ++ "complete")
  executeProgram :: a -> IO ()
  executeProgram a =
    let dir = fullDirectory a
        stdOutFile = dir ++ "stdout"
        stdErrFile = dir ++ "stderr"
        completeFile = dir ++ "complete"
        args = arguments a
     in do (createDirectoryIfMissing True dir) ;
           input a ;
           stdOutStream <- openFile stdOutFile WriteMode ;
           stdErrStream <- openFile stdErrFile WriteMode ;
           (_, _, _, process) <- createProcess (CreateProcess { cmdspec = (RawCommand binaryName args), cwd = (Just dir), env = Nothing, std_in = Inherit, std_out = (UseHandle stdOutStream), std_err = (UseHandle stdErrStream), close_fds = False, create_group = False, delegate_ctlc = False }) ; --TODO Inherit should be NoHandle.  Should try to isolate the process as much as possible.
           _ <- waitForProcess process ; --Don't care about the exit code.
           writeFile completeFile ""
  readProgramResult :: a -> IO b
  readProgramResult a = --TODO this allows data races when used with unsafeIO.  We need a way of encapsulating the CWD aspects of the code.
    let progDir = fullDirectory a
     in do dir0 <- getCurrentDirectory
           setCurrentDirectory (dir0 ++ (fullDirectory a))
           result <- input a
           setCurrentDirectory dir0
           return result
  runProgram :: a -> IO b
  runProgram a =
    let dir = fullDirectory a
     in do memoExists <- checkForMemo a
           if memoExists then (return ()) else executeProgram a
           readProgramResult a

{-
data DynamicProgram = DynamicProgram 

instance Program DynamicProgram where
  binaryName DynamicProgram (name, _) _ _ _ = name
  subName DynamicProgram (_, subname) _ _ _ = name
-}

