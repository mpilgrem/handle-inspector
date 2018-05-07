{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Exception (Exception, SomeException(..), throw, try)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.))
import Data.Typeable (Typeable)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import System.Console.MinTTY (isMinTTYHandle)
import System.Environment (lookupEnv)
import System.IO(hIsTerminalDevice, stderr, stdout)
import System.Win32.Types (BOOL, DWORD, ErrCode, HANDLE, getLastError,
  iNVALID_HANDLE_VALUE, nullHANDLE, withHandleToHANDLE)

import System.Console.ANSI (hSupportsANSI)

#if defined(i386_HOST_ARCH)
#define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#define WINDOWS_CCONV ccall
#else
#error Unknown mingw32 arch
#endif

main :: IO ()
main = do
  putStrLn "Inspecting handle and environment"
  putStrLn "---------------------------------"
  putStr "\n"
  isTDOut <- hIsTerminalDevice stdout
  putStrLn $ "hIsTerminalDevice stdout: " ++ show isTDOut
  isTDErr <- hIsTerminalDevice stderr
  putStrLn $ "hIsTerminalDevice stderr: " ++ show isTDErr
  withHandleToHANDLE stdout (\h -> putStrLn $ "stdout: " ++ show h)
  withHandleToHANDLE stderr (\h -> putStrLn $ "stderr: " ++ show h)
  isMTOut <- withHandleToHANDLE stdout isMinTTYHandle
  putStrLn $ "isMinTTYHandle stdout: " ++ show isMTOut
  isMTErr <- withHandleToHANDLE stderr isMinTTYHandle
  putStrLn $ "isMinTTYHandle stderr: " ++ show isMTErr
  soh <- getStdHandle sTD_OUTPUT_HANDLE
  putStrLn $ "STD_OUTPUT_HANDLE: " ++ show soh
  seh <- getStdHandle sTD_ERROR_HANDLE
  putStrLn $ "STD_ERROR_HANDLE: " ++ show seh
  when ((soh /= iNVALID_HANDLE_VALUE) && (soh /= nullHANDLE)) $ do
    mmode <- conHostConsoleMode soh
    case mmode of
      Nothing -> putStrLn "No Console Mode?"
      Just mode -> do
        putStrLn $ "Console Mode - EVTP: " ++ show (isEnableVTProcessing mode)
        let mode' = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
        mOk <- changeConHostConsoleMode soh mode'
        case mOk of
          Nothing -> putStrLn "Unable to change mode."
          Just _ -> putStrLn "Mode changed!"
  trm <- lookupTERM
  putStrLn $ "TERM: " ++ show trm
  isSAOut <- hSupportsANSI stdout
  putStrLn $ "hSupportsANSI stdout: " ++ show isSAOut
  isSAErr <- hSupportsANSI stderr
  putStrLn $ "hSupportsANSI stderr: " ++ show isSAErr



foreign import WINDOWS_CCONV unsafe "windows.h GetStdHandle"
  getStdHandle :: DWORD -> IO HANDLE

-- The function tries to get a ConHost console mode from a handle that is
-- assumed to be a valid standard handle (see getValidStdHandle)
conHostConsoleMode :: HANDLE -> IO (Maybe DWORD)
conHostConsoleMode h = do
  result <- try (getConsoleMode h) :: IO (Either SomeException DWORD)
  case result of
    Left _ -> return Nothing
    Right mode -> return (Just mode)

changeConHostConsoleMode :: HANDLE -> DWORD -> IO (Maybe ())
changeConHostConsoleMode h mode = do
  result <- try (setConsoleMode h mode) :: IO (Either SomeException ())
  case result of
    Left _ -> return Nothing
    Right _ -> return (Just ())

isEnableVTProcessing :: DWORD -> Bool
isEnableVTProcessing mode = mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0

lookupTERM :: IO (Maybe String)
lookupTERM = lookupEnv "TERM"

eNABLE_VIRTUAL_TERMINAL_PROCESSING :: DWORD
eNABLE_VIRTUAL_TERMINAL_PROCESSING =   4

sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE, sTD_ERROR_HANDLE :: DWORD
sTD_INPUT_HANDLE  = 0xFFFFFFF6 -- minus 10
sTD_OUTPUT_HANDLE = 0xFFFFFFF5 -- minus 11
sTD_ERROR_HANDLE  = 0xFFFFFFF4 -- minus 12

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleMode"
  cGetConsoleMode :: HANDLE -> Ptr DWORD -> IO BOOL

getConsoleMode :: HANDLE -> IO DWORD
getConsoleMode handle = alloca $ \ptr_mode -> do
  throwIfFalse $ cGetConsoleMode handle ptr_mode
  peek ptr_mode

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleMode"
  cSetConsoleMode :: HANDLE -> DWORD -> IO BOOL

setConsoleMode :: HANDLE -> DWORD -> IO ()
setConsoleMode handle attributes
  = throwIfFalse $ cSetConsoleMode handle attributes

throwIfFalse :: IO Bool -> IO ()
throwIfFalse action = do
  succeeded <- action
  if not succeeded
    then getLastError >>= throw . ConsoleException -- TODO: Check if last error
    -- is zero for some instructable reason (?)
    else return ()

data ConsoleException = ConsoleException !ErrCode deriving (Eq, Typeable)

instance Show ConsoleException where
  show (ConsoleException 6) =
    "A fatal error has occurred.\n\n" ++
    "An attempt has been made to send console virtual terminal sequences\n" ++
    "(ANSI codes) to an output that has not been recognised as an\n" ++
    "ANSI-capable terminal and also cannot be emulated as an ANSI-enabled\n" ++
    "terminal (emulation needs a ConHost-based terminal, such as Command\n" ++
    "Prompt or PowerShell). That may occur, for example, if output has\n" ++
    "been redirected to a file.\n\n" ++
    "If that is unexpected, please post an issue at:\n" ++
    "https://github.com/feuerbach/ansi-terminal/issues\n"
  show (ConsoleException errCode) = "ConsoleException " ++ show errCode

instance Exception ConsoleException
