------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Run
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: starting, stopping and reading event sets
--

module System.PAPI.Run (
  EventState(..)
, state
, start
, stop
, read
, accum
, readTs
, write
, reset
) where

import Prelude hiding (read)

import Foreign
import Foreign.C

import System.PAPI.EventSet
import System.PAPI.Error
import System.PAPI.Util

#include "papi.h"

data EventState = EventState {
  eventStateStopped :: Bool,
  eventStateRunning :: Bool,
  eventStatePaused :: Bool,
  eventStateNotInit :: Bool,
  eventStateOverflowing :: Bool,
  eventStateProfiling :: Bool,
  eventStateMultiplexing :: Bool,
  eventStateAttached :: Bool
} deriving (Eq, Show)

instance Storable EventState where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
    cint <- peek (castPtr ptr :: Ptr CInt)
    return $ EventState
      (toBool $ cint .&. #{const PAPI_STOPPED})
      (toBool $ cint .&. #{const PAPI_RUNNING})
      (toBool $ cint .&. #{const PAPI_PAUSED})
      (toBool $ cint .&. #{const PAPI_NOT_INIT})
      (toBool $ cint .&. #{const PAPI_OVERFLOWING})
      (toBool $ cint .&. #{const PAPI_PROFILING})
      (toBool $ cint .&. #{const PAPI_MULTIPLEXING})
      (toBool $ cint .&. #{const PAPI_ATTACHED})

  poke ptr value = do
    poke (castPtr ptr :: Ptr CInt) $
      toMask (eventStateStopped      value) #{const PAPI_STOPPED} .|.
      toMask (eventStateRunning      value) #{const PAPI_RUNNING} .|.
      toMask (eventStatePaused       value) #{const PAPI_PAUSED} .|.
      toMask (eventStateNotInit      value) #{const PAPI_NOT_INIT} .|.
      toMask (eventStateOverflowing  value) #{const PAPI_OVERFLOWING} .|.
      toMask (eventStateProfiling    value) #{const PAPI_PROFILING} .|.
      toMask (eventStateMultiplexing value) #{const PAPI_MULTIPLEXING} .|.
      toMask (eventStateAttached     value) #{const PAPI_ATTACHED}

foreign import ccall "PAPI_state" papi_state
  :: EventSet -> Ptr EventState -> IO PapiError
state :: EventSet -> IO EventState
state eventSet =
  alloca $ \ptr ->
  papi_state eventSet ptr >>= papiThrow "Error in state: " >> peek ptr

foreign import ccall "PAPI_start" papi_start :: EventSet -> IO PapiError
start :: EventSet -> IO ()
start eventset =
  papi_start eventset >>= papiThrow "Error in start: " >> return ()

foreign import ccall "PAPI_stop" papi_stop
  :: EventSet -> Ptr CLLong -> IO PapiError
stop :: EventSet -> Int -> IO [Integer]
stop eventset len =
  allocaArray len $ \event ->
  papi_stop eventset event >>= papiThrow "Error in stop: "
  >> peekArray len event >>= return . map toInteger

foreign import ccall "PAPI_read" papi_read
  :: EventSet -> Ptr CLLong -> IO PapiError
read :: EventSet -> Int -> IO [Integer]
read eventset len =
  allocaArray len $ \event ->
  papi_read eventset event >>= papiThrow "Error in read: "
  >> peekArray len event >>= return . map toInteger

foreign import ccall "PAPI_accum" papi_accum
  :: EventSet -> Ptr CLLong -> IO PapiError
accum :: EventSet -> [Integer] -> IO [Integer]
accum eventset values =
  withArrayLen (map fromInteger values) $ \len ptr ->
  papi_accum eventset ptr >>= papiThrow "Error in accum: "
  >> peekArray len ptr >>= return . map toInteger

foreign import ccall "PAPI_read_ts" papi_read_ts
  :: EventSet -> Ptr CLLong -> Ptr CLLong -> IO PapiError
readTs :: EventSet -> Int -> IO ([Integer], Integer)
readTs eventset len =
  alloca $ \cyc ->
  allocaArray len $ \ptr ->
  papi_read_ts eventset ptr cyc >>= papiThrow "Error in readTs: " >>
  peekArray len ptr >>= \values ->
  peek cyc >>= \c -> return (map toInteger values, toInteger c)

foreign import ccall "PAPI_write" papi_write
  :: EventSet -> Ptr CLLong -> IO PapiError
write :: EventSet -> [Integer] -> IO ()
write eventset values =
  withArray (map fromInteger values) $ \ptr ->
  papi_write eventset ptr >>= papiThrow "Error in write: " >> return ()

foreign import ccall "PAPI_reset" papi_reset :: EventSet -> IO PapiError
reset :: EventSet -> IO ()
reset eventset =
  papi_reset eventset >>= papiThrow "Error in reset: " >> return ()
