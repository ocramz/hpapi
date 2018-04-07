------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.HighLevel
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: high-level interface
--

module System.PAPI.HighLevel (
  numCounters
, flips, flops, ipc
, startCounters
, stopCounters
, readCounters
, accumCounters
, withCounters
) where

import Control.Monad
import Foreign
import Foreign.C

import System.PAPI.Event
import System.PAPI.Error

#include "papi.h"

-- Initializing the High-Level API
foreign import ccall "PAPI_num_counters" papi_num_counters :: CInt
numCounters :: Integer
numCounters = toInteger papi_num_counters

-- Execution Rate Calls
type RateCall
  = Ptr Float -> Ptr Float -> Ptr CLLong -> Ptr Float -> IO PapiError
foreign import ccall "PAPI_flips"          papi_flips :: RateCall
foreign import ccall "PAPI_flops"          papi_flops :: RateCall
foreign import ccall "PAPI_ipc"            papi_ipc   :: RateCall

rate_call :: RateCall -> IO (Float, Float, Integer, Float)
rate_call call =
  alloca $ \rtime ->
  alloca $ \ptime ->
  alloca $ \flpins ->
  alloca $ \mflips -> do
    call rtime ptime flpins mflips >>=papiThrow "Error in flips,flops or ipc: "
    liftM4 (,,,)
        (peek rtime) (peek ptime)
        (liftM toInteger $ peek flpins) (peek mflips)

flips, flops, ipc :: IO (Float, Float, Integer, Float)
                     --  rtime, ptime, flpins, mflips
flips = rate_call papi_flips
flops = rate_call papi_flops
ipc   = rate_call papi_ipc

-- Reading, Accumulating, and Stopping Counters
type CounterCall a = Ptr a -> CInt -> IO PapiError
foreign import ccall "PAPI_start_counters" start_counters :: CounterCall CInt
foreign import ccall "PAPI_accum_counters" accum_counters :: CounterCall CLLong
foreign import ccall "PAPI_read_counters"  read_counters  :: CounterCall CLLong
foreign import ccall "PAPI_stop_counters"  stop_counters  :: CounterCall CLLong

counter_call :: (Storable a, Integral a)
  => ((Ptr a -> IO [Integer]) -> IO [Integer])
  -> CounterCall a -> Int -> IO [Integer]
counter_call alloc call int_len =
  alloc $ \values -> do
    call values cint_len >>= papiThrow "Error when manipulating counters: "
    liftM (map toInteger) $ peekArray int_len values
  where cint_len = fromIntegral int_len

startCounters :: [EventCode] -> IO ()
startCounters events =
  withArray (map unEventCode events) $ \eventsPtr -> do
    start_counters eventsPtr (fromIntegral $ length events)
        >>= papiThrow "Error in startCounters: "
    return ()

-- TODO: remove need to pass count to stopCounters and readCounters
stopCounters :: Int -> IO [Integer]
stopCounters int_len =
  counter_call (allocaArray $ fromIntegral int_len) stop_counters int_len

readCounters :: Int -> IO [Integer]
readCounters int_len =
  counter_call (allocaArray $ fromIntegral int_len) read_counters int_len

accumCounters :: [Integer] -> IO [Integer]
accumCounters values =
  counter_call (withArray $ map fromInteger values)
    accum_counters (length values)

-- | Given a list of events to count and an IO operation,
-- run the operation and return the resulting counts.
withCounters :: [EventCode] -> IO a -> IO (a, [Integer])
withCounters wrapped_events action =
  withArrayLen (map unEventCode wrapped_events) $ \int_len events ->
    allocaArray int_len $ \values -> do
      let cint_len = fromIntegral int_len :: CInt
      start_counters events cint_len >>= papiThrow "Error in withCounters: "
      x <- action
      stop_counters values cint_len >>= papiThrow "Error in withCounters: "
      values' <- peekArray int_len values
      return (x, map toInteger values')
