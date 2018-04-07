------------------------------------------------------------------------
-- |
-- Module   : System.PAPI
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library
--

module System.PAPI
( module System.PAPI.Event
, module System.PAPI.HighLevel
, module System.PAPI.Init
, module System.PAPI.Options
, module System.PAPI.Timer
, module System.PAPI.EventSet
, module System.PAPI.Run
, module System.PAPI.Error
) where

-- Events
import System.PAPI.Event

-- High-Level API
import System.PAPI.HighLevel

-- Initialization of the Low-Level API
import System.PAPI.Init

-- Building EventSets
import System.PAPI.EventSet

-- Running EventSets
import System.PAPI.Run

-- Getting and Setting Options
import System.PAPI.Options

-- PAPI Timers
import System.PAPI.Timer

-- PAPI Error Handling
import System.PAPI.Error

{-
-- ** Advanced PAPI Features
-- * Multiplexing
import System.PAPI.Multiplex
  PAPI_multiplex_init

-- * Threads
import System.PAPI.Thread
  -- Thread Definitions
  PAPI_thread_init
  PAPI_thread_id
  PAPI_list_threads

  PAPI_register_thread
  PAPI_unregister_thread
  PAPI_get_thr_specific
  PAPI_set_thr_specific

  PAPI_attach
  PAPI_detach

-- Locks
import System.PAPI.Locks
  -- Locking Definitions
  PAPI_lock
  PAPI_unlock

-- Overflow
import System.PAPI.Overflow
  -- Overflow Definitions
  PAPI_overflow
  PAPI_get_overflow_event_index

-- Statistical Profiling
import System.PAPI.Profil
  -- Profile Definitions
  PAPI_profil
  PAPI_sprofil
-}
