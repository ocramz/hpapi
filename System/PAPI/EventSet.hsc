------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.EventSet
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: event set construction and definition
--

module System.PAPI.EventSet (
  EventSet()
, createEventset
, cleanupEventset
, numEvents
, listEvents
, addEvent
, addEvents
, removeEvent
, removeEvents
) where

import Control.Monad(liftM)
import Foreign
import Foreign.C

import System.PAPI.Event
import System.PAPI.Error

#include "papi.h"
#include "papiStdEventDefs.h"

newtype EventSet = EventSet CInt deriving (Eq, Show, Storable)

foreign import ccall "PAPI_create_eventset" papi_create_eventset
  :: Ptr CInt -> IO ()
createEventset :: IO EventSet
createEventset =
  alloca $ \x -> do
  poke (castPtr x :: Ptr CInt) (#{const PAPI_NULL})
  papi_create_eventset x
  liftM EventSet $ peek x -- Use EventSet explicitly to avoid warning

foreign import ccall "PAPI_cleanup_eventset" papi_cleanup_eventset
  :: EventSet -> IO PapiError
cleanupEventset :: EventSet -> IO ()
cleanupEventset x =
  papi_cleanup_eventset x >>= papiThrow "Error in cleanupEventset: "
  >> return ()

foreign import ccall "PAPI_num_events" papi_num_events
  :: EventSet -> IO PapiError
numEvents :: EventSet -> IO Integer
numEvents x =
  papi_num_events x >>= papiThrow "Error in numEvents: "
  >>= return . toInteger

foreign import ccall "PAPI_list_events" papi_list_events
  :: EventSet -> Ptr EventCode -> Ptr CInt -> IO PapiError
listEvents :: EventSet -> Int -> IO [EventCode]
listEvents eventset n =
  with (fromIntegral n) $ \nptr ->
  allocaArray n $ \events ->
  papi_list_events eventset events nptr >>= papiThrow "Error in listEvents: "
  >> liftM fromIntegral (peek nptr) >>= flip peekArray events

foreign import ccall "PAPI_add_event" papi_add_event
  :: EventSet -> EventCode -> IO PapiError
addEvent :: EventSet -> EventCode -> IO ()
addEvent eventset eventcode =
  papi_add_event eventset eventcode >>= papiThrow "Error in addEvent: "
  >> return ()

foreign import ccall "PAPI_add_events" papi_add_events
  :: EventSet -> Ptr EventCode -> CInt -> IO PapiError
addEvents :: EventSet -> [EventCode] -> IO (Maybe Int)
addEvents eventset eventcodes =
  withArrayLen eventcodes $ \len events ->
  papi_add_events eventset events (fromIntegral len)
  >>= papiThrow "Error in addEvents: "
  >>= \x -> if x == 0
            then return Nothing
            else return $ Just $ fromIntegral x

foreign import ccall "PAPI_remove_event" papi_remove_event
  :: EventSet -> EventCode -> IO PapiError
removeEvent :: EventSet -> EventCode -> IO ()
removeEvent eventset eventcode =
  papi_remove_event eventset eventcode >>= papiThrow "Error in removeEvent: "
  >> return ()

foreign import ccall "PAPI_remove_events" papi_remove_events
  :: EventSet -> Ptr EventCode -> CInt -> IO PapiError
removeEvents :: EventSet -> [EventCode] -> IO (Maybe Int)
removeEvents eventset eventcodes =
  withArrayLen eventcodes $ \len events ->
  papi_remove_events eventset events (fromIntegral len)
  >>= papiThrow "Error in removeEvents: "
  >>= \x -> if x == 0
            then return Nothing
            else return $ Just $ fromIntegral x
