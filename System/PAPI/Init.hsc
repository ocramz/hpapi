------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Init
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: library initialization
--

module System.PAPI.Init(
  Version(..)
, papiVersion
, papiVerCurrent
, Initialized(..)
, isInitialized
, libraryInit
, shutdown
) where

import Control.Monad
import Foreign.C

import System.PAPI.Error

#include "papi.h"

-- Version related info
newtype Version = Version Integer deriving (Eq, Show)
papiVersion, papiVerCurrent :: Version
papiVersion = Version #{const PAPI_VERSION}
papiVerCurrent = Version #{const PAPI_VER_CURRENT}

-- Initialization related stuff
data Initialized = PapiNotInited | PapiLowLevelInited | PapiHighLevelInited
  deriving (Eq, Show)

foreign import ccall "PAPI_is_initialized" papi_is_initialized :: IO CInt
isInitialized :: IO Initialized
isInitialized = do
  x <- papi_is_initialized
  return $ case x of
    #{const PAPI_NOT_INITED} -> PapiNotInited
    #{const PAPI_LOW_LEVEL_INITED} -> PapiLowLevelInited
    #{const PAPI_HIGH_LEVEL_INITED} -> PapiHighLevelInited
    _ -> error $ "Internal Error: Unknown result from papi_is_initialized "
                   ++ show x

foreign import ccall "PAPI_library_init" papi_library_init :: CInt -> IO PapiError
libraryInit :: IO ()
libraryInit = do
  r <- papi_library_init papi_ver_current
       >>= papiThrow "PAPI initialization error: "
  when (r /= papi_ver_current) $ error "PAPI library version mis-match"
  where papi_ver_current = #{const PAPI_VER_CURRENT}

foreign import ccall "PAPI_shutdown" papi_shutdown :: IO ()
shutdown :: IO ()
shutdown = papi_shutdown
