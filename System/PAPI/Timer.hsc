------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Timer
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: timer functions
--

module System.PAPI.Timer (
  getRealCyc
, getRealNsec
, getRealUsec
, getVirtCyc
, getVirtNsec
, getVirtUsec
) where

import Control.Monad
import Foreign.C

#include "papi.h"

foreign import ccall "PAPI_get_real_cyc"  papi_get_real_cyc  :: IO CLLong
foreign import ccall "PAPI_get_real_nsec" papi_get_real_nsec :: IO CLLong
foreign import ccall "PAPI_get_real_usec" papi_get_real_usec :: IO CLLong
foreign import ccall "PAPI_get_virt_cyc"  papi_get_virt_cyc  :: IO CLLong
foreign import ccall "PAPI_get_virt_nsec" papi_get_virt_nsec :: IO CLLong
foreign import ccall "PAPI_get_virt_usec" papi_get_virt_usec :: IO CLLong

getRealCyc, getRealNsec, getRealUsec :: IO Integer
getRealCyc  = liftM toInteger papi_get_real_cyc
getRealNsec = liftM toInteger papi_get_real_nsec
getRealUsec = liftM toInteger papi_get_real_usec

getVirtCyc, getVirtNsec, getVirtUsec :: IO Integer
getVirtCyc  = liftM toInteger papi_get_virt_cyc
getVirtNsec = liftM toInteger papi_get_virt_nsec
getVirtUsec = liftM toInteger papi_get_virt_usec
