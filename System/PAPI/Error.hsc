------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Error
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: error and return code handling
--

module System.PAPI.Error (
  papiThrow
, papiStrError
, PapiError(..)
, papiErrorOk
, papiErrorEinval
, papiErrorEnomem
, papiErrorEsys
, papiErrorEsbstr
, papiErrorEnosupp
, papiErrorEbug
, papiErrorEnoevnt
, papiErrorEcnflct
, papiErrorEnotrun
, papiErrorEisrun
, papiErrorEnoevst
, papiErrorEnotpreset
, papiErrorEnocntr
, papiErrorEmisc
, papiErrorEperm
, papiErrorEnoinit
, papiErrorEbuf  
) where

import Foreign.C
import Foreign.Ptr

#include "papi.h"

-- Private functions
foreign import ccall errno :: IO CInt
foreign import ccall strerror :: CInt -> CString

foreign import ccall "PAPI_strerror" papi_strerror :: PapiError -> CString

-- Not implemented: PAPI_perror

-- Public functions
papiThrow :: String -> PapiError -> IO CInt
papiThrow _ (PapiError err) | err >= 0 = return err
papiThrow str err = papiStrError err >>= error . (str++)

papiStrError :: PapiError -> IO String
papiStrError err = do
  let cstr = papi_strerror err
  if cstr == nullPtr
    then return $ "Unknown error " ++ show (unPapiError err)
    else do
      papi <- peekCString cstr
      if err /= papiErrorEsys
        then return papi
        else do
          sys <- errno >>= peekCString . strerror
          return $ papi ++ ": " ++ sys

newtype PapiError = PapiError { unPapiError :: CInt } deriving (Eq, Show)
#{enum PapiError, PapiError
, papiErrorOk = PAPI_OK /*No error*/
, papiErrorEinval = PAPI_EINVAL /*Invalid argument*/
, papiErrorEnomem = PAPI_ENOMEM /*Insufficient memory*/
, papiErrorEsys = PAPI_ESYS /*A SystemC library call failed please check errno*/
, papiErrorEsbstr = PAPI_ESBSTR /*Substrate returned an error
                              usually the result of an unimplemented feature*/
, papiErrorEnosupp = PAPI_ENOSUPP /*Access to the counters was lost or interrupted*/
, papiErrorEbug = PAPI_EBUG /*Internal error. please send mail to the developers*/
, papiErrorEnoevnt = PAPI_ENOEVNT /*Hardware Event does not exist*/
, papiErrorEcnflct = PAPI_ECNFLCT /*Hardware Event exists but cannot be counted 
                                   due to counter resource limitations*/
, papiErrorEnotrun = PAPI_ENOTRUN /*No Events or EventSets are currently counting*/
, papiErrorEisrun = PAPI_EISRUN /*Events or EventSets are currently counting*/
, papiErrorEnoevst = PAPI_ENOEVST /* No EventSet Available*/
, papiErrorEnotpreset = PAPI_ENOTPRESET /* Not a Preset Event in argument*/
, papiErrorEnocntr = PAPI_ENOCNTR /* Hardware does not support counters*/
, papiErrorEmisc = PAPI_EMISC /* No clue as to what this error code means*/
, papiErrorEperm = PAPI_EPERM /* You lack the necessary permissions*/
, papiErrorEnoinit = PAPI_ENOINIT /* PAPI hasn't been initialized yet*/
, papiErrorEbuf = PAPI_EBUF /* Buffer size exceeded (usually strings)*/
}
