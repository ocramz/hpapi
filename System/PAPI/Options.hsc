------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Options
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: system options
--

module System.PAPI.Options (
  getExecutableInfo
, getHardwareInfo
, getSubstrateInfo
, getSharedLibInfo
, getDmemInfo
, getMultiplex
, setMultiplex
, setDebug
, setDomain
, setGranularity
, numHwctrs

, getOptAttach
, getOptDetach
, getOptDefMpxNs
, getOptDefItimerNs
, getOptItimer
, getOptMultiplex
, getOptPreload
, getOptDebug
, getOptClockrate
, getOptMaxCpus
, getOptMaxHwCtrs
, getOptDefDomain
, getOptDefGranularity
, getOptGranularity
, getOptShlibInfo
, getOptExeInfo
, getOptHwInfo
, getOptSubstrateInfo
, getDomainOption
, getOptLibVersion

, setOptDetach
, setOptAttach
, setOptDefMpxNs
, setOptDefItimerNs
, setOptDefItimer
, setOptMultiplex
, setOptDebug
, setOptDefDomain
, setOptDomain
, setOptDefGranularity
, setOptGranularity
, setOptDataAddress
, setOptInstrAddress

, DebugLevel()
, debugQuiet
, debugVerbEcont
, debugVerbEstop

, Domain()
, domainUser
, domainKernel
, domainOther
, domainSupervisor
, domainAll
, domainMin
, domainMax

, Granularity()
, granularityThead
, granularityMin
, granularityProc
, granularityProcG
, granularitySys
, granularitySysCpu
, granularityMax

, ThreadId(..)

, ExeInfo(..)
, AddressMap(..)
, HwInfo(..)
, Vendor(..)
, MhInfo(..)
, MhLevel(..)
, MhTLBInfo(..)
, MhCacheInfo(..)
, MhType(..)
, MhCacheType(..)
, MhWritePolicy(..)
, MhReplacementPolicy(..)
, SubstrateInfo(..)
, ShlibInfo(..)
, DmemInfo(..)
, ItimerOption(..)
, ItimerFlags(..)
, PreloadInfo(..)
, DebugHandler
, DebugOption(..)
, MultiplexOption(..)
, MultiplexFlags(..)

) where

import Control.Monad
import Foreign
import Foreign.C

import System.PAPI.Init (Version(..))
import System.PAPI.EventSet
import System.PAPI.Error
import System.PAPI.Util

-- Helpers

#include "papi.h"
#include "template-bit-hsc.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-----------------------
-- general info functions

getObjectInfo :: (Storable a) => IO (Ptr a) -> String -> IO a
getObjectInfo getter str = getter >>= nullThrow ("Error in " ++ str) >>= peek

--------

foreign import ccall "PAPI_get_executable_info" papi_get_executable_info
  :: IO (Ptr ExeInfo)
getExecutableInfo :: IO ExeInfo
getExecutableInfo = getObjectInfo papi_get_executable_info "getExecutableInfo"

foreign import ccall "PAPI_get_hardware_info" papi_get_hardware_info
  :: IO (Ptr HwInfo)
getHardwareInfo :: IO HwInfo
getHardwareInfo = getObjectInfo papi_get_hardware_info "getHardwareInfo"

foreign import ccall "PAPI_get_substrate_info" papi_get_substrate_info
  :: IO (Ptr SubstrateInfo)
getSubstrateInfo :: IO SubstrateInfo
getSubstrateInfo = getObjectInfo papi_get_substrate_info "getSubstrateInfo"

foreign import ccall "PAPI_get_shared_lib_info" papi_get_shared_lib_info
  :: IO (Ptr ShlibInfo)
getSharedLibInfo :: IO ShlibInfo
getSharedLibInfo = getObjectInfo papi_get_shared_lib_info "getSharedLibInfo"

foreign import ccall "PAPI_get_dmem_info" papi_get_dmem_info
  :: Ptr DmemInfo -> IO PapiError
getDmemInfo :: IO DmemInfo
getDmemInfo =
  alloca $ \ptr ->
  papi_get_dmem_info ptr >>= papiThrow "Error in getDmemInfo: " >>
  peek ptr

foreign import ccall "PAPI_get_multiplex" papi_get_multiplex
  :: EventSet -> IO PapiError
getMultiplex :: EventSet -> IO Bool
getMultiplex event =
  papi_get_multiplex event >>= papiThrow "Error in getMultiplex: " >>=
    return . toBool

foreign import ccall "PAPI_set_multiplex" papi_set_multiplex
  :: EventSet -> IO PapiError
setMultiplex :: EventSet -> IO ()
setMultiplex event =
  papi_set_multiplex event >>= papiThrow "Error in setMultiplex: " >> return ()

foreign import ccall "PAPI_set_debug" papi_set_debug
  :: DebugLevel -> IO PapiError
setDebug :: DebugLevel -> IO ()
setDebug debug =
  papi_set_debug debug >>= papiThrow "Error in setDebug: " >> return ()

foreign import ccall "PAPI_set_domain" papi_set_domain
  :: Domain -> IO PapiError
setDomain :: Domain -> IO ()
setDomain domain =
  papi_set_domain domain >>= papiThrow "Error in setDomain: " >> return ()

foreign import ccall "PAPI_set_granularity" papi_set_granularity
  :: Granularity -> IO PapiError
setGranularity :: Granularity -> IO ()
setGranularity granularity =
  papi_set_granularity granularity >>= papiThrow "Error in setGranularity: "
  >> return ()

foreign import ccall "PAPI_num_hwctrs" papi_num_hwctrs :: IO PapiError
numHwctrs :: IO Integer 
numHwctrs =
  papi_num_hwctrs >>= papiThrow "Error in numHwctrs: " >>= return . toInteger

-----------------
-- getOpt functions

foreign import ccall "PAPI_get_opt" papi_get_opt ::
  CInt -> Ptr Option -> IO PapiError

getPtrOpt :: CInt -> String
  -> (Ptr Option -> IO t)
  -> (CInt -> Ptr Option -> IO a)
  -> IO a
getPtrOpt opt name poker peeker =
  alloca $ \ptr -> do
  poker ptr
  result <- papi_get_opt opt ptr >>=
            papiThrow ("Error in getOpt" ++ name ++ ": ")
  peeker result ptr

getPurePtrOpt :: CInt -> String -> (Ptr Option -> IO a) -> IO a
getPurePtrOpt opt name peeker =
  getPtrOpt opt name (const $ return ()) (const peeker)

getReturnOpt :: CInt -> String -> (CInt -> a) -> IO a
getReturnOpt opt name ctor =
  papi_get_opt opt nullPtr >>=
  papiThrow ("Error in getOpt" ++ name ++ ": ") >>=
  return . ctor

------------

getOptAttach :: EventSet -> IO (Bool, ThreadId)
getOptAttach event = getPtrOpt #{const PAPI_ATTACH} "Attach" poker peeker where
  poker ptr = #{poke PAPI_option_t, attach.eventset} ptr event
  peeker res ptr = do
    tid <- #{peek PAPI_option_t, attach.tid} ptr
    return (toBool res, tid)

getOptDetach :: EventSet -> IO (Bool, ThreadId)
getOptDetach event = getPtrOpt #{const PAPI_DETACH} "Detach" poker peeker where
  poker ptr = #{poke PAPI_option_t, attach.eventset} ptr event
  peeker res ptr = do
    tid <- #{peek PAPI_option_t, attach.tid} ptr
    return (toBool res, tid)

getOptDefMpxNs :: IO Integer
getOptDefMpxNs = getPurePtrOpt #{const PAPI_DEF_MPX_NS} "DefMpxNs"
  (liftM fromCInt . #{peek PAPI_option_t, multiplex.ns})

getOptDefItimerNs :: IO Integer
getOptDefItimerNs = getPurePtrOpt #{const PAPI_DEF_ITIMER_NS} "DefItimerNs"
  (liftM fromCInt . #{peek PAPI_option_t, itimer.ns})

getOptItimer :: IO ItimerOption
getOptItimer = getPurePtrOpt #{const PAPI_DEF_ITIMER} "Itimer"
  #{peek PAPI_option_t, itimer}

getOptMultiplex :: EventSet -> IO (Bool, Integer, MultiplexFlags)
getOptMultiplex event =
  getPtrOpt #{const PAPI_MULTIPLEX} "Multiplex" poker peeker where
  poker ptr = #{poke PAPI_option_t, multiplex.eventset} ptr event
  peeker res ptr = do
    ns <- liftM fromCInt $ #{peek PAPI_option_t, multiplex.ns} ptr
    flags <- #{peek PAPI_option_t, multiplex.flags} ptr
    return (toBool res, ns, flags)

getOptPreload :: IO PreloadInfo
getOptPreload = getPurePtrOpt #{const PAPI_PRELOAD} "Preload"
  #{peek PAPI_option_t, preload}

getOptDebug :: IO DebugOption
getOptDebug = getPurePtrOpt #{const PAPI_DEBUG} "Debug"
  #{peek PAPI_option_t, debug}

getOptClockrate :: IO Integer
getOptClockrate = getReturnOpt #{const PAPI_CLOCKRATE} "Clockrate" fromCInt

getOptMaxCpus :: IO Integer
getOptMaxCpus = getReturnOpt #{const PAPI_MAX_CPUS} "MaxCpus" fromCInt

getOptMaxHwCtrs :: IO Integer
getOptMaxHwCtrs = getReturnOpt #{const PAPI_MAX_HWCTRS} "MaxHwCtrs" fromCInt

getOptDefDomain :: IO Domain
getOptDefDomain = getReturnOpt #{const PAPI_DEFDOM} "DefDomain" Domain

getOptDefGranularity :: IO Granularity
getOptDefGranularity =
  getReturnOpt #{const PAPI_DEFGRN} "DefGranularity" Granularity

getOptGranularity :: EventSet -> IO Granularity
getOptGranularity event =
  getPtrOpt #{const PAPI_GRANUL} "Granularity" poker peeker where
  poker ptr = #{poke PAPI_option_t, granularity.eventset} ptr event
  peeker _ ptr = #{peek PAPI_option_t, granularity.granularity} ptr

getOptShlibInfo :: IO ShlibInfo
getOptShlibInfo = getPurePtrOpt #{const PAPI_SHLIBINFO} "ShlibInfo"
  (peek <=< #{peek PAPI_option_t, shlib_info})

getOptExeInfo :: IO ExeInfo
getOptExeInfo = getPurePtrOpt #{const PAPI_EXEINFO} "ExeInfo"
  (peek <=< #{peek PAPI_option_t, exe_info})

getOptHwInfo :: IO HwInfo
getOptHwInfo = getPurePtrOpt #{const PAPI_HWINFO} "HwInfo"
  (peek <=< #{peek PAPI_option_t, hw_info})

getOptSubstrateInfo :: IO SubstrateInfo
getOptSubstrateInfo = getPurePtrOpt #{const PAPI_SUBSTRATEINFO} "SubstrateInfo"
  (peek <=< #{peek PAPI_option_t, sub_info})

getDomainOption :: EventSet -> IO Domain
getDomainOption event =
  getPtrOpt #{const PAPI_DOMAIN} "Domain" poker peeker where
  poker ptr = #{poke PAPI_option_t, domain.eventset} ptr event
  peeker _ ptr = #{peek PAPI_option_t, domain.domain} ptr

getOptLibVersion :: IO Version
getOptLibVersion =
  getReturnOpt #{const PAPI_LIB_VERSION} "LibVersion" (Version . fromCInt)

-----------------
-- setOpt functions

foreign import ccall "PAPI_set_opt" papi_set_opt ::
  CInt -> Ptr Option -> IO PapiError

setPtrOpt :: CInt -> String
  -> (Ptr Option -> IO ())
  -> (Ptr Option -> IO a) -> IO a
setPtrOpt opt name poker peeker =
  alloca $ \ptr -> do
  poker ptr
  papi_set_opt opt ptr >>= papiThrow ("Error in setOpt" ++ name ++ ": ")
  peeker ptr

setPurePtrOpt :: CInt -> String -> (Ptr Option -> IO ()) -> IO ()
setPurePtrOpt opt name poker = setPtrOpt opt name poker (const $ return ())

--------

setOptDetach :: EventSet -> IO ()
setOptDetach event = setPurePtrOpt #{const PAPI_DETACH} "Detach" poker where
  poker ptr = #{poke PAPI_option_t, attach.eventset} ptr event

setOptAttach :: EventSet -> ThreadId -> IO ()
setOptAttach event tid =
  setPurePtrOpt #{const PAPI_DETACH} "Attach" poker where
  poker ptr = do
    #{poke PAPI_option_t, attach.eventset} ptr event
    #{poke PAPI_option_t, attach.tid} ptr tid

setOptDefMpxNs :: Integer -> IO Integer
setOptDefMpxNs ns =
  setPtrOpt #{const PAPI_DEF_MPX_NS} "DefMpxNs" poker peeker where
  poker ptr = #{poke PAPI_option_t, multiplex.ns} ptr (toCInt ns)
  peeker = liftM fromCInt . #{peek PAPI_option_t, multiplex.ns}

setOptDefItimerNs :: Integer -> IO Integer
setOptDefItimerNs ns =
  setPtrOpt #{const PAPI_DEF_ITIMER_NS} "DefItimerNs" poker peeker where
  poker ptr = #{poke PAPI_option_t, itimer.ns} ptr (toCInt ns)
  peeker = liftM fromCInt . #{peek PAPI_option_t, itimer.ns}

setOptDefItimer :: ItimerOption -> IO ()
setOptDefItimer itimer =
  setPurePtrOpt #{const PAPI_DEF_ITIMER} "DefItimer" poker where
  poker ptr = #{poke PAPI_option_t, itimer} ptr itimer

setOptMultiplex :: MultiplexOption -> IO Integer
setOptMultiplex multiplex =
  setPtrOpt #{const PAPI_MULTIPLEX} "Multiplex" poker peeker where
  poker ptr = #{poke PAPI_option_t, multiplex} ptr multiplex
  peeker = liftM fromCInt . #{peek PAPI_option_t, multiplex.ns}

setOptDebug :: DebugOption -> IO ()
setOptDebug debug = setPurePtrOpt #{const PAPI_DEBUG} "Debug" poker where
  poker ptr = #{poke PAPI_option_t, debug} ptr debug

setOptDefDomain :: Domain -> IO ()
setOptDefDomain domain =
  setPurePtrOpt #{const PAPI_DEFDOM} "DefDomain" poker where
  poker ptr = #{poke PAPI_option_t, defdomain.domain} ptr domain

setOptDomain :: EventSet -> Domain -> IO ()
setOptDomain event domain =
  setPurePtrOpt #{const PAPI_DOMAIN} "Domain" poker where
  poker ptr = do
    #{poke PAPI_option_t, domain.eventset} ptr event
    #{poke PAPI_option_t, domain.domain} ptr domain

setOptDefGranularity :: Granularity -> IO ()
setOptDefGranularity granularity =
  setPurePtrOpt #{const PAPI_DEFGRN} "DefGranularity" poker where
  poker ptr = #{poke PAPI_option_t, defgranularity} ptr granularity

setOptGranularity :: EventSet -> Granularity -> IO ()
setOptGranularity event granularity =
  setPurePtrOpt #{const PAPI_GRANUL} "Granularity" poker where
  poker ptr = do
    #{poke PAPI_option_t, granularity.eventset} ptr event
    #{poke PAPI_option_t, granularity.granularity} ptr granularity

setOptDataAddress :: EventSet -> Addr -> Addr -> IO (Integer, Integer)
setOptDataAddress event start end = setPtrOpt #{const PAPI_DATA_ADDRESS} "DataAddress"
  poker peeker where
  poker ptr = do
    #{poke PAPI_option_t, addr.eventset} ptr event
    #{poke PAPI_option_t, addr.start} ptr start
    #{poke PAPI_option_t, addr.end} ptr end
  peeker ptr = do
    start_off <- liftM fromCInt $ #{peek PAPI_option_t, addr.start_off} ptr
    end_off <- liftM fromCInt $ #{peek PAPI_option_t, addr.end_off} ptr
    return (start_off, end_off)

setOptInstrAddress :: EventSet -> Addr -> Addr -> IO (Integer, Integer)
setOptInstrAddress event start end = setPtrOpt #{const PAPI_INSTR_ADDRESS} "InstrAddress"
  poker peeker where
  poker ptr = do
    #{poke PAPI_option_t, addr.eventset} ptr event
    #{poke PAPI_option_t, addr.start} ptr start
    #{poke PAPI_option_t, addr.end} ptr end
  peeker ptr = do
    start_off <- liftM fromCInt $ #{peek PAPI_option_t, addr.start_off} ptr
    end_off <- liftM fromCInt $ #{peek PAPI_option_t, addr.end_off} ptr
    return (start_off, end_off)

{-
-- General information requests -- Get only

data Clockrate = Clockrate
data MaxCpus = MaxCpus
data MaxHwctrs = MaxHwctrs
data Exeinfo = Exeinfo
data Hwinfo = Hwinfo
data Shlibinfo = Shlibinfo
data Substrateinfo = Substrateinfo
data LibVersion = LibVersion
data Preload = Preload

-- Defaults for the global library

data Defdom = Defdom
data Defgrn = Defgrn
data Debug = Debug

-- Multiplexing control

data Multiplex = Multiplex
data MaxMpxCtrs = MaxMpxCtrs -- Get only
data DefMpxUsec = DefMpxUsec

-- Manipulating individual event sets

data Attach = Attach
data Detach = Detach
data Domain = Domain -- only some platforms
data Granul = Granul -- unimplemented

-- Platform Specific Options

data DataAddress = DataAddress -- Set only. Itanium
data InstrAddress = InstrAddress -- Set only. Itanium

-- Missing Options

data EdgeDetect = EdgeDetect -- <not implemented>
data Invert = Invert -- <not implemented>
data Profil = Profil -- <not implemented>

-- Itanium Only
data DefMpxNs = DefMpxNs
data DefItimerNs = DefItimerNs -- Same as DefMpxNs
-}

----------------

newtype DebugLevel = DebugLevel CInt deriving (Eq, Show, Storable)
#{enum DebugLevel, DebugLevel
, debugQuiet = PAPI_QUIET
, debugVerbEcont = PAPI_VERB_ECONT
, debugVerbEstop = PAPI_VERB_ESTOP
}

newtype Domain = Domain CInt deriving (Eq, Show, Storable)
#{enum Domain, Domain
, domainUser       = PAPI_DOM_USER
, domainKernel     = PAPI_DOM_KERNEL
, domainOther      = PAPI_DOM_OTHER
, domainSupervisor = PAPI_DOM_SUPERVISOR
, domainAll        = PAPI_DOM_ALL
, domainMin        = PAPI_DOM_MIN
, domainMax        = PAPI_DOM_MAX
}

newtype Granularity = Granularity CInt deriving (Eq, Show, Storable)
#{enum Granularity, Granularity
, granularityThead  = PAPI_GRN_THR
, granularityMin    = PAPI_GRN_MIN
, granularityProc   = PAPI_GRN_PROC
, granularityProcG  = PAPI_GRN_PROCG
, granularitySys    = PAPI_GRN_SYS
, granularitySysCpu = PAPI_GRN_SYS_CPU
, granularityMax    = PAPI_GRN_MAX
}

-- TODO: move to threading when that module gets created
newtype ThreadId = ThreadId { unThreadId :: CInt }
  deriving (Eq, Show, Storable)

----------------

data Option

instance Storable Option where
  sizeOf _ = #{size PAPI_option_t}
  alignment _ = #{alignment PAPI_option_t}

  peek = error "peek on System.PAPI.Option not permitted"
  poke = error "poke on System.PAPI.Option not permitted"

----------------

data ExeInfo = ExeInfo {
  exeFullname :: String,
  exeAddressInfo :: AddressMap
  } deriving (Eq, Show)

instance Storable ExeInfo where
  sizeOf _ = #{size PAPI_exe_info_t}
  alignment _ = #{alignment PAPI_exe_info_t}
  peek = const (return ExeInfo)
    `ap2` peekCString . #{ptr PAPI_exe_info_t, fullname}
    `ap2` #{peek PAPI_exe_info_t, address_info}
  poke ptr value = do
    pokeCString (#{ptr PAPI_exe_info_t, fullname} ptr)
                (exeFullname value) #{const PAPI_HUGE_STR_LEN-1}
    poke (#{ptr PAPI_exe_info_t, address_info} ptr)
         (exeAddressInfo value)
    return ()

data AddressMap = AddressMap {
  addrMapName :: String,
  addrMapTextStart, addrMapTextEnd :: Addr,
  addrMapDataStart, addrMapDataEnd :: Addr,
  addrMapBssStart, addrMapBssEnd :: Addr
  } deriving (Eq, Show)

instance Storable AddressMap where
  sizeOf _ = #{size PAPI_address_map_t}
  alignment _ = #{alignment PAPI_address_map_t}
  peek = const (return AddressMap)
    `ap2` peekCString . #{ptr PAPI_address_map_t, name}
    `ap2` #{peek PAPI_address_map_t, text_start}
    `ap2` #{peek PAPI_address_map_t, text_end}
    `ap2` #{peek PAPI_address_map_t, data_start}
    `ap2` #{peek PAPI_address_map_t, data_end}
    `ap2` #{peek PAPI_address_map_t, bss_start}
    `ap2` #{peek PAPI_address_map_t, bss_end}

  poke ptr value = do
    pokeCString (#{ptr PAPI_address_map_t, name} ptr)
                (addrMapName value) #{const PAPI_HUGE_STR_LEN-1}
    #{poke PAPI_address_map_t, text_start} ptr $ addrMapTextStart value
    #{poke PAPI_address_map_t, text_end}   ptr $ addrMapTextEnd value
    #{poke PAPI_address_map_t, data_start} ptr $ addrMapDataStart value
    #{poke PAPI_address_map_t, data_end}   ptr $ addrMapDataEnd value
    #{poke PAPI_address_map_t, bss_start}  ptr $ addrMapBssStart value
    #{poke PAPI_address_map_t, bss_end}    ptr $ addrMapBssEnd value
    return ()

----------------

data HwInfo = HwInfo {
  hwNcpu :: Integer,
  hwNnodes :: Integer,
  hwTotalcpus :: Integer,
  hwVendor :: Vendor,
  hwVendorString :: String,
  hwModel :: Integer,
  hwModelString :: String,
  hwRevision :: Float,
  hwMhz :: Float,
  hwClockMhz :: Integer,
  hwMemHierarchy :: MhInfo
  } deriving (Eq, Show)

instance Storable HwInfo where
  sizeOf _ = #{size PAPI_hw_info_t}
  alignment _ = #{alignment PAPI_hw_info_t}
        
  peek = const (return HwInfo)
    `ap2` liftM fromCInt . #{peek PAPI_hw_info_t, ncpu}
    `ap2` liftM fromCInt . #{peek PAPI_hw_info_t, nnodes}
    `ap2` liftM fromCInt . #{peek PAPI_hw_info_t, totalcpus}
    `ap2` #{peek PAPI_hw_info_t, vendor}
    `ap2` peekCString . #{ptr PAPI_hw_info_t, vendor_string}
    `ap2` liftM fromCInt . #{peek PAPI_hw_info_t, model}
    `ap2` peekCString . #{ptr PAPI_hw_info_t, model_string}
    `ap2` #{peek PAPI_hw_info_t, revision}
    `ap2` #{peek PAPI_hw_info_t, mhz}
    `ap2` liftM fromCInt . #{peek PAPI_hw_info_t, clock_mhz}
    `ap2` #{peek PAPI_hw_info_t, mem_hierarchy}

  poke ptr value = do
    #{poke PAPI_hw_info_t, ncpu} ptr $ toCInt $ hwNcpu value
    #{poke PAPI_hw_info_t, nnodes} ptr $ toCInt $ hwNnodes value
    #{poke PAPI_hw_info_t, totalcpus} ptr $ toCInt $ hwTotalcpus value
    poke (#{ptr PAPI_hw_info_t, vendor} ptr) $ hwVendor value
    pokeCString (#{ptr PAPI_hw_info_t, vendor_string} ptr)
                (hwVendorString value) #{const PAPI_MAX_STR_LEN-1}
    #{poke PAPI_hw_info_t, model} ptr $ toCInt $ hwModel value
    pokeCString (#{ptr PAPI_hw_info_t, model_string} ptr)
                (hwModelString value) #{const PAPI_MAX_STR_LEN-1}
    #{poke PAPI_hw_info_t, revision} ptr $ hwRevision value
    #{poke PAPI_hw_info_t, mhz} ptr $ hwMhz value
    #{poke PAPI_hw_info_t, clock_mhz} ptr $ toCInt $ hwClockMhz value
    poke (#{ptr PAPI_hw_info_t, mem_hierarchy} ptr) $ hwMemHierarchy value

----

data Vendor
  = VendorUnknown
  | VendorIntel
  | VendorAmd
  | VendorCyrix
  | VendorIbm
  | VendorMips
  | VendorCray
  | VendorSun
  | VendorFreescale
  | VendorSicortex
  deriving (Eq, Show)

instance Storable Vendor where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)

  peek x = do
    x' <- peek (castPtr x :: Ptr CInt)
    case x' of
      #{const PAPI_VENDOR_UNKNOWN}   -> return VendorUnknown
      #{const PAPI_VENDOR_INTEL}     -> return VendorIntel
      #{const PAPI_VENDOR_AMD}       -> return VendorAmd
      #{const PAPI_VENDOR_CYRIX}     -> return VendorCyrix
      #{const PAPI_VENDOR_IBM}       -> return VendorIbm
      #{const PAPI_VENDOR_MIPS}      -> return VendorMips
      #{const PAPI_VENDOR_CRAY}      -> return VendorCray
      #{const PAPI_VENDOR_SUN}       -> return VendorSun
      #{const PAPI_VENDOR_FREESCALE} -> return VendorFreescale
      #{const PAPI_VENDOR_SICORTEX}  -> return VendorSicortex
      _                              -> error "Internal error: Unknown vendor"

  poke ptr VendorUnknown =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_UNKNOWN}
  poke ptr VendorIntel =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_INTEL}
  poke ptr VendorAmd =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_AMD}
  poke ptr VendorCyrix =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_CYRIX}
  poke ptr VendorIbm =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_IBM}
  poke ptr VendorMips =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_MIPS}
  poke ptr VendorCray =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_CRAY}
  poke ptr VendorSun =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_SUN}
  poke ptr VendorFreescale =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_FREESCALE}
  poke ptr VendorSicortex =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_VENDOR_SICORTEX}

----

newtype MhInfo = MhInfo [MhLevel] deriving (Eq, Show)

instance Storable MhInfo where
  sizeOf _ = #{size PAPI_mh_info_t}
  alignment _ = #{alignment PAPI_mh_info_t}

  peek ptr = do
    levels <- #{peek PAPI_mh_info_t, levels} ptr :: IO CInt
    let levelArray = #{ptr PAPI_mh_info_t, level} ptr
    liftM MhInfo $ mapM (peekElemOff levelArray) [0..fromIntegral levels-1]

  poke ptr (MhInfo values) = do
    let values' = take #{const PAPI_MAX_MEM_HIERARCHY_LEVELS} values
    let len = length values'
    #{poke PAPI_mh_info_t, levels} ptr len
    let arrayPtr = #{ptr PAPI_mh_info_t, level} ptr
    mapM_ (uncurry $ pokeElemOff arrayPtr) $ zip [0..] values'

----

data MhLevel = MhLevel {
  mhLevelTLB :: [MhTLBInfo],
  mhLevelCache :: [MhCacheInfo]
  } deriving (Eq, Show)

instance Storable MhLevel where
  sizeOf _ = #{size PAPI_mh_level_t}
  alignment _ = #{alignment PAPI_mh_level_t}

  peek ptr = do
    tlb <- mapM (peekElemOff tlbArray) [0..mh_max_levels-1]
    cache <- mapM (peekElemOff cacheArray) [0..mh_max_levels-1]
    return $ MhLevel tlb cache
    where mh_max_levels = #{const PAPI_MH_MAX_LEVELS}
          tlbArray = #{ptr PAPI_mh_level_t, tlb} ptr
          cacheArray = #{ptr PAPI_mh_level_t, cache} ptr

  poke ptr (MhLevel tlb cache) = do
    mapM_ (uncurry $ pokeElemOff tlbPtr) $
      take mh_max_levels (zip [0..] (tlb ++ repeat emptyTLB))
    mapM_ (uncurry $ pokeElemOff cachePtr) $
      take mh_max_levels (zip [0..] (cache ++ repeat emptyCache))
    where tlbPtr = #{ptr PAPI_mh_level_t, tlb} ptr
          cachePtr = #{ptr PAPI_mh_level_t, cache} ptr
          emptyTLB = MhTLBInfo emptyMhType 0 0
          emptyCache = MhCacheInfo emptyMhType 0 0 0 0
          mh_max_levels = #{const PAPI_MH_MAX_LEVELS}
          emptyMhType = MhType (MhCacheType False False False False)
                               MhTypeWriteThrough
                               MhReplacementPolicyUnknown

----

data MhTLBInfo = MhTLBInfo {
  mhTLBType :: MhType,
  mhTLBNumEntries :: Integer,
  mhTLBAssociativity :: Integer
  } deriving (Eq, Show)

instance Storable MhTLBInfo where
  sizeOf _ = #{size PAPI_mh_tlb_info_t}
  alignment _ = #{alignment PAPI_mh_tlb_info_t}

  peek = const (return MhTLBInfo)
    `ap2` #{peek PAPI_mh_tlb_info_t, type}
    `ap2` liftM fromCInt . #{peek PAPI_mh_tlb_info_t, num_entries}
    `ap2` liftM fromCInt . #{peek PAPI_mh_tlb_info_t, associativity}

  poke ptr value = do
    poke (#{ptr PAPI_mh_tlb_info_t, type} ptr) (mhTLBType value)
    #{poke PAPI_mh_tlb_info_t, num_entries} ptr $
      toCInt $ mhTLBNumEntries value
    #{poke PAPI_mh_tlb_info_t, associativity} ptr $
      toCInt $ mhTLBAssociativity value

----

data MhCacheInfo = MhCacheInfo {
  mhCacheType :: MhType,
  mhCacheSize :: Integer,
  mhCacheLineSize :: Integer,
  mhCacheNumLines :: Integer,
  mhCacheAssociativity :: Integer
  } deriving (Eq, Show)

instance Storable MhCacheInfo where
  sizeOf _ = #{size PAPI_mh_cache_info_t}
  alignment _ = #{alignment PAPI_mh_cache_info_t}

  peek = const (return MhCacheInfo)
    `ap2` #{peek PAPI_mh_cache_info_t, type}
    `ap2` liftM fromCInt . #{peek PAPI_mh_cache_info_t, size}
    `ap2` liftM fromCInt . #{peek PAPI_mh_cache_info_t, line_size}
    `ap2` liftM fromCInt . #{peek PAPI_mh_cache_info_t, num_lines}
    `ap2` liftM fromCInt . #{peek PAPI_mh_cache_info_t, associativity}

  poke ptr value = do
    poke (#{ptr PAPI_mh_cache_info_t, type} ptr) (mhCacheType value)
    #{poke PAPI_mh_cache_info_t, size} ptr $ toCInt $ mhCacheSize value
    #{poke PAPI_mh_cache_info_t, line_size} ptr $
      toCInt $ mhCacheLineSize value
    #{poke PAPI_mh_cache_info_t, num_lines} ptr $
      toCInt $ mhCacheNumLines value
    #{poke PAPI_mh_cache_info_t, associativity} ptr $
      toCInt $ mhCacheAssociativity value

----

data MhType = MhType {
  mhTypeCacheType :: MhCacheType,
  mhTypeWritePolicy :: MhWritePolicy,
  mhTypeReplacementPolicy :: MhReplacementPolicy
  } deriving (Eq, Show)

data MhCacheType = MhCacheType {
  mhTypeInst :: Bool,
  mhTypeData :: Bool,
  mhTypeVector :: Bool,
  mhTypeTrace :: Bool
} deriving (Eq, Show)

data MhWritePolicy
  = MhTypeWriteThrough
  | MhTypeWriteBack
  deriving (Eq, Show)

data MhReplacementPolicy
  = MhReplacementPolicyUnknown
  | MhReplacementPolicyLRU
  | MhReplacementPolicyPseudoLRU
  deriving (Eq, Show)

instance Storable MhType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
    cint <- peek (castPtr ptr :: Ptr CInt)
    return $ MhType
      (MhCacheType
        (toBool $ cint .&. #{const PAPI_MH_TYPE_INST})
        (toBool $ cint .&. #{const PAPI_MH_TYPE_DATA})
        (toBool $ cint .&. #{const PAPI_MH_TYPE_VECTOR})
        (toBool $ cint .&. #{const PAPI_MH_TYPE_TRACE}))
      (case cint .&. #{const PAPI_MH_CACHE_WRITE_POLICY(-1)} of
        #{const PAPI_MH_TYPE_WT} -> MhTypeWriteThrough
        #{const PAPI_MH_TYPE_WB} -> MhTypeWriteBack
        _ -> error "Internal error: Unknown cache write policy")
      (case cint .&. #{const PAPI_MH_CACHE_REPLACEMENT_POLICY(-1)} of
        #{const PAPI_MH_TYPE_UNKNOWN}    -> MhReplacementPolicyUnknown
        #{const PAPI_MH_TYPE_LRU}        -> MhReplacementPolicyLRU
        #{const PAPI_MH_TYPE_PSEUDO_LRU} -> MhReplacementPolicyPseudoLRU
        _ -> error "Internal error: Unknown replacement policy")

  poke ptr (MhType cache write replacement) =
    poke (castPtr ptr :: Ptr CInt) $
      toMask (mhTypeInst   cache) #{const PAPI_MH_TYPE_INST} .|.
      toMask (mhTypeData   cache) #{const PAPI_MH_TYPE_DATA} .|.
      toMask (mhTypeVector cache) #{const PAPI_MH_TYPE_EMPTY} .|.
      toMask (mhTypeTrace  cache) #{const PAPI_MH_TYPE_EMPTY} .|.
      (case write of
        MhTypeWriteThrough -> #{const PAPI_MH_TYPE_WT}
        MhTypeWriteBack    -> #{const PAPI_MH_TYPE_WB}) .|.
      (case replacement of
        MhReplacementPolicyUnknown   -> #{const PAPI_MH_TYPE_UNKNOWN}
        MhReplacementPolicyLRU       -> #{const PAPI_MH_TYPE_LRU}
        MhReplacementPolicyPseudoLRU -> #{const PAPI_MH_TYPE_PSEUDO_LRU})


----------------

data SubstrateInfo = SubstrateInfo {
  substrateName :: String,
  substrateVersion :: String,
  substrateSupportVersion :: String,
  substrateKernelVersion :: String,
  substrateNumCntrs :: Integer,
  substrateNumMpxCntrs :: Integer,
  substrateNumPresetEvents :: Integer,
  substrateNumNativeEvents :: Integer,
  substrateDefaultDomain :: Integer,
  substrateAvailableDomains :: Integer,
  substrateDefaultGranularity :: Integer,
  substrateAvailableGranularities :: Integer,
  substrateItimerSig :: Integer,
  substrateItimerNum :: Integer,
  substrateItimerNs :: Integer,
  substrateItimerResNs :: Integer,
  substrateHardwareIntrSig :: Integer,
  substrateClockTicks :: Integer,
  substrateOpcodeMatchWidth :: Integer,
  substrateHardwareIntr :: Bool,
  substratePreciseIntr :: Bool,
  substratePosix1bTimers :: Bool,
  substrateKernelProfile :: Bool,
  substrateKernelMultiplex :: Bool,
  substrateDataAddressRange :: Bool,
  substrateInstrAddressRange :: Bool,
  substrateFastCounterRead :: Bool,
  substrateFastRealTimer :: Bool,
  substrateFastVirtualTimer :: Bool,
  substrateAttach :: Bool,
  substrateAttachMustPtrace :: Bool,
  substrateEdgeDetect :: Bool,
  substrateInvert :: Bool,
  substrateProfileEar :: Bool,
  substrateCntrGroups :: Bool,
  substrateCntrUmasks :: Bool,
  substrateCntrIEAREvents :: Bool,
  substrateCntrDEAREvents :: Bool,
  substrateCntrOPCMEvents :: Bool
  } deriving (Eq, Show)

instance Storable SubstrateInfo where
  sizeOf _ = #{size PAPI_substrate_info_t}
  alignment _ = #{alignment PAPI_substrate_info_t}

  peek = const (return SubstrateInfo)
    `ap2` peekCString . #{ptr PAPI_substrate_info_t, name}
    `ap2` peekCString . #{ptr PAPI_substrate_info_t, version}
    `ap2` peekCString . #{ptr PAPI_substrate_info_t, support_version}
    `ap2` peekCString . #{ptr PAPI_substrate_info_t, kernel_version}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, num_cntrs}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, num_mpx_cntrs}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, num_preset_events}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, num_native_events}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, default_domain}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, available_domains}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, default_granularity}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, available_granularities}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, itimer_sig}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, itimer_num}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, itimer_ns}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, itimer_res_ns}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, hardware_intr_sig}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, clock_ticks}
    `ap2` liftM fromCInt . #{peek PAPI_substrate_info_t, opcode_match_width}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, hardware_intr}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, precise_intr}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, posix1b_timers}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, kernel_profile}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, kernel_multiplex}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, data_address_range}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, instr_address_range}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, fast_counter_read}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, fast_real_timer}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, fast_virtual_timer}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, attach}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, attach_must_ptrace}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, edge_detect}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, invert}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, profile_ear}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, cntr_groups}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, cntr_umasks}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, cntr_IEAR_events}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, cntr_DEAR_events}
    `ap2` liftM wordToBool . #{peek_bit PAPI_substrate_info_t, cntr_OPCM_events}

  poke ptr value = do
    pokeCString (#{ptr PAPI_substrate_info_t, name} ptr)
                (substrateName value) #{const PAPI_MAX_STR_LEN-1}
    pokeCString (#{ptr PAPI_substrate_info_t, version} ptr)
                (substrateVersion value) #{const PAPI_MIN_STR_LEN-1}
    pokeCString (#{ptr PAPI_substrate_info_t, support_version} ptr)
                (substrateSupportVersion value) #{const PAPI_MIN_STR_LEN-1}
    pokeCString (#{ptr PAPI_substrate_info_t, kernel_version} ptr)
                (substrateKernelVersion value) #{const PAPI_MIN_STR_LEN-1}
    #{poke PAPI_substrate_info_t, num_cntrs} ptr $
      toCInt $ substrateNumCntrs value
    #{poke PAPI_substrate_info_t, num_mpx_cntrs} ptr $
      toCInt $ substrateNumMpxCntrs value
    #{poke PAPI_substrate_info_t, num_preset_events} ptr $
      toCInt $ substrateNumPresetEvents value
    #{poke PAPI_substrate_info_t, num_native_events} ptr $
      toCInt $ substrateNumNativeEvents value
    #{poke PAPI_substrate_info_t, default_domain} ptr $
      toCInt $ substrateDefaultDomain value
    #{poke PAPI_substrate_info_t, available_domains} ptr $
      toCInt $ substrateAvailableDomains value
    #{poke PAPI_substrate_info_t, default_granularity} ptr $
      toCInt $ substrateDefaultGranularity value
    #{poke PAPI_substrate_info_t, available_granularities} ptr $
      toCInt $ substrateAvailableGranularities value
    #{poke PAPI_substrate_info_t, itimer_sig} ptr $
      toCInt $ substrateItimerSig value
    #{poke PAPI_substrate_info_t, itimer_num} ptr $
      toCInt $ substrateItimerNum value
    #{poke PAPI_substrate_info_t, itimer_ns} ptr $
      toCInt $ substrateItimerNs value
    #{poke PAPI_substrate_info_t, itimer_res_ns} ptr $
      toCInt $ substrateItimerResNs value
    #{poke PAPI_substrate_info_t, hardware_intr_sig} ptr $
      toCInt $ substrateHardwareIntrSig value
    #{poke PAPI_substrate_info_t, clock_ticks} ptr $
      toCInt $ substrateClockTicks value
    #{poke PAPI_substrate_info_t, opcode_match_width} ptr $
      toCInt $ substrateOpcodeMatchWidth value

    #{poke_bit PAPI_substrate_info_t, hardware_intr} ptr $
      boolToWord $ substrateHardwareIntr value
    #{poke_bit PAPI_substrate_info_t, precise_intr} ptr $
      boolToWord $ substratePreciseIntr value
    #{poke_bit PAPI_substrate_info_t, posix1b_timers} ptr $
      boolToWord $ substratePosix1bTimers value
    #{poke_bit PAPI_substrate_info_t, kernel_profile} ptr $
      boolToWord $ substrateKernelProfile value
    #{poke_bit PAPI_substrate_info_t, kernel_multiplex} ptr $
      boolToWord $ substrateKernelMultiplex value
    #{poke_bit PAPI_substrate_info_t, data_address_range} ptr $
      boolToWord $ substrateDataAddressRange value
    #{poke_bit PAPI_substrate_info_t, instr_address_range} ptr $
      boolToWord $ substrateInstrAddressRange value
    #{poke_bit PAPI_substrate_info_t, fast_counter_read} ptr $
      boolToWord $ substrateFastCounterRead value
    #{poke_bit PAPI_substrate_info_t, fast_real_timer} ptr $
      boolToWord $ substrateFastRealTimer value
    #{poke_bit PAPI_substrate_info_t, fast_virtual_timer} ptr $
      boolToWord $ substrateFastVirtualTimer value
    #{poke_bit PAPI_substrate_info_t, attach} ptr $
      boolToWord $ substrateAttach value
    #{poke_bit PAPI_substrate_info_t, attach_must_ptrace} ptr $
      boolToWord $ substrateAttachMustPtrace value
    #{poke_bit PAPI_substrate_info_t, edge_detect} ptr $
      boolToWord $ substrateEdgeDetect value
    #{poke_bit PAPI_substrate_info_t, invert} ptr $
      boolToWord $ substrateInvert value
    #{poke_bit PAPI_substrate_info_t, profile_ear} ptr $
      boolToWord $ substrateProfileEar value
    #{poke_bit PAPI_substrate_info_t, cntr_groups} ptr $
      boolToWord $ substrateCntrGroups value
    #{poke_bit PAPI_substrate_info_t, cntr_umasks} ptr $
      boolToWord $ substrateCntrUmasks value
    #{poke_bit PAPI_substrate_info_t, cntr_IEAR_events} ptr $
      boolToWord $ substrateCntrIEAREvents value
    #{poke_bit PAPI_substrate_info_t, cntr_DEAR_events} ptr $
      boolToWord $ substrateCntrDEAREvents value
    #{poke_bit PAPI_substrate_info_t, cntr_OPCM_events} ptr $
      boolToWord $ substrateCntrOPCMEvents value

----------------

newtype ShlibInfo = ShlibInfo [AddressMap] deriving (Eq, Show)
instance Storable ShlibInfo where
  sizeOf _ = #{size PAPI_shlib_info_t}
  alignment _ = #{alignment PAPI_shlib_info_t}

  peek ptr = do
    count <- #{peek PAPI_shlib_info_t, count} ptr :: IO CInt
    mapArray <- #{peek PAPI_shlib_info_t, map} ptr
    liftM ShlibInfo $ mapM (peekElemOff mapArray) [0..fromIntegral count-1]

  poke = error "poke on System.PAPI.ShlibInfo not permitted"

----------------

data DmemInfo = DmemInfo {
  dmemPeak :: Integer,
  dmemSize :: Integer,
  dmemResident :: Integer,
  dmemHighWaterMark :: Integer,
  dmemShared :: Integer,
  dmemText :: Integer,
  dmemLibrary :: Integer,
  dmemHeap :: Integer,
  dmemLocked :: Integer,
  dmemStack :: Integer,
  dmemPagesize :: Integer,
  dmemPte :: Integer
} deriving (Eq, Show)

instance Storable DmemInfo where
  sizeOf _ = #{size PAPI_dmem_info_t}
  alignment _ = #{alignment PAPI_dmem_info_t}

  peek = const (return DmemInfo)
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, peak}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, size}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, resident}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, high_water_mark}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, shared}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, text}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, library}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, heap}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, locked}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, stack}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, pagesize}
    `ap2` liftM fromCLLong . #{peek PAPI_dmem_info_t, pte}

  poke ptr value = do
    #{poke PAPI_dmem_info_t, peak} ptr $ toCLLong $ dmemPeak value
    #{poke PAPI_dmem_info_t, size} ptr $ toCLLong $ dmemSize value
    #{poke PAPI_dmem_info_t, resident} ptr $ toCLLong $ dmemResident value
    #{poke PAPI_dmem_info_t, high_water_mark} ptr $ toCLLong $ dmemHighWaterMark value
    #{poke PAPI_dmem_info_t, shared} ptr $ toCLLong $ dmemShared value
    #{poke PAPI_dmem_info_t, text} ptr $ toCLLong $ dmemText value
    #{poke PAPI_dmem_info_t, library} ptr $ toCLLong $ dmemLibrary value
    #{poke PAPI_dmem_info_t, heap} ptr $ toCLLong $ dmemHeap value
    #{poke PAPI_dmem_info_t, locked} ptr $ toCLLong $ dmemLocked value
    #{poke PAPI_dmem_info_t, stack} ptr $ toCLLong $ dmemStack value
    #{poke PAPI_dmem_info_t, pagesize} ptr $ toCLLong $ dmemPagesize value
    #{poke PAPI_dmem_info_t, pte} ptr $ toCLLong $ dmemPte value
    return ()

----------------

data ItimerOption = ItimerOption {
  itimerOptionItimerNum :: Integer,
  itimerOptionItimerSig :: Integer,
  itimerOptionNs :: Integer,
  itimerOptionFlags :: ItimerFlags
} deriving (Eq, Show)

data ItimerFlags = ItimerFlagsDefault deriving (Eq, Show)

instance Storable ItimerOption where
  sizeOf _ = #{size PAPI_itimer_option_t}
  alignment _ = #{alignment PAPI_itimer_option_t}

  peek = const (return ItimerOption)
    `ap2` liftM fromCInt . #{peek PAPI_itimer_option_t, itimer_num}
    `ap2` liftM fromCInt . #{peek PAPI_itimer_option_t, itimer_sig}
    `ap2` liftM fromCInt . #{peek PAPI_itimer_option_t, ns}
    `ap2` (const $ return ItimerFlagsDefault)

  poke ptr (ItimerOption num sig ns flags) = do
    #{poke PAPI_itimer_option_t, itimer_num} ptr $ toCInt num
    #{poke PAPI_itimer_option_t, itimer_sig} ptr $ toCInt sig
    #{poke PAPI_itimer_option_t, ns} ptr $ toCInt ns
    case flags of
      ItimerFlagsDefault -> #{poke PAPI_itimer_option_t, flags} ptr (0 :: CInt)

----------------

data PreloadInfo = PreloadInfo {
  preloadLibPreloadEnv :: String,
  preloadLibPreloadSep :: Char,
  preloadLibDirEnv :: String,
  preloadLibDirSep :: Char
} deriving (Eq, Show)

instance Storable PreloadInfo where
  sizeOf _ = #{size PAPI_preload_info_t}
  alignment _ = #{alignment PAPI_preload_info_t}

  peek = const (return PreloadInfo)
    `ap2` peekCString           . #{ptr  PAPI_preload_info_t, lib_preload_env}
    `ap2` liftM castCCharToChar . #{peek PAPI_preload_info_t, lib_preload_sep}
    `ap2` peekCString           . #{ptr  PAPI_preload_info_t, lib_dir_env}
    `ap2` liftM castCCharToChar . #{peek PAPI_preload_info_t, lib_dir_sep}

  poke ptr (PreloadInfo preloadEnv preloadSep dirEnv dirSep) = do
    pokeCString (#{ptr PAPI_preload_info_t, lib_preload_env} ptr)
      preloadEnv #{const PAPI_MAX_STR_LEN}
    #{poke PAPI_preload_info_t, lib_preload_sep} ptr preloadSep
    pokeCString (#{ptr PAPI_preload_info_t, lib_dir_env} ptr)
      dirEnv #{const PAPI_MAX_STR_LEN}
    #{poke PAPI_preload_info_t, lib_dir_sep} ptr dirSep

----------------

type DebugHandler = FunPtr (CInt -> CInt)
data DebugOption = DebugOption {
  debugOptionLevel :: DebugLevel,
  debugOptionHandler :: FunPtr (CInt -> CInt)
} deriving (Eq, Show)

instance Storable DebugOption where
  sizeOf _ = #{size PAPI_debug_option_t}
  alignment _ = #{alignment PAPI_debug_option_t}

  peek = const (return DebugOption)
    `ap2` #{peek PAPI_debug_option_t, level}
    `ap2` #{peek PAPI_debug_option_t, handler}

  poke ptr (DebugOption level handler) = do
    #{poke PAPI_debug_option_t, level} ptr level
    #{poke PAPI_debug_option_t, handler} ptr handler

----------------

data MultiplexOption = MultiplexOption {
  multiplexOptionEventSet :: EventSet,
  multiplexOptionNs :: Integer,
  multiplexOptionFlags :: MultiplexFlags
} deriving (Eq, Show)

instance Storable MultiplexOption where
  sizeOf _ = #{size PAPI_multiplex_option_t}
  alignment _ = #{alignment PAPI_multiplex_option_t}

  peek = const (return MultiplexOption)
    `ap2` #{peek PAPI_multiplex_option_t, eventset}
    `ap2` liftM fromCInt . #{peek PAPI_multiplex_option_t, ns}
    `ap2` #{peek PAPI_multiplex_option_t, flags}

  poke ptr (MultiplexOption eventset ns flags) = do
    #{poke PAPI_multiplex_option_t, eventset} ptr eventset
    #{poke PAPI_multiplex_option_t, ns} ptr $ toCInt ns
    #{poke PAPI_multiplex_option_t, flags} ptr flags

----------------

data MultiplexFlags = MultiplexFlagsDefault | MultiplexFlagsForceSw
  deriving (Eq, Show)

instance Storable MultiplexFlags where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
    cint <- peek (castPtr ptr :: Ptr CInt)
    case cint of
      #{const PAPI_MULTIPLEX_DEFAULT}  -> return MultiplexFlagsDefault
      #{const PAPI_MULTIPLEX_FORCE_SW} -> return MultiplexFlagsForceSw
      _ -> error "Internal error: Unknown multiplex flag"

  poke ptr MultiplexFlagsDefault =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_MULTIPLEX_DEFAULT}
  poke ptr MultiplexFlagsForceSw =
    poke (castPtr ptr :: Ptr CInt) #{const PAPI_MULTIPLEX_FORCE_SW}
