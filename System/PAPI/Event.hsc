------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Event
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: event codes
--

module System.PAPI.Event (
  enumEvent
, getEventInfo
, queryEvent
, eventCodeToName
, eventNameToCode

, EventCode(..)
, papi_null
, papi_br_cn
, papi_br_ins
, papi_br_msp
, papi_br_ntk
, papi_br_prc
, papi_br_tkn
, papi_br_ucn
, papi_bru_idl
, papi_btac_m
, papi_ca_cln
, papi_ca_inv
, papi_ca_itv
, papi_ca_shr
, papi_ca_snp
, papi_csr_fal
, papi_csr_suc
, papi_csr_tot
, papi_fad_ins
, papi_fdv_ins
, papi_fma_ins
, papi_fml_ins
, papi_fnv_ins
, papi_fp_ins
, papi_fp_ops
, papi_fp_stal
, papi_fpu_idl
, papi_fsq_ins
, papi_ful_ccy
, papi_ful_icy
, papi_fxu_idl
, papi_hw_int
, papi_int_ins
, papi_tot_cyc
, papi_tot_iis
, papi_tot_ins
, papi_vec_ins
, papi_l1_dca
, papi_l1_dch
, papi_l1_dcm
, papi_l1_dcr
, papi_l1_dcw
, papi_l1_ica
, papi_l1_ich
, papi_l1_icm
, papi_l1_icr
, papi_l1_icw
, papi_l1_ldm
, papi_l1_stm
, papi_l1_tca
, papi_l1_tch
, papi_l1_tcm
, papi_l1_tcr
, papi_l1_tcw
, papi_l2_dca
, papi_l2_dch
, papi_l2_dcm
, papi_l2_dcr
, papi_l2_dcw
, papi_l2_ica
, papi_l2_ich
, papi_l2_icm
, papi_l2_icr
, papi_l2_icw
, papi_l2_ldm
, papi_l2_stm
, papi_l2_tca
, papi_l2_tch
, papi_l2_tcm
, papi_l2_tcr
, papi_l2_tcw
, papi_l3_dca
, papi_l3_dch
, papi_l3_dcm
, papi_l3_dcr
, papi_l3_dcw
, papi_l3_ica
, papi_l3_ich
, papi_l3_icm
, papi_l3_icr
, papi_l3_icw
, papi_l3_ldm
, papi_l3_stm
, papi_l3_tca
, papi_l3_tch
, papi_l3_tcm
, papi_l3_tcr
, papi_l3_tcw
, papi_ld_ins
, papi_lst_ins
, papi_lsu_idl
, papi_mem_rcy
, papi_mem_scy
, papi_mem_wcy
, papi_prf_dm
, papi_res_stl
, papi_sr_ins
, papi_stl_ccy
, papi_stl_icy
, papi_syc_ins
, papi_tlb_dm
, papi_tlb_im
, papi_tlb_sd
, papi_tlb_tl

, Modifier(..)
, papiEnumEvents
, papiEnumFirst
, papiPresetEnumAvail
, papiPresetEnumMsc
, papiPresetEnumIns
, papiPresetEnumIdl
, papiPresetEnumBr
, papiPresetEnumCnd
, papiPresetEnumMem
, papiPresetEnumCach
, papiPresetEnumL1
, papiPresetEnumL2
, papiPresetEnumL3
, papiPresetEnumTlb
, papiPresetEnumFp
, papiNtvEnumUmasks
, papiNtvEnumUmaskCombos
, papiNtvEnumIarr
, papiNtvEnumDarr
, papiNtvEnumOpcm
, papiNtvEnumIear
, papiNtvEnumDear
, papiNtvEnumGroups
, papiEnumAll

, EventType
, EventInfo(..)

) where

import Control.Monad
import Foreign
import Foreign.C

import System.PAPI.Error
import System.PAPI.Util

#include "papi.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
-- ^ TODO move "alignment" macro to common header

-- * Event Query
foreign import ccall "PAPI_enum_event" papi_enum_event
  :: Ptr EventCode -> Modifier -> IO PapiError
enumEvent :: EventCode -> Modifier -> IO EventCode
enumEvent event modifier =
  with event $ \ptr ->
  papi_enum_event ptr modifier >>= papiThrow "Error in enumEvent: " >>
  peek ptr

foreign import ccall "PAPI_get_event_info" papi_get_event_info
  :: EventCode -> Ptr EventInfo -> IO PapiError
getEventInfo :: EventCode -> IO EventInfo
getEventInfo event =
  alloca $ \ptr ->
  papi_get_event_info event ptr >>= papiThrow "Error in getEventInfo: " >>
  peek ptr

foreign import ccall "PAPI_query_event" papi_query_event
  :: EventCode -> IO PapiError
queryEvent :: EventCode -> IO Bool
queryEvent event = do
  ret <- papi_query_event event
  if ret == papiErrorEnoevnt
    then return False
    else papiThrow "Error in queryEvent: " ret >> return True

-- * Event Translation
foreign import ccall "PAPI_event_code_to_name" papi_event_code_to_name
  :: EventCode -> CString -> IO PapiError
eventCodeToName :: EventCode -> IO String
eventCodeToName event =
  allocaArray #{const PAPI_MAX_STR_LEN} $ \ptr ->
  papi_event_code_to_name event ptr >>= papiThrow "Error in eventCodeToName: "
  >> peekCString ptr

foreign import ccall "PAPI_event_name_to_code" papi_event_name_to_code
  :: CString -> Ptr EventCode -> IO PapiError
eventNameToCode :: String -> IO EventCode
eventNameToCode name =
  withCString name $ \str ->
  alloca $ \event ->
  papi_event_name_to_code str event >>= papiThrow "Error in eventNameToCode: "
  >> peek event

-- * EventCode Type
newtype EventCode = EventCode { unEventCode :: CInt }
  deriving (Eq, Show,Storable)

#{enum EventCode, EventCode
, papi_null = PAPI_NULL // A nonexistent hardware event used as a placeholder

// Conditional Branching
, papi_br_cn = PAPI_BR_CN // Conditional branch instructions
, papi_br_ins = PAPI_BR_INS // Branch instructions
, papi_br_msp = PAPI_BR_MSP // Conditional branch instructions mispredicted
, papi_br_ntk = PAPI_BR_NTK // Conditional branch instructions not taken
, papi_br_prc = PAPI_BR_PRC // Conditional branch instructions correctly predicted
, papi_br_tkn = PAPI_BR_TKN // Conditional branch instructions taken
, papi_br_ucn = PAPI_BR_UCN // Unconditional branch instructions
, papi_bru_idl = PAPI_BRU_IDL // Cycles branch units are idle
, papi_btac_m = PAPI_BTAC_M // Branch target address cache misses

// Cache Requests:
, papi_ca_cln = PAPI_CA_CLN // Requests for exclusive access to clean cache line
, papi_ca_inv = PAPI_CA_INV // Requests for cache line invalidation
, papi_ca_itv = PAPI_CA_ITV // Requests for cache line intervention
, papi_ca_shr = PAPI_CA_SHR // Requests for exclusive access to shared cache line
, papi_ca_snp = PAPI_CA_SNP // Requests for a snoop

// Conditional Store:
, papi_csr_fal = PAPI_CSR_FAL // Failed store conditional instructions
, papi_csr_suc = PAPI_CSR_SUC // Successful store conditional instructions
, papi_csr_tot = PAPI_CSR_TOT // Total store conditional instructions

// Floating Point Operations:
, papi_fad_ins = PAPI_FAD_INS // Floating point add instructions
, papi_fdv_ins = PAPI_FDV_INS // Floating point divide instructions
, papi_fma_ins = PAPI_FMA_INS // FMA instructions completed
, papi_fml_ins = PAPI_FML_INS // Floating point multiply instructions
, papi_fnv_ins = PAPI_FNV_INS // Floating point inverse instructions
, papi_fp_ins = PAPI_FP_INS // Floating point instructions
, papi_fp_ops = PAPI_FP_OPS // Floating point operations
, papi_fp_stal = PAPI_FP_STAL // Cycles the FP unit
, papi_fpu_idl = PAPI_FPU_IDL // Cycles floating point units are idle
, papi_fsq_ins = PAPI_FSQ_INS // Floating point square root instructions

// Instruction Counting:
, papi_ful_ccy = PAPI_FUL_CCY // Cycles with maximum instructions completed
, papi_ful_icy = PAPI_FUL_ICY // Cycles with maximum instruction issue
, papi_fxu_idl = PAPI_FXU_IDL // Cycles integer units are idle
, papi_hw_int = PAPI_HW_INT // Hardware interrupts
, papi_int_ins = PAPI_INT_INS // Integer instructions
, papi_tot_cyc = PAPI_TOT_CYC // Total cycles
, papi_tot_iis = PAPI_TOT_IIS // Instructions issued
, papi_tot_ins = PAPI_TOT_INS // Instructions completed
, papi_vec_ins = PAPI_VEC_INS // Vector/SIMD instructions

// Cache Access:
, papi_l1_dca = PAPI_L1_DCA // L1 data cache accesses
, papi_l1_dch = PAPI_L1_DCH // L1 data cache hits
, papi_l1_dcm = PAPI_L1_DCM // L1 data cache misses
, papi_l1_dcr = PAPI_L1_DCR // L1 data cache reads
, papi_l1_dcw = PAPI_L1_DCW // L1 data cache writes
, papi_l1_ica = PAPI_L1_ICA // L1 instruction cache accesses
, papi_l1_ich = PAPI_L1_ICH // L1 instruction cache hits
, papi_l1_icm = PAPI_L1_ICM // L1 instruction cache misses
, papi_l1_icr = PAPI_L1_ICR // L1 instruction cache reads
, papi_l1_icw = PAPI_L1_ICW // L1 instruction cache writes
, papi_l1_ldm = PAPI_L1_LDM // L1 load misses
, papi_l1_stm = PAPI_L1_STM // L1 store misses
, papi_l1_tca = PAPI_L1_TCA // L1 total cache accesses
, papi_l1_tch = PAPI_L1_TCH // L1 total cache hits
, papi_l1_tcm = PAPI_L1_TCM // L1 total cache misses
, papi_l1_tcr = PAPI_L1_TCR // L1 total cache reads
, papi_l1_tcw = PAPI_L1_TCW // L1 total cache writes
, papi_l2_dca = PAPI_L2_DCA // L2 data cache accesses
, papi_l2_dch = PAPI_L2_DCH // L2 data cache hits
, papi_l2_dcm = PAPI_L2_DCM // L2 data cache misses
, papi_l2_dcr = PAPI_L2_DCR // L2 data cache reads
, papi_l2_dcw = PAPI_L2_DCW // L2 data cache writes
, papi_l2_ica = PAPI_L2_ICA // L2 instruction cache accesses
, papi_l2_ich = PAPI_L2_ICH // L2 instruction cache hits
, papi_l2_icm = PAPI_L2_ICM // L2 instruction cache misses
, papi_l2_icr = PAPI_L2_ICR // L2 instruction cache reads
, papi_l2_icw = PAPI_L2_ICW // L2 instruction cache writes
, papi_l2_ldm = PAPI_L2_LDM // L2 load misses
, papi_l2_stm = PAPI_L2_STM // L2 store misses
, papi_l2_tca = PAPI_L2_TCA // L2 total cache accesses
, papi_l2_tch = PAPI_L2_TCH // L2 total cache hits
, papi_l2_tcm = PAPI_L2_TCM // L2 total cache misses
, papi_l2_tcr = PAPI_L2_TCR // L2 total cache reads
, papi_l2_tcw = PAPI_L2_TCW // L2 total cache writes
, papi_l3_dca = PAPI_L3_DCA // L3 data cache accesses
, papi_l3_dch = PAPI_L3_DCH // L3 Data Cache Hits
, papi_l3_dcm = PAPI_L3_DCM // L3 data cache misses
, papi_l3_dcr = PAPI_L3_DCR // L3 data cache reads
, papi_l3_dcw = PAPI_L3_DCW // L3 data cache writes
, papi_l3_ica = PAPI_L3_ICA // L3 instruction cache accesses
, papi_l3_ich = PAPI_L3_ICH // L3 instruction cache hits
, papi_l3_icm = PAPI_L3_ICM // L3 instruction cache misses
, papi_l3_icr = PAPI_L3_ICR // L3 instruction cache reads
, papi_l3_icw = PAPI_L3_ICW // L3 instruction cache writes
, papi_l3_ldm = PAPI_L3_LDM // L3 load misses
, papi_l3_stm = PAPI_L3_STM // L3 store misses
, papi_l3_tca = PAPI_L3_TCA // L3 total cache accesses
, papi_l3_tch = PAPI_L3_TCH // L3 total cache hits
, papi_l3_tcm = PAPI_L3_TCM // L3 cache misses
, papi_l3_tcr = PAPI_L3_TCR // L3 total cache reads
, papi_l3_tcw = PAPI_L3_TCW // L3 total cache writes

// Data Access:
, papi_ld_ins = PAPI_LD_INS // Load instructions
, papi_lst_ins = PAPI_LST_INS // Load/store instructions completed
, papi_lsu_idl = PAPI_LSU_IDL // Cycles load/store units are idle
, papi_mem_rcy = PAPI_MEM_RCY // Cycles Stalled Waiting for memory Reads
, papi_mem_scy = PAPI_MEM_SCY // Cycles Stalled Waiting for memory accesses
, papi_mem_wcy = PAPI_MEM_WCY // Cycles Stalled Waiting for memory writes
, papi_prf_dm = PAPI_PRF_DM // Data prefetch cache misses
, papi_res_stl = PAPI_RES_STL // Cycles stalled on any resource
, papi_sr_ins = PAPI_SR_INS // Store instructions
, papi_stl_ccy = PAPI_STL_CCY // Cycles with no instructions completed
, papi_stl_icy = PAPI_STL_ICY // Cycles with no instruction issue
, papi_syc_ins = PAPI_SYC_INS // Synchronization instructions completed

// TLB Operations:
, papi_tlb_dm = PAPI_TLB_DM // Data translation lookaside buffer misses
, papi_tlb_im = PAPI_TLB_IM // Instruction translation lookaside buffer misses
, papi_tlb_sd = PAPI_TLB_SD // Translation lookaside buffer shootdowns
, papi_tlb_tl = PAPI_TLB_TL // Total translation lookaside buffer misses
}

newtype Modifier = Modifier { unModifier :: CInt } deriving (Eq, Show)

#{enum Modifier, Modifier
, papiEnumEvents = PAPI_ENUM_EVENTS	// Always enumerate all events
, papiEnumFirst = PAPI_ENUM_FIRST	// Enumerate first event (preset or native)
, papiPresetEnumAvail = PAPI_PRESET_ENUM_AVAIL // Enumerate events that exist here

   // PAPI PRESET section
, papiPresetEnumMsc = PAPI_PRESET_ENUM_MSC	// Miscellaneous preset events
, papiPresetEnumIns = PAPI_PRESET_ENUM_INS	// Instruction related preset events
, papiPresetEnumIdl = PAPI_PRESET_ENUM_IDL	// Stalled or Idle preset events
, papiPresetEnumBr = PAPI_PRESET_ENUM_BR	// Branch related preset events
, papiPresetEnumCnd = PAPI_PRESET_ENUM_CND	// Conditional preset events
, papiPresetEnumMem = PAPI_PRESET_ENUM_MEM	// Memory related preset events
, papiPresetEnumCach = PAPI_PRESET_ENUM_CACH	// Cache related preset events
, papiPresetEnumL1 = PAPI_PRESET_ENUM_L1	// L1 cache related preset events
, papiPresetEnumL2 = PAPI_PRESET_ENUM_L2	// L2 cache related preset events
, papiPresetEnumL3 = PAPI_PRESET_ENUM_L3	// L3 cache related preset events
, papiPresetEnumTlb = PAPI_PRESET_ENUM_TLB	// Translation Lookaside Buffer events
, papiPresetEnumFp = PAPI_PRESET_ENUM_FP	// Floating Point related preset events

   // PAPI native event related section
, papiNtvEnumUmasks = PAPI_NTV_ENUM_UMASKS	// all individual bits for given group
, papiNtvEnumUmaskCombos = PAPI_NTV_ENUM_UMASK_COMBOS // all combinations of mask bits for given group
, papiNtvEnumIarr = PAPI_NTV_ENUM_IARR	// Enumerate events that support IAR (instruction address ranging)
, papiNtvEnumDarr = PAPI_NTV_ENUM_DARR	// Enumerate events that support DAR (data address ranging)
, papiNtvEnumOpcm = PAPI_NTV_ENUM_OPCM	// Enumerate events that support OPC (opcode matching)
, papiNtvEnumIear = PAPI_NTV_ENUM_IEAR	// Enumerate IEAR (instruction event address register) events
, papiNtvEnumDear = PAPI_NTV_ENUM_DEAR	// Enumerate DEAR (data event address register) events
, papiNtvEnumGroups = PAPI_NTV_ENUM_GROUPS	// Enumerate groups an event belongs to a la POWER4/5
, papiEnumAll = PAPI_ENUM_ALL
}

type EventType = CUInt -- TODO: currenly unused by PAPI?

data EventInfo = EventInfo {
  eventEventCode :: EventCode
, eventEventType :: EventType
, eventSymbol :: String
, eventShortDescr :: String
, eventLongDescr :: String
, eventDerived :: String
, eventPostfix :: String
, eventCodeAndName :: [(Integer, String)]
, eventNote :: String
} deriving (Eq, Show)

instance Storable EventInfo where
  sizeOf _ = #{size PAPI_event_info_t}
  alignment _ = #{alignment PAPI_event_info_t}

  peek ptr = do
    count <- liftM fromCUInt $ #{peek PAPI_event_info_t, count} ptr
    code <- mapM (liftM fromCUInt . peekElemOff codeArray)
                 [0..fromIntegral count-1]
    name <- sequence [peekCString (nameArray `plusPtr`
                                   (i * #{const PAPI_2MAX_STR_LEN}))
                     | i <- [0..fromIntegral count-1]]
    (const (return EventInfo)
      `ap2` #{peek PAPI_event_info_t, event_code}
      `ap2` #{peek PAPI_event_info_t, event_type}
      `ap2` peekCString . #{ptr PAPI_event_info_t, symbol}
      `ap2` peekCString . #{ptr PAPI_event_info_t, short_descr}
      `ap2` peekCString . #{ptr PAPI_event_info_t, long_descr}
      `ap2` peekCString . #{ptr PAPI_event_info_t, derived}
      `ap2` peekCString . #{ptr PAPI_event_info_t, postfix}
      `ap2` (const $ return $ zip code name)
      `ap2` peekCString . #{ptr PAPI_event_info_t, note}) ptr
    where codeArray = #{ptr PAPI_event_info_t, code} ptr
          nameArray = #{ptr PAPI_event_info_t, name} ptr

  poke ptr value = do
    let (code, name) = unzip (take #{const PAPI_MAX_INFO_TERMS}
                                   (eventCodeAndName value))
    #{poke PAPI_event_info_t, event_code} ptr $ eventEventCode value
    #{poke PAPI_event_info_t, event_type} ptr $ eventEventType value
    #{poke PAPI_event_info_t, count}      ptr $
      (fromIntegral $ length $ eventCodeAndName value :: CInt)
    pokeCString (#{ptr PAPI_event_info_t, symbol} ptr)
      (eventSymbol value) #{const PAPI_HUGE_STR_LEN-1}
    pokeCString (#{ptr PAPI_event_info_t, short_descr} ptr)
      (eventShortDescr value) #{const PAPI_MIN_STR_LEN-1}
    pokeCString (#{ptr PAPI_event_info_t, long_descr} ptr)
      (eventLongDescr value) #{const PAPI_HUGE_STR_LEN-1}
    pokeCString (#{ptr PAPI_event_info_t, derived} ptr)
      (eventDerived value) #{const PAPI_MIN_STR_LEN-1}
    pokeCString (#{ptr PAPI_event_info_t, postfix} ptr)
      (eventLongDescr value) #{const PAPI_MIN_STR_LEN-1}
    zipWithM_ (pokeElemOff codeArray)  [0..] (map toCUInt code)
    zipWithM_ (\i x -> pokeCString
                         (nameArray `plusPtr` (i * #{const PAPI_2MAX_STR_LEN}))
                         x #{const PAPI_2MAX_STR_LEN-1})
      [0..] name
    pokeCString (#{ptr PAPI_event_info_t, note} ptr)
      (eventNote value) #{const PAPI_HUGE_STR_LEN-1}
    where codeArray = #{ptr PAPI_event_info_t, code} ptr
          nameArray = #{ptr PAPI_event_info_t, name} ptr
