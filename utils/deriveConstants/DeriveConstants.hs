#include "ghcautoconf.h"

{- ------------------------------------------------------------------------

(c) The GHC Team, 1992-2012
(c) William Kenyon 2014

DeriveConstants is a program that extracts constants from the C
declarations in the header files (primarily struct sizes and field offsets)
and makes these constants availiable to Haskell and Cmm files.

DeriveConstants generates constants twice, once with PROFILING on, and once with
PROFILING off. This is because many struct sizes and offsets are sensitive to
the CPP define PROFILING. If the need arose, it would be easy to extend
DeriveConstants to be sensitive to another CPP define such as DEBUG or THREADED
(see Note [template])

DeriveConstants generates the following files:

  * DerivedConstants.h
    - included by *.cmm files
    - Example (derived on a 64 bit machine)

        #ifdef PROFILING // The following constants were calculated
                         // with PROFILING turned on.

         #define SIZEOF_StgMutArrPtrs 40      // size in bytes
                                              // of C struct StgMutArrPtrs

         #define OFFSET_StgMutArrPtrs_ptrs 24 // offset in bytes of field 'ptrs'
                                              // from beggining of C struct
                                              // StgMutArrPtrs

         #define REP_StgMutArrPtrs_ptrs b64   // size in bytes of field 'ptrs'
                                              // in C struct StgMutArrPtrs

        #else // The following constants were calculated with PROFILING turned off

         #define SIZEOF_StgMutArrPtrs 24
         #define OFFSET_StgMutArrPtrs_ptrs 8
         #define REP_StgMutArrPtrs_ptrs b64

        #endif

        #define StgMutArrPtrs_ptrs(__ptr__) \
         REP_StgMutArrPtrs_ptrs[__ptr__+OFFSET_StgMutArrPtrs_ptrs]
         // Helper macro which can be used to load from, or assign to the ptrs
         // field, when given a pointer to a StgMutArrPtrs struct

  * platformConstants
    - read by DynFlags.
    - read at runtime to avoid recompilation of everything every time a c file
      is altered.
    - Example (derived on a 64 bit machine)
      PlatformConstants{
       ...
       ,pcProfiling_SIZEOFW_StgMutArrPtrs = 5
       ,pcProfiling_SIZEOF_StgMutArrPtrs = 40
       ,pcProfiling_OFFSET_StgMutArrPtrs_ptrs = 24
       ,pcProfiling_REP_StgMutArrPtrs_ptrs = 8  -- 64 bits measured in bytes
       ,pc_SIZEOFW_StgMutArrPtrs = 3
       ,pc_SIZEOF_StgMutArrPtrs = 24
       ,pc_OFFSET_StgMutArrPtrs_ptrs = 8
       ,pc_REP_StgMutArrPtrs_ptrs = 8           -- 64 bits measured in bytes
       ...
      }

  * GHCConstantsHaskellType.hs
    - included by PlatformConstants.
    - this defines the PlatformConstants type. The PlatformConstants type
      is used in Dflags to read the contents of platformContents at runtime

  * GHCConstantsHaskellDflagsWrappers.hs
    - included by Dflags
    - oFFSET_ and rEP_ wrapper functions to grab values from the
      platformConstants type.
    - example:

      oFFSET_StgMutArrPtrs_ptrs :: DynFlags -> Int
      oFFSET_StgMutArrPtrs_ptrs dflags = 
        if gopt Opt_SccProfilingOn dflags then (
          pcProfiling_OFFSET_StgMutArrPtrs_ptrs $ sPlatformConstants $ settings dflags
        ) else (
          pc_OFFSET_StgMutArrPtrs_ptrs $ sPlatformConstants $ settings dflags
        )

  * GHCConstantsHaskellDflagsExports.hs
    - included into the Dflags export list
    - contains all the wrapper functions defined in
      GHCConstantsHaskellDflagsWrappers.hs

  * GHCConstantsHaskellCodeGenWrappers.hs
    - included by StgCmmUtils
    - lOAD_ and sTORE_ wrapper functions for the code generator
    - example:

      lOAD_StgMutArrPtrs_ptrs :: DynFlags -> CmmExpr -> CmmExpr
      sTORE_StgMutArrPtrs_ptrs :: DynFlags -> CmmExpr -> CmmExpr -> CmmAGraph
      lOAD_StgMutArrPtrs_ptrs dflags ptr =
        CmmLoad (cmmOffsetB dflags ptr (oFFSET_StgMutArrPtrs_ptrs dflags)) $
          cmmBits $ widthFromBytes $ rEP_StgMutArrPtrs_ptrs dflags
      sTORE_StgMutArrPtrs_ptrs dflags ptr val =
        mkStore (cmmOffsetB dflags ptr (oFFSET_StgMutArrPtrs_ptrs dflags)) val

  * GHCConstantsHaskellCodeGenExports.hs
    - included into the StgCmmUtils export list
    - contains all the wrapper functions defined in
      GHCConstantsHaskellCodeGenWrappers.hs
------------------------------------------------------------------------ -}

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Numeric
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info
import System.Process

--------------------------------------------------------------------------------
-- The wanteds are split into sections,                                       --
-- based on what CPP defines they are sensitive to:                           --
-- * [Insensitive]                                                            --
-- * [Sensitive to PROFILING]                                                 --
-- * [Sensitive to PROFILING and TABLES_NEXT_TO_CODE]                         --
-- There is also a [PROFILING only] section. This is for getting info         --
-- from structs which do not exist when PROFILING is not defined.             --
-- To clarify:                                                                --
--   [Sensitive to PROFILING] generates constants for                         --
--   when PROFILING is defined and not defined                                --
--                                                                            --
--   [PROFILING only] generates constants for                                 --
--   when PROFILING is defined and generates an error for when it is not defined
--------------------------------------------------------------------------------
wanteds_i, wanteds_p, wanteds_profOnly :: [Wanted]
-------------------
-- [Insensitive] --
-------------------
wanteds_i = concat
  [[mkWord both  "BLOCK_SIZE"         "BLOCK_SIZE"]
   -- Size of a storage manager block (in bytes).
  ,[mkWord [Cmm] "MBLOCK_SIZE"        "MBLOCK_SIZE"]
  -- blocks that fit in an MBlock, leaving space for the block descriptors
  ,[mkWord both  "BLOCKS_PER_MBLOCK" "BLOCKS_PER_MBLOCK"]

--------------------------------------------------------------------------------
-- Note [baseReg layout]                                                      --
--                                                                            --
--              stgEagerBlackholeInfo | These functions are accessed by a     --
--              stgGCEnter1           | negative offset from baseReg. boi     --
--              stgGCFun              | allows for this negative offset       --
--  baseReg --> rR1                   + These are registers in the register   --
--              rR2                   + table, accessed by a positive offset  --
--              ...                   + from baseReg, the standard bo is used --
--------------------------------------------------------------------------------

  ,offset [bro "r"] "Capability" "f.stgEagerBlackholeInfo"
  ,offset [bro "r"] "Capability" "f.stgGCEnter1"
  ,offset [bro "r"] "Capability" "f.stgGCFun"

  -- The three offsets above are negative offsets from baseReg. see [baseReg layout].

  ,offset [bo] "StgRegTable" "rR1"
  ,offset [bo] "StgRegTable" "rR2"
  ,offset [bo] "StgRegTable" "rR3"
  ,offset [bo] "StgRegTable" "rR4"
  ,offset [bo] "StgRegTable" "rR5"
  ,offset [bo] "StgRegTable" "rR6"
  ,offset [bo] "StgRegTable" "rR7"
  ,offset [bo] "StgRegTable" "rR8"
  ,offset [bo] "StgRegTable" "rR9"
  ,offset [bo] "StgRegTable" "rR10"
  ,offset [bo] "StgRegTable" "rF1"
  ,offset [bo] "StgRegTable" "rF2"
  ,offset [bo] "StgRegTable" "rF3"
  ,offset [bo] "StgRegTable" "rF4"
  ,offset [bo] "StgRegTable" "rF5"
  ,offset [bo] "StgRegTable" "rF6"
  ,offset [bo] "StgRegTable" "rD1"
  ,offset [bo] "StgRegTable" "rD2"
  ,offset [bo] "StgRegTable" "rD3"
  ,offset [bo] "StgRegTable" "rD4"
  ,offset [bo] "StgRegTable" "rD5"
  ,offset [bo] "StgRegTable" "rD6"
  ,offset [bo] "StgRegTable" "rXMM1"
  ,offset [bo] "StgRegTable" "rXMM2"
  ,offset [bo] "StgRegTable" "rXMM3"
  ,offset [bo] "StgRegTable" "rXMM4"
  ,offset [bo] "StgRegTable" "rXMM5"
  ,offset [bo] "StgRegTable" "rXMM6"
  ,offset [bo] "StgRegTable" "rYMM1"
  ,offset [bo] "StgRegTable" "rYMM2"
  ,offset [bo] "StgRegTable" "rYMM3"
  ,offset [bo] "StgRegTable" "rYMM4"
  ,offset [bo] "StgRegTable" "rYMM5"
  ,offset [bo] "StgRegTable" "rYMM6"
  ,offset [bo] "StgRegTable" "rZMM1"
  ,offset [bo] "StgRegTable" "rZMM2"
  ,offset [bo] "StgRegTable" "rZMM3"
  ,offset [bo] "StgRegTable" "rZMM4"
  ,offset [bo] "StgRegTable" "rZMM5"
  ,offset [bo] "StgRegTable" "rZMM6"
  ,offset [bo] "StgRegTable" "rL1"
  ,offset [bo] "StgRegTable" "rSp"
  ,offset [bo] "StgRegTable" "rSpLim"
  ,offset [bo] "StgRegTable" "rHp"
  ,offset [bo] "StgRegTable" "rHpLim"
  ,offset [bo] "StgRegTable" "rCCCS"
  ,offset [bo] "StgRegTable" "rCurrentTSO"
  ,offset [bo] "StgRegTable" "rCurrentNursery"
  ,offset [bo] "StgRegTable" "rHpAlloc"
  ,offset [co,cr,cm] "StgRegTable" "rRet"
  ,offset [co,cr,cm] "StgRegTable" "rNursery"

  ,offset [bo]       "Capability"  "r"
  ,offset [co]       "Capability"  "lock"
  ,offset [co,cr,cm] "Capability"  "no"
  ,offset [co,cr,cm] "Capability"  "mut_lists"
  ,offset [co,cr,cm] "Capability"  "context_switch"
  ,offset [co,cr,cm] "Capability"  "interrupt"
  ,offset [co,cr,cm] "Capability"  "sparks"

  ,offset [bo,br,bm] "bdescr"      "start"
  ,offset [bo,br,bm] "bdescr"      "free"
  ,offset [bo,br,bm] "bdescr"      "blocks"
  ,offset [co,cr,cm] "bdescr"      "gen_no"
  ,offset [co,cr,cm] "bdescr"      "link"

  ,size   [Cmm]      "generation"
  ,offset [co,cr,cm] "generation"  "n_new_large_words"
  ,offset [co,cr,cm] "generation"  "weak_ptr_list"

  ,size   both       "CostCentreStack"
  ,offset [co,cr,cm] "CostCentreStack"    "ccsID"
  ,offset [bo,br,cm] "CostCentreStack"    "mem_alloc"
  ,offset [bo,br,cm] "CostCentreStack"    "scc_count"
  ,offset [co,cr,cm] "CostCentreStack"    "prevStack"

  ,offset [co,cr,cm] "CostCentre"         "ccID"
  ,offset [co,cr,cm] "CostCentre"         "link"

  ,offset [bo,br,cm] "StgEntCounter"      "allocs"
  ,offset [bo,br,cm] "StgEntCounter"      "allocd"
  ,offset [bo,br,cm] "StgEntCounter"      "registeredp"
  ,offset [bo,br,cm] "StgEntCounter"      "link"
  ,offset [bo,br,cm] "StgEntCounter"      "entry_count"

  ,offset_ [co,cr,cm] "RTS_FLAGS" "ProfFlags.showCCSOnException"
                         "RtsFlags_ProfFlags_showCCSOnException"
  ,offset_ [co,cr,cm] "RTS_FLAGS" "DebugFlags.apply"
                         "RtsFlags_DebugFlags_apply"
  ,offset_ [co,cr,cm] "RTS_FLAGS" "DebugFlags.sanity"
                         "RtsFlags_DebugFlags_sanity"
  ,offset_ [co,cr,cm] "RTS_FLAGS" "DebugFlags.weak"
                         "RtsFlags_DebugFlags_weak"
  ,offset_ [co,cr,cm] "RTS_FLAGS" "GcFlags.initialStkSize"
                         "RtsFlags_GcFlags_initialStkSize"
  ,offset_ [co,cr,cm] "RTS_FLAGS" "MiscFlags.tickInterval"
                         "RtsFlags_MiscFlags_tickInterval"

  ,offset  [co,cr,cm] "StgLargeBitmap" "size"
  ,offset  [co]       "StgLargeBitmap" "bitmap"

  ,size   [Cmm]      "snEntry"
  ,offset [co,cr,cm] "snEntry" "sn_obj"
  ,offset [co,cr,cm] "snEntry" "addr"

  ,size   [Cmm]      "spEntry"
  ,offset [co,cr,cm] "spEntry" "addr"

   -- Note that this conditional part only affects the C headers.
   -- That's important, as it means we get the same PlatformConstants
   -- type on all platforms.
  ,if os == "mingw32"
   then concat
     [size [Cmm]        "StgAsyncIOResult"
     ,offset [co,cr,cm] "StgAsyncIOResult" "reqID"
     ,offset [co,cr,cm] "StgAsyncIOResult" "len"
     ,offset [co,cr,cm] "StgAsyncIOResult" "errCode"
     ]
   else []

        -- pre-compiled thunk types
  ,[mkWord [Haskell] "MAX_SPEC_SELECTEE_SIZE" "MAX_SPEC_SELECTEE_SIZE"]
  ,[mkWord [Haskell] "MAX_SPEC_AP_SIZE"       "MAX_SPEC_AP_SIZE"]

   -- closure sizes: these do NOT include the header (see below for
   -- header sizes)
  ,[mkWord [Haskell] "MIN_PAYLOAD_SIZE" "MIN_PAYLOAD_SIZE"]

  ,[mkInt  [Haskell] "MIN_INTLIKE" "MIN_INTLIKE"]
  ,[mkWord [Haskell] "MAX_INTLIKE" "MAX_INTLIKE"]

  ,[mkWord [Haskell] "MIN_CHARLIKE" "MIN_CHARLIKE"]
  ,[mkWord [Haskell] "MAX_CHARLIKE" "MAX_CHARLIKE"]

  ,[mkWord [Haskell] "MUT_ARR_PTRS_CARD_BITS" "MUT_ARR_PTRS_CARD_BITS"]

   -- A section of code-generator-related MAGIC CONSTANTS.
  ,[mkWord [Haskell] "MAX_Vanilla_REG"      "MAX_VANILLA_REG"]
  ,[mkWord [Haskell] "MAX_Float_REG"        "MAX_FLOAT_REG"]
  ,[mkWord [Haskell] "MAX_Double_REG"       "MAX_DOUBLE_REG"]
  ,[mkWord [Haskell] "MAX_Long_REG"         "MAX_LONG_REG"]
  ,[mkWord [Haskell] "MAX_XMM_REG"          "MAX_XMM_REG"]
  ,[mkWord [Haskell] "MAX_Real_Vanilla_REG" "MAX_REAL_VANILLA_REG"]
  ,[mkWord [Haskell] "MAX_Real_Float_REG"   "MAX_REAL_FLOAT_REG"]
  ,[mkWord [Haskell] "MAX_Real_Double_REG"  "MAX_REAL_DOUBLE_REG"]
  ,[mkWord [Haskell] "MAX_Real_XMM_REG"     "MAX_REAL_XMM_REG"]
  ,[mkWord [Haskell] "MAX_Real_Long_REG"    "MAX_REAL_LONG_REG"]

   -- This tells the native code generator the size of the spill
   -- area is has available.
  ,[mkWord [Haskell] "RESERVED_C_STACK_BYTES" "RESERVED_C_STACK_BYTES"]
   -- The amount of (Haskell) stack to leave free for saving
   -- registers when returning to the scheduler.
  ,[mkWord [Haskell] "RESERVED_STACK_WORDS" "RESERVED_STACK_WORDS"]
   -- Continuations that need more than this amount of stack
   -- should do their own stack check (see bug #1466).
  ,[mkWord [Haskell] "AP_STACK_SPLIM" "AP_STACK_SPLIM"]

   -- Size of a word, in bytes
  ,[mkWord [Haskell] "WORD_SIZE" "SIZEOF_HSWORD"]

   -- Size of a double in StgWords.
  ,[mkWord [Haskell] "DOUBLE_SIZE" "SIZEOF_DOUBLE"]

   -- Size of a C int, in bytes. May be smaller than wORD_SIZE.
  ,[mkWord [Haskell] "CINT_SIZE"       "SIZEOF_INT"]
  ,[mkWord [Haskell] "CLONG_SIZE"      "SIZEOF_LONG"]
  ,[mkWord [Haskell] "CLONG_LONG_SIZE" "SIZEOF_LONG_LONG"]

   -- Number of bits to shift a bitfield left by in an info table.
  ,[mkWord [Haskell] "BITMAP_BITS_SHIFT" "BITMAP_BITS_SHIFT"]

   -- Amount of pointer bits used for semi-tagging constructor closures
  ,[mkWord [Haskell] "TAG_BITS" "TAG_BITS"]

  ,[mkBool [Haskell] "WORDS_BIGENDIAN"    "defined(WORDS_BIGENDIAN)"]
  ,[mkBool [Haskell] "DYNAMIC_BY_DEFAULT" "defined(DYNAMIC_BY_DEFAULT)"]

  ,[mkWord [Haskell] "LDV_SHIFT"         "LDV_SHIFT"]
  ,[mkNat  [Haskell] "ILDV_CREATE_MASK"  "LDV_CREATE_MASK"]
  ,[mkNat  [Haskell] "ILDV_STATE_CREATE" "LDV_STATE_CREATE"]
  ,[mkNat  [Haskell] "ILDV_STATE_USE"    "LDV_STATE_USE"]
  ]

------------------------------
-- [Sensitive to PROFILING] --
------------------------------
wanteds_p = concat
  [sizeW  [Haskell]  "StgHeader"
  ,size   both       "StgHeader"
  ,sizeW  [Haskell]  "StgThunkHeader"
  ,size   both       "StgThunkHeader"

  ,sizeW  [Haskell]  "StgClosure"
  ,offset [bo,br,bm] "StgClosure"         "header.info"

  ,offset [co,cmp]   "StgClosure"         "payload"
  ,offset [co,cmp]   "StgThunk"           "payload"

  ,size   both    "StgUpdateFrame"
  ,size   [Cmm]   "StgCatchFrame"
  ,size   [Cmm]   "StgStopFrame"

  ,sizeW  [Haskell]  "StgSmallMutArrPtrs"
  ,size   both       "StgSmallMutArrPtrs"
  ,offset [bo,br,bm] "StgSmallMutArrPtrs" "ptrs"

  ,sizeW  [Haskell]  "StgMutArrPtrs"
  ,size   both       "StgMutArrPtrs"
  ,offset [bo,br,bm] "StgMutArrPtrs" "ptrs"
  ,offset [bo,br,bm] "StgMutArrPtrs" "size"

  ,sizeW  [Haskell]  "StgArrWords"
  ,size   both       "StgArrWords"
  ,offset [bo,cr,cm] "StgArrWords"   "bytes"
  ,offset [co,cmp]   "StgArrWords"   "payload"

  ,offset  [co,cr,cm] "StgTSO" "_link"
  ,offset  [co,cr,cm] "StgTSO" "global_link"
  ,offset  [co,cr,cm] "StgTSO" "what_next"
  ,offset  [co,cr,cm] "StgTSO" "why_blocked"
  ,offset  [co,cr,cm] "StgTSO" "block_info"
  ,offset  [co,cr,cm] "StgTSO" "blocked_exceptions"
  ,offset  [co,cr,cm] "StgTSO" "id"
  ,offset  [co,cr,cm] "StgTSO" "cap"
  ,offset  [co,cr,cm] "StgTSO" "saved_errno"
  ,offset  [co,cr,cm] "StgTSO" "trec"
  ,offset  [co,cr,cm] "StgTSO" "flags"
  ,offset  [co,cr,cm] "StgTSO" "dirty"
  ,offset  [co,cr,cm] "StgTSO" "bq"
  ,offset  [bo,br,bm] "StgTSO" "alloc_limit"
  --For               "StgTSO" "cccs" see [PROFILING only]
  ,offset  [bo,br,bm] "StgTSO" "stackobj"

  ,offset [bo,br,bm] "StgStack"    "sp"
  ,offset [bo]       "StgStack"    "stack"
  ,offset [co,cr,cm] "StgStack"    "stack_size"
  ,offset [co,cr,cm] "StgStack"    "dirty"

  ,offset [bo,br,bm] "StgUpdateFrame" "updatee"
  ,offset [co,cr,cm] "StgCatchFrame"  "handler"
  ,offset [co,cr,cm] "StgCatchFrame"  "exceptions_blocked"

  ,size   [Cmm]       "StgPAP"
  ,offset [co,cr,cm]  "StgPAP" "n_args"
  ,offset [co,crg,cm] "StgPAP" "fun"
  ,offset [co,cr,cm]  "StgPAP" "arity"
  ,offset [co,cmp]    "StgPAP" "payload"

  ,size   [Cmm]       "StgAP"
  ,offset [co,cr,cm]  "StgAP"  "n_args"
  ,offset [co,crg,cm] "StgAP"  "fun"
  ,offset [co,cmp]    "StgAP"  "payload"

  ,size   [Cmm]       "StgAP_STACK"
  ,offset [co,cr,cm]  "StgAP_STACK" "size"
  ,offset [co,crg,cm] "StgAP_STACK" "fun"
  ,offset [co,cmp]    "StgAP_STACK" "payload"

  ,size   [Cmm]       "StgSelector"

  ,offset [co,crg,cm] "StgInd" "indirectee"

  ,size   [Cmm]      "StgMutVar"
  ,offset [co,cr,cm] "StgMutVar" "var"

  ,size   [Cmm]      "StgAtomicallyFrame"
  ,offset [co,cr,cm] "StgAtomicallyFrame" "code"
  ,offset [co,cr,cm] "StgAtomicallyFrame" "next_invariant_to_check"
  ,offset [co,cr,cm] "StgAtomicallyFrame" "result"

  ,offset [co,cr,cm] "StgInvariantCheckQueue" "invariant"
  ,offset [co,cr,cm] "StgInvariantCheckQueue" "my_execution"
  ,offset [co,cr,cm] "StgInvariantCheckQueue" "next_queue_entry"

  ,offset [co,cr,cm] "StgAtomicInvariant" "code"

  ,offset [co,cr,cm] "StgTRecHeader" "enclosing_trec"

  ,size   [Cmm]      "StgCatchSTMFrame"
  ,offset [co,cr,cm] "StgCatchSTMFrame" "handler"
  ,offset [co,cr,cm] "StgCatchSTMFrame" "code"

  ,size   [Cmm]      "StgCatchRetryFrame"
  ,offset [co,cr,cm] "StgCatchRetryFrame" "running_alt_code"
  ,offset [co,cr,cm] "StgCatchRetryFrame" "first_code"
  ,offset [co,cr,cm] "StgCatchRetryFrame" "alt_code"

  ,offset [co,cr,cm] "StgTVarWatchQueue" "closure"
  ,offset [co,cr,cm] "StgTVarWatchQueue" "next_queue_entry"
  ,offset [co,cr,cm] "StgTVarWatchQueue" "prev_queue_entry"

  ,size   [Cmm]      "StgTVar"
  ,offset [co,cr,cm] "StgTVar" "current_value"
  ,offset [co,cr,cm] "StgTVar" "first_watch_queue_entry"
  ,offset [co,cr,cm] "StgTVar" "num_updates"

  ,size   [Cmm]      "StgWeak"
  ,offset [co,cr,cm] "StgWeak" "link"
  ,offset [co,cr,cm] "StgWeak" "key"
  ,offset [co,cr,cm] "StgWeak" "value"
  ,offset [co,cr,cm] "StgWeak" "finalizer"
  ,offset [co,cr,cm] "StgWeak" "cfinalizers"

  ,size   [Cmm]      "StgCFinalizerList"
  ,offset [co,cr,cm] "StgCFinalizerList" "link"
  ,offset [co,cr,cm] "StgCFinalizerList" "fptr"
  ,offset [co,cr,cm] "StgCFinalizerList" "ptr"
  ,offset [co,cr,cm] "StgCFinalizerList" "eptr"
  ,offset [co,cr,cm] "StgCFinalizerList" "flag"

  ,size   [Cmm]      "StgMVar"
  ,offset [co,cr,cm] "StgMVar" "head"
  ,offset [co,cr,cm] "StgMVar" "tail"
  ,offset [co,cr,cm] "StgMVar" "value"

  ,size   [Cmm]      "StgMVarTSOQueue"
  ,offset [co,cr,cm] "StgMVarTSOQueue" "link"
  ,offset [co,cr,cm] "StgMVarTSOQueue" "tso"

  ,size   [Cmm]      "StgBCO"
  ,offset [co,cr,cm] "StgBCO" "instrs"
  ,offset [co,cr,cm] "StgBCO" "literals"
  ,offset [co,cr,cm] "StgBCO" "ptrs"
  ,offset [co,cr,cm] "StgBCO" "arity"
  ,offset [co,cr,cm] "StgBCO" "size"
  ,offset [co,cmp]   "StgBCO" "bitmap"

  ,size   [Cmm]      "StgStableName"
  ,offset [co,cr,cm] "StgStableName" "sn"

  ,size   [Cmm]      "StgBlockingQueue"
  ,offset [co,cr,cm] "StgBlockingQueue" "bh"
  ,offset [co,cr,cm] "StgBlockingQueue" "owner"
  ,offset [co,cr,cm] "StgBlockingQueue" "queue"
  ,offset [co,cr,cm] "StgBlockingQueue" "link"

  ,size   [Cmm]      "MessageBlackHole"
  ,offset [co,cr,cm] "MessageBlackHole" "link"
  ,offset [co,cr,cm] "MessageBlackHole" "tso"
  ,offset [co,cr,cm] "MessageBlackHole" "bh"

  ,size [Haskell] "StgRetInfoTable"

  ,size   both    "StgInfoTable"

#if defined(TABLES_NEXT_TO_CODE)
  ,lit [Cmm]     "StgInfoTable_entry" [def "StgInfoTable_entry(ptr)" "(ptr)"]
  ,lit [Haskell] "lOAD_StgInfoTable_entry"
    ["lOAD_StgInfoTable_entry :: DynFlags -> CmmExpr -> CmmExpr"
    ,"lOAD_StgInfoTable_entry _dflags ptr = ptr"]

  ,offset [bro "code",br,cm] "StgInfoTable" "layout.payload.ptrs"
  ,offset [bro "code",br,cm] "StgInfoTable" "layout.payload.nptrs"
  ,offset [bro "code",br,cm] "StgInfoTable" "type"
  ,offset [hro "code",hr,hm] "StgInfoTable" "u.constr_tag"

#else
  ,offset [bo,br,bm] "StgInfoTable" "entry"
  ,offset [bo,br,cm] "StgInfoTable" "layout.payload.ptrs"
  ,offset [bo,br,cm] "StgInfoTable" "layout.payload.nptrs"
  ,offset [bo,br,cm] "StgInfoTable" "type"
  ,offset [ho,hr,hm] "StgInfoTable" "u.constr_tag"
#endif

#if defined(TABLES_NEXT_TO_CODE)
  ,offset [cro "i.code",cr,cm] "StgFunInfoTable" "f.slow_apply_offset"
  ,lit [Cmm] "StgFunInfoTable_slow_apply"
    ["#define StgFunInfoTable_slow_apply(ptr) \\"
    ," (TO_W_(StgFunInfoTable_slow_apply_offset(ptr)) + StgInfoTable_entry(ptr))"
    ]
  ,offset [cro "i.code",cr,cm] "StgFunInfoTable" "f.fun_type"
  ,offset [bro "i.code",br,bm] "StgFunInfoTable" "f.arity"
  ,offset [cro "i.code",cr,cm] "StgFunInfoTable" "f.b.bitmap"
#else
  ,offset [cro "i",cr,cm] "StgFunInfoTable" "f.slow_apply"
  ,offset [cro "i",cr,cm] "StgFunInfoTable" "f.fun_type"
  ,offset [bro "i",br,bm] "StgFunInfoTable" "f.arity"
  ,offset [cro "i",cr,cm] "StgFunInfoTable" "f.b.bitmap"
#endif
  ]
----------------------
-- [PROFILING only] --
----------------------
wanteds_profOnly = concat
  [sizeW  both       "StgProfHeader"
  ,offset [bo,br,bm] "StgClosure"     "header.prof.ccs"
  ,offset [bo,br,bm] "StgClosure"     "header.prof.hp.ldvw"
  ,offset [bo,br,bm] "StgTSO"         "prof.cccs"
  ,size   [Cmm]      "StgTSOProfInfo"
  ]

---------------------------------------------
-- End of wanteds definitions              --
-- Start of DeriveConstants Implementation --
---------------------------------------------
bro,hro,cro           :: String -> String -> String -> String -> Wanted
bo,co,br,hr,cr,bm,hm,cm,cmp,crg :: String -> String -> String -> Wanted
bo  = mkOffset_       both
--ho  = mkOffset_       [Haskell]
co  = mkOffset_       [Cmm]
bro = mkRelOffset_    both
hro = mkRelOffset_    [Haskell]
cro = mkRelOffset_    [Cmm]
br  = mkRep_          both
hr  = mkRep_          [Haskell]
cr  = mkRep_          [Cmm]
bm  = mkMacro_        both
hm  = mkMacro_        [Haskell]
cm  = mkMacro_        [Cmm]
cmp = mkMacroPayload_ [Cmm]
crg = mkRepGcptr_     [Cmm]
------------------
-- Control flow --
------------------

type Template = [Group]
data Group = Group [Wanted] Conditional
data Conditional = Error (String -> String)
                 | Derive
                 | If Define Then Conditional Else Conditional
data Then = Then
data Else = Else

template :: Template
template =
  [ Group wanteds_i Derive   -- [Insensitive]
  , Group wanteds_p $        -- [Sensitive to PROFILING]
      If Profiling Then Derive Else Derive
  , Group wanteds_profOnly $ -- [PROFILING only]
      If Profiling Then Derive Else $
       Error (++ " is not accessible when not compiling for PROFILING")
  ]

data Fst a b = Fst a
data Snd a b = Snd b

type Wanted = Constant Fst
type Result = Constant Snd

data Constant f = Constant String [Target] (Either' f)

data Either' f = Independant Independant
               | Dependant   (Dependant f)

isDependant,isIndependant :: Wanted -> Bool
isDependant (Constant _ _ Dependant{}) = True
isDependant _ = False
isIndependant = not . isDependant

--Platform Independant Constant
data Independant = Macro | MacroPayload | RepGcptr | Literal [String]



--Platform Dependant Constant
data Dependant f = Rep  (f CExpr   Integer) -- note [GetRep]
                 | Word (f CExpr   Integer)
                 | Int  (f CExpr   Integer)
                 | Nat  (f CExpr   Integer)
                 | Bool (f CPPExpr Bool   )

showType,showEmpty :: Dependant a -> String
showType Rep{}  = "Int"
showType Word{} = "Int"
showType Int{}  = "Int"
showType Nat{}  = "Integer"
showType Bool{} = "Bool"

--showEmpty, for initializing an empty PlatformConstants type
showEmpty Rep{}  = "0"
showEmpty Word{} = "0"
showEmpty Int{}  = "0"
showEmpty Nat{}  = "0"
showEmpty Bool{} = "False"


-- [GetRep]
-- GetRep is for defining REP_x to be b32 etc
-- These are both the C-- types used in a load
--    e.g.  b32[addr]
-- and the names of the CmmTypes in the compiler
--    b32 :: CmmType

data Target = Haskell | Cmm deriving Eq
both :: [Target]
both = [Haskell, Cmm]

data Define = Profiling deriving (Eq, Show)

showDefine :: Define -> String
showDefine Profiling = "PROFILING"

newtype CExpr = CExpr     String
newtype CPPExpr = CPPExpr String

size,sizeW :: [Target] -> String -> [Wanted]
size ts theType = [mkWord ts name expr]
  where
    name = "SIZEOF_"    ++ theType
    expr = "TYPE_SIZE(" ++ theType ++ ")"

sizeW ts theType = [mkWord ts name expr]
  where
    name = "SIZEOFW_"    ++ theType
    expr = "TYPE_SIZEW(" ++ theType ++ ")"

lit :: [Target] -> String -> [String] -> [Wanted]
lit ts nm strs  = [Constant nm ts $ Independant $ Literal strs]

mkOffset_,mkRep_,mkMacro_, mkMacroPayload_, mkRepGcptr_
             :: [Target]           -> String -> String -> String -> Wanted
mkRelOffset_ :: [Target] -> String -> String -> String -> String -> Wanted
mkRelOffset_ ts theField' theType theField nameBase = mkInt ts name expr
  where
    name = "OFFSET_" ++ nameBase
    expr = "(offsetof(" ++ theType  ++ "," ++ theField  ++ "))"
     ++ " - (offsetof(" ++ theType  ++ "," ++ theField' ++ "))"
mkOffset_ ts theType theField nameBase = mkWord ts name expr
  where
    name = "OFFSET_" ++ nameBase
    expr = "offsetof(" ++ theType ++ "," ++ theField ++ ")"
mkRep_ ts theType theField nameBase = mkRep ts name expr
  where
    name = "REP_" ++ nameBase
    expr = "FIELD_SIZE(" ++ theType ++ ", " ++ theField ++ ")"

mkMacro_        ts _ty _fld nm = Constant nm ts $ Independant Macro
mkMacroPayload_ ts _ty _fld nm = Constant nm ts $ Independant MacroPayload
mkRepGcptr_     ts _ty _fld nm = Constant nm ts $ Independant RepGcptr

mkRep, mkInt, mkWord, mkNat, mkBool :: [Target] -> String -> String -> Wanted
mkRep  ts n e = Constant n ts $ Dependant $ Rep  $ Fst $ CExpr   e
mkInt  ts n e = Constant n ts $ Dependant $ Int  $ Fst $ CExpr   e
mkWord ts n e = Constant n ts $ Dependant $ Word $ Fst $ CExpr   e
mkNat  ts n e = Constant n ts $ Dependant $ Nat  $ Fst $ CExpr   e
mkBool ts n e = Constant n ts $ Dependant $ Bool $ Fst $ CPPExpr e

offset_ :: [String->String->String->Wanted]->String->String->String->[Wanted]
offset  :: [String->String->String->Wanted]->String->String->        [Wanted]
offset_ fs theType theField nameBase = [f theType theField nameBase | f <- fs]
offset  fs theType theField          = offset_ fs theType theField nameBase
  where nameBase = theType ++ "_" ++ (deleteDots $ theField)

getResults :: [Define] -> [Wanted] -> IO [Result]
getResults ds wanteds = do
  tmpdir   <- requireOption "tmpdir"       o_tmpdir
  gccProg  <- requireOption "gcc program"  o_gccProg
  nmProg   <- requireOption "nm program"   o_nmProg
  verbose  <- getOption                    o_verbose
  gccFlags <- getOption                    o_gccFlags
  let cFile = tmpdir </> "tmp.c"
      oFile = tmpdir </> "tmp.o"
      gccFlags' = gccFlags ++ ["-c", cFile, "-o", oFile]
  writeFile cFile $ unlines $
    ["//autogenerated file see DeriveConstants.hs"
    ,"#define IN_STG_CODE 0"
    ,"#define THREADED_RTS"
    ,"#include \"PosixSource.h\""
    ,"#include \"Rts.h\""
    ,"#include \"Stable.h\""
    ,"#include \"Capability.h\""
    ,""
    ,"#include <inttypes.h>"
    ,"#include <stddef.h>"
    ,"#include <stdio.h>"
    ,"#include <string.h>"
    ,""
    ,"#define TYPE_SIZE(type) (sizeof(type))"
    ,"#define TYPE_SIZEW(type) (sizeofW(type))"
    ,"#define FIELD_SIZE(s_type, field) \\"
    ,"((size_t)sizeof(((s_type*)0)->field))"
    ,""
    ,"#pragma GCC poison sizeof sizeofw"
    ] ++ concatMap doWanted wanteds
  execute verbose gccProg $ gccFlags' ++ map (("-D" ++) . showDefine) ds
  xs <- readProcess nmProg [oFile] ""
  let ls = lines xs
      ms = map parseNmLine ls
      m = Map.fromList $ catMaybes ms
  mapM (lookupResult m) wanteds
  where
    prefix = "derivedConstant"
    -- We add 1 to the value, as some platforms will make a symbol
    -- of size 1 when for
    --     char foo[0];
    -- We then subtract 1 again when parsing.
    doWanted (Constant _name _ts Independant{}) = []
    doWanted (Constant  name _ts (Dependant e)) = doExpr (prefix ++ name) e
    doExpr n (Rep  (Fst (CExpr cExpr)))
      = ["char " ++ n ++ "[1 + " ++ cExpr ++ "];"]
    doExpr n (Word (Fst (CExpr cExpr)))
      = ["char " ++ n ++ "[1 + " ++ cExpr ++ "];"]
    doExpr n (Int  (Fst (CExpr cExpr)))
      = ["char " ++ n ++ "Mag[1 + ((intptr_t)(" ++ cExpr ++ ") >= 0 ? ("
         ++ cExpr ++ ") : -(" ++ cExpr ++ "))];"
        ,"char " ++ n ++ "Sig[(intptr_t)(" ++ cExpr ++ ") >= 0 ? 3 : 1];"
        ]
    doExpr n (Nat  (Fst (CExpr cExpr)))
      = -- These casts fix "right shift count >= width of type"
              -- warnings
      let cExpr' = "(uint64_t)(size_t)(" ++ cExpr ++ ")"
      in ["char " ++ n ++ "0[1 + ((" ++ cExpr' ++ ") & 0xFFFF)];"
         ,"char " ++ n ++ "1[1 + (((" ++ cExpr' ++ ") >> 16) & 0xFFFF)];"
         ,"char " ++ n ++ "2[1 + (((" ++ cExpr' ++ ") >> 32) & 0xFFFF)];"
         ,"char " ++ n ++ "3[1 + (((" ++ cExpr' ++ ") >> 48) & 0xFFFF)];"
         ]
    doExpr n (Bool (Fst (CPPExpr cppExpr)))
      = ["#if " ++ cppExpr,
         "char " ++ n ++ "[1];",
         "#else",
         "char " ++ n ++ "[2];",
         "#endif"]

    -- parseNmLine parses nm output that looks like
    -- "0000000b C derivedConstantMAX_Vanilla_REG"
    -- and returns ("MAX_Vanilla_REG", 11)
    parseNmLine xs0 = case break (' ' ==) xs0 of
      (x1, ' ' : xs1) ->
        case break (' ' ==) xs1 of
          (x2, ' ' : x3) ->
            case readHex x1 of
              [(size_got, "")] ->
                case x2 of
                  "C" ->
                    let x3' = case x3 of
                          '_' : rest -> rest
                          _          -> x3
                    in case stripPrefix prefix x3' of
                      Just name -> Just (name, size_got)
                      _ -> Nothing
                  _ -> Nothing
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing

    -- If an Int value is larger than 2^28 or smaller
    -- than -2^28, then fail.
    -- This test is a bit conservative, but if any
    -- constants are roughly maxBound or minBound then
    -- we probably need them to be Integer rather than
    -- Int so that -- cross-compiling between 32bit and
    -- 64bit platforms works.
    lookupSmall :: Map String Integer -> String -> IO Integer
    lookupSmall m name
          = case Map.lookup name m of
              Just v
               | v >   2^(28 :: Int) ||
                 v < -(2^(28 :: Int)) ->
                  die ("Value too large for GetWord: " ++ show v)
               | otherwise -> return v
              Nothing -> die ("Can't find " ++ show name)

    lookupResult :: Map String Integer -> Wanted -> IO Result
    lookupResult _ (Constant n ts (Independant i)) =
      return $ Constant n ts (Independant i)
    lookupResult m (Constant name ts (Dependant expr)) = do
      val <- case expr of
               Word{} -> do v <- lookupSmall m name
                            return $ Word $ Snd $ v - 1
               Int{} ->  do mag <- lookupSmall m (name ++ "Mag")
                            sig <- lookupSmall m (name ++ "Sig")
                            return $ Int $ Snd $ (mag - 1) * (sig - 2)
               Nat{} ->  do v0 <- lookupSmall m (name ++ "0")
                            v1 <- lookupSmall m (name ++ "1")
                            v2 <- lookupSmall m (name ++ "2")
                            v3 <- lookupSmall m (name ++ "3")
                            let v = (v0 - 1)
                                  + shiftL (v1 - 1) 16
                                  + shiftL (v2 - 1) 32
                                  + shiftL (v3 - 1) 48
                            return $ Nat $ Snd v
               Bool{} -> do v <- lookupSmall m name
                            case v of
                              1 -> return $ Bool $ Snd True
                              2 -> return $ Bool $ Snd False
                              _ -> die ("Bad boolean: " ++ show v)
               Rep{} -> do  v <- lookupSmall m name
                            return $ Rep $ Snd $ v - 1
      return $ Constant name ts (Dependant val)

filterWanteds :: [Wanted] -> Target -> [Wanted]
filterWanteds ws t = [Constant n ts x | Constant n ts x <- ws, t `elem` ts]

writeHaskellType :: IO ()
writeHaskellType = do
 outFile <- requireOption "no output file" o_outputFile
 groupType <- mapM (doGroup ((" :: " ++) . showType)) template
 groupEmpty <- mapM (doGroup ((" = " ++) . showEmpty)) template
 writeFile outFile $ unlines
  ["data PlatformConstants = PlatformConstants {"
  ,unlines $ indent 2 $ commas $ concat $ groupType
  ,"} deriving Read"
  ,""
  ,"emptyPlatformConstants :: PlatformConstants"
  ,"emptyPlatformConstants = PlatformConstants {"
  ,unlines $ indent 2 $ commas $ concat $ groupEmpty
  ,"}"
  ]
 where
  doGroup f (Group ws conditional) =
   doConditional conditional startState{cs_derive = derive}
   where
    derive defines = return $ concatMap doWanted $ filterWanteds ws Haskell
     where
      doWanted (Constant _n _t (Independant _i))   = []
      doWanted (Constant name _t (Dependant expr)) =
       ["pc" ++ concatMap show defines ++ "_" ++ name ++ f expr]

writeHaskellWrappers :: (Wanted -> Bool) -> IO ()
writeHaskellWrappers p = do
 outFile <- requireOption "no output file" o_outputFile
 mapM doGroup template >>= (writeFile outFile . unlines . concat)
 where
  doGroup (Group ws conditional) = mapM doWanted
    (filter p $ filterWanteds ws Haskell) >>= (return . concat)
   where
    doWanted (Constant name _t (Independant Macro)) = return
      ["lOAD_" ++ name ++ " :: DynFlags -> CmmExpr -> CmmExpr"
      ,"sTORE_" ++ name ++ " :: DynFlags -> CmmExpr -> CmmExpr -> CmmAGraph"
      ,"lOAD_" ++ name ++ " dflags ptr ="
      ,"  CmmLoad (cmmOffsetB dflags ptr (oFFSET_" ++ name ++ " dflags)) $"
      ,"    cmmBits $ widthFromBytes $ rEP_" ++ name ++ " dflags"
      ,"sTORE_" ++ name ++ " dflags ptr val ="
      ,"  mkStore (cmmOffsetB dflags ptr (oFFSET_" ++ name ++ " dflags)) val"]
    doWanted (Constant _ _ (Independant (Literal strs))) = return strs
    doWanted (Constant _ _ (Independant _)) = error "writeHaskellWrappers:Indie"
    doWanted (Constant name _t (Dependant expr)) = do
      conds <- doConditional conditional startState {cs_derive     = derive
                                                    ,cs_error      = err
                                                    ,cs_ifThenElse = ifThenElse
                                                    }
      return $ [haskellise name ++ " :: DynFlags -> " ++ showType expr
               ,haskellise name ++ " dflags = "] ++ indent 2 conds
      where
       ifThenElse Profiling        = f "gopt Opt_SccProfilingOn dflags"
       f a b c = ["if " ++ a ++ " then ("]
              ++ indent 2 b
              ++ [") else ("]
              ++ indent 2 c
              ++ [")"]
       err g = ["error \"" ++ g name ++ "\""]
       derive ds = return $ ["pc" ++ concatMap show ds ++ "_" ++ name
                             ++ " $ sPlatformConstants $ settings dflags"]

writeHaskellExports :: (Wanted -> Bool) -> IO ()
writeHaskellExports p = do
 outFile <- requireOption "no output file" o_outputFile
 writeFile outFile $ unlines $ indent 4 $ commas $ concatMap doGroup template
 where
  doGroup (Group ws _) = concatMap doWanted (filter p $ filterWanteds ws Haskell)
  doWanted (Constant name _ts (Independant Macro)) = ["lOAD_" ++ name
                                                     ,"sTORE_" ++ name]
  doWanted (Constant name _ts (Independant Literal{})) = [name]
  doWanted (Constant _ _ (Independant _)) = error "writeHaskellExports:Indie"
  doWanted (Constant name _ts _expr) = [haskellise name]

writeValues :: Target -> IO ()
writeValues Haskell = do
 outFile <- requireOption "no output file" o_outputFile
 groups <- mapM doGroup template
 writeFile outFile $ unlines
   ["PlatformConstants {"
   ,unlines $ indent 2 $ commas $ concat groups
   ,"}"
   ]
 where
  doGroup (Group ws conditional) =
   doConditional conditional startState{cs_derive = derive}
   where
    derive ds = do
      rs <- getResults ds $ filterWanteds ws Haskell
      return $ concatMap doResult rs
     where
      doResult (Constant _n   _t (Independant _i)) = []
      doResult (Constant name _ts (Dependant value)) =
        ["pc" ++ concatMap show ds ++ "_" ++ name ++ " = " ++ showVal value]
      showVal (Rep  (Snd v)) = show v
      showVal (Word (Snd v)) = show v
      showVal (Int  (Snd v)) = show v
      showVal (Nat  (Snd v)) = show v
      showVal (Bool (Snd v)) = show v

writeValues Cmm = do
  outFile  <- requireOption "no output file" o_outputFile
  groups <- mapM doGroup template
  writeFile outFile $ unlines $
    ["/* This file is created automatically.  Do not edit by hand.*/"
    ,unlines $ concat groups
    ]
 where
  doGroup (Group ws conditional) =
   doConditional conditional startState{cs_derive = derive,
                                        cs_error    = err,
                                        cs_ifThenElse = ifThenElse}
   where
    ifThenElse d b c = concat [["#ifdef " ++ showDefine d]
                              ,indent 2 $ b
                              ,["#else /* " ++ showDefine d ++ " */"]
                              ,indent 2 $ c
                              ,["#endif /* " ++ showDefine d ++ " */"]
                              ]
    err f = concatMap doResult $ filterWanteds ws Cmm
     where doResult (Constant name _ts _e)= [def name $ "#error " ++ f name]
    derive ds = do
     rs <- getResults ds $ filterWanteds ws Cmm
     return $ concatMap doResult rs
     where
      showVal (Rep  (Snd v)) = "b" ++ (show $ v * 8)
      showVal (Word (Snd v)) = show v
      showVal (Int  (Snd v)) = show v
      showVal (Nat  (Snd v)) = show v
      showVal (Bool (Snd v)) = show $ fromEnum v
      doResult (Constant name _ts (Dependant val)) = [def name $ showVal val]
      doResult (Constant name _ts (Independant typ)) = case typ of
        Literal strs -> strs
        RepGcptr -> [def ("REP_" ++ name) " gcptr"]
        Macro    -> [def x y]
         where x =  name ++ "(__ptr__)"
               y =  "REP_" ++ name ++ "[__ptr__+OFFSET_" ++ name ++ "]"
        MacroPayload ->  [def x y]
         where x = name ++ "(__ptr__,__ix__)"
               y = "W_[__ptr__ + OFFSET_" ++ name ++ " + WDS(__ix__)]"

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitFailure

execute :: Bool -> FilePath -> [String] -> IO ()
execute verbose prog args
 = do when verbose $ putStrLn $ showCommandForUser prog args
      ec <- rawSystem prog args
      unless (ec == ExitSuccess) $
          die ("Executing " ++ show prog ++ " failed")

main :: IO ()
main = do
  mode <- requireOption "mode" o_mode
  case mode of
    Gen_Haskell_Wrappers p -> writeHaskellWrappers p
    Gen_Haskell_Exports  p -> writeHaskellExports p
    Gen_Haskell_Type     -> writeHaskellType
    Gen_Values target    -> writeValues target

requireOption :: String -> (Options -> Maybe a) -> IO a
requireOption descr opt = do
  opts <- parseArgs
  case opt opts of
    Just x  -> return x
    Nothing -> die ("No " ++ descr ++ " given")

getOption :: (Options -> a) -> IO a
getOption opt = do
  opts <- parseArgs
  return $ opt opts

data Options = Options {
                   o_verbose :: Bool,
                   o_mode :: Maybe Mode,
                   o_tmpdir :: Maybe FilePath,
                   o_outputFile :: Maybe FilePath,
                   o_gccProg :: Maybe FilePath,
                   o_gccFlags :: [String],
                   o_nmProg :: Maybe FilePath
}

parseArgs :: IO Options
parseArgs = do args <- getArgs
               opts <- f emptyOptions args
               return (opts {o_gccFlags = reverse (o_gccFlags opts)})
    where
          f opts [] = return opts
          f opts ("-v" : args')
              = f (opts {o_verbose = True}) args'
          f opts ("--gen-haskell-type" : args')
              = f (opts {o_mode = Just Gen_Haskell_Type}) args'
          f opts ("--gen-haskell-dflags-wrappers" : args')
            = f (opts {o_mode = Just (Gen_Haskell_Wrappers isDependant)}) args'
          f opts ("--gen-haskell-dflags-exports" : args')
            = f (opts {o_mode = Just (Gen_Haskell_Exports isDependant)}) args'
          f opts ("--gen-haskell-codegen-wrappers" : args')
            = f (opts {o_mode = Just (Gen_Haskell_Wrappers isIndependant)}) args'
          f opts ("--gen-haskell-codegen-exports" : args')
            = f (opts {o_mode = Just (Gen_Haskell_Exports isIndependant)}) args'
          f opts ("--gen-header" : args')
              = f (opts {o_mode = Just (Gen_Values Cmm)}) args'
          f opts ("--gen-haskell-value" : args')
              = f (opts {o_mode = Just (Gen_Values Haskell)}) args'
          f opts ("--tmpdir" : dir : args')
              = f (opts {o_tmpdir = Just dir}) args'
          f opts ("-o" : fn : args')
              = f (opts {o_outputFile = Just fn}) args'
          f opts ("--gcc-program" : prog : args')
              = f (opts {o_gccProg = Just prog}) args'
          f opts ("--gcc-flag" : flag : args')
              = f (opts {o_gccFlags = flag : o_gccFlags opts}) args'
          f opts ("--nm-program" : prog : args')
              = f (opts {o_nmProg = Just prog}) args'
          f _ (flag : _) = die ("Unrecognised flag: " ++ show flag)
          emptyOptions = Options {
                             o_verbose = False,
                             o_mode = Nothing,
                             o_tmpdir = Nothing,
                             o_outputFile = Nothing,
                             o_gccProg = Nothing,
                             o_gccFlags = [],
                             o_nmProg = Nothing
                       }

data Mode = Gen_Haskell_Type
          | Gen_Haskell_Wrappers (Wanted -> Bool)
          | Gen_Haskell_Exports  (Wanted -> Bool)
          | Gen_Values Target
--------------------------------
-- Boring string manipulation --
--------------------------------

-- deleteDots "abcd.defg.hijk" = "hijk"
deleteDots :: String -> String
deleteDots = reverse . f . reverse
 where
  f [] = []
  f ('.':_) = []
  f (x:xs) = x : f xs

haskellise :: String -> String
haskellise (c : cs) = toLower c : cs
haskellise "" = ""

indent :: Int -> [String] -> [String]
indent n xs = map ((take n $ repeat ' ')++) xs

commas :: [String] -> [String]
commas   xs =  (' ' :    head xs)
        : (map (',' :) $ tail xs)

def :: String -> String -> String
def x y = "#define" ++ " " ++ x ++ " " ++ y

----------------------------------------------
-- Boring Stuff for Traversing Conditional  --
----------------------------------------------

doConditional :: Conditional -> ConditionalState -> IO [String]
doConditional (Error f) st  = return $ (cs_error st) f
doConditional Derive st =         (cs_derive st) (cs_defines st)
doConditional (If d Then c Else c') st = do
  trueString  <- doConditional c st{cs_defines=d:cs_defines st}
  falseString <- doConditional c' st
  return $ (cs_ifThenElse st) d trueString falseString

data ConditionalState = ConditionalState {
       cs_derive     :: [Define] -> IO [String]
      ,cs_error      :: (String -> String) -> [String]
      ,cs_ifThenElse :: Define -> [String] -> [String] -> [String]
      ,cs_defines    :: [Define]
      }

startState :: ConditionalState
startState = ConditionalState {
       cs_derive     = \_ -> return []
      ,cs_error      = \_ -> []
      ,cs_ifThenElse = \_ -> \a -> \b -> a++b
      ,cs_defines    = []
      }
