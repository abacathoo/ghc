{- ------------------------------------------------------------------------

(c) The GHC Team, 1992-2012

DeriveConstants is a program that extracts information from the C
declarations in the header files (primarily struct field offsets)
and generates various files, such as a header file that can be #included
into non-C source containing this information.

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

mixs :: [Mix]
mixs = concat
  [[mkWord both "PROF_HDR_SIZE"      "sizeofW(StgProfHeader)"]
  ,[mkWord both "HDR_SIZE"           "sizeofW(StgHeader)"]
  ,[mkWord both "BACKTRACE_HDR_SIZE" "sizeofW(StgBacktraceHeader)"]
  ,[mkWord both "THUNK_HDR_SIZE"     "sizeofW(StgThunkHeader)"]
  -- Size of a storage manager block (in bytes).
  ,[mkWord both  "BLOCK_SIZE"         "BLOCK_SIZE"]
  ,[mkWord [Cmm] "MBLOCK_SIZE"        "MBLOCK_SIZE"]

  -- blocks that fit in an MBlock, leaving space for the block descriptors
  ,[mkWord both "BLOCKS_PER_MBLOCK"  "BLOCKS_PER_MBLOCK"]
   -- could be derived, but better to save doing the calculation twice

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

  ,intOffset both    "stgEagerBlackholeInfo" "FUN_OFFSET(stgEagerBlackholeInfo)"
  ,intOffset both    "stgGCEnter1"           "FUN_OFFSET(stgGCEnter1)"
  ,intOffset both    "stgGCFun"              "FUN_OFFSET(stgGCFun)"

  -- The three offsets above are negative offsets from baseReg. see [baseReg layout].


  ,offset [bo]       "StgRegTable" "rR1"
  ,offset [bo]       "StgRegTable" "rR2"
  ,offset [bo]       "StgRegTable" "rR3"
  ,offset [bo]       "StgRegTable" "rR4"
  ,offset [bo]       "StgRegTable" "rR5"
  ,offset [bo]       "StgRegTable" "rR6"
  ,offset [bo]       "StgRegTable" "rR7"
  ,offset [bo]       "StgRegTable" "rR8"
  ,offset [bo]       "StgRegTable" "rR9"
  ,offset [bo]       "StgRegTable" "rR10"
  ,offset [bo]       "StgRegTable" "rF1"
  ,offset [bo]       "StgRegTable" "rF2"
  ,offset [bo]       "StgRegTable" "rF3"
  ,offset [bo]       "StgRegTable" "rF4"
  ,offset [bo]       "StgRegTable" "rF5"
  ,offset [bo]       "StgRegTable" "rF6"
  ,offset [bo]       "StgRegTable" "rD1"
  ,offset [bo]       "StgRegTable" "rD2"
  ,offset [bo]       "StgRegTable" "rD3"
  ,offset [bo]       "StgRegTable" "rD4"
  ,offset [bo]       "StgRegTable" "rD5"
  ,offset [bo]       "StgRegTable" "rD6"
  ,offset [bo]       "StgRegTable" "rXMM1"
  ,offset [bo]       "StgRegTable" "rXMM2"
  ,offset [bo]       "StgRegTable" "rXMM3"
  ,offset [bo]       "StgRegTable" "rXMM4"
  ,offset [bo]       "StgRegTable" "rXMM5"
  ,offset [bo]       "StgRegTable" "rXMM6"
  ,offset [bo]       "StgRegTable" "rYMM1"
  ,offset [bo]       "StgRegTable" "rYMM2"
  ,offset [bo]       "StgRegTable" "rYMM3"
  ,offset [bo]       "StgRegTable" "rYMM4"
  ,offset [bo]       "StgRegTable" "rYMM5"
  ,offset [bo]       "StgRegTable" "rYMM6"
  ,offset [bo]       "StgRegTable" "rZMM1"
  ,offset [bo]       "StgRegTable" "rZMM2"
  ,offset [bo]       "StgRegTable" "rZMM3"
  ,offset [bo]       "StgRegTable" "rZMM4"
  ,offset [bo]       "StgRegTable" "rZMM5"
  ,offset [bo]       "StgRegTable" "rZMM6"
  ,offset [bo]       "StgRegTable" "rL1"
  ,offset [bo]       "StgRegTable" "rSp"
  ,offset [bo]       "StgRegTable" "rSpLim"
  ,offset [bo]       "StgRegTable" "rHp"
  ,offset [bo]       "StgRegTable" "rHpLim"
  ,offset [bo]       "StgRegTable" "rCCCS"
  ,offset [bo]       "StgRegTable" "rCurrentBacktrace"
  ,offset [bo]       "StgRegTable" "rCurrentTSO"
  ,offset [bo]       "StgRegTable" "rCurrentNursery"
  ,offset [bo]       "StgRegTable" "rHpAlloc"
  ,offset [bo,br,cm] "StgRegTable" "rRet"
  ,offset [bo,br,cm] "StgRegTable" "rNursery"

  ,offset [bo]       "Capability"  "r"
  ,offset [bo]       "Capability"  "lock"
  ,offset [bo,br,cm] "Capability"  "no"
  ,offset [bo,br,cm] "Capability"  "mut_lists"
  ,offset [bo,br,cm] "Capability"  "context_switch"
  ,offset [bo,br,cm] "Capability"  "interrupt"
  ,offset [bo,br,cm] "Capability"  "sparks"

  ,offset [bo,br,cm] "bdescr"      "start"
  ,offset [bo,br,cm] "bdescr"      "free"
  ,offset [bo,br,cm] "bdescr"      "blocks"
  ,offset [bo,br,cm] "bdescr"      "gen_no"
  ,offset [bo,br,cm] "bdescr"      "link"

  ,size   [Cmm]      "generation"
  ,offset [bo,br,cm] "generation"  "n_new_large_words"
  ,offset [bo,br,cm] "generation"  "weak_ptr_list"

  ,size   both       "CostCentreStack"
  ,offset [bo,br,cm] "CostCentreStack"    "ccsID"
  ,offset [bo,br,cm] "CostCentreStack"    "mem_alloc"
  ,offset [bo,br,cm] "CostCentreStack"    "scc_count"
  ,offset [bo,br,cm] "CostCentreStack"    "prevStack"

  ,offset [bo,br,cm] "CostCentre"         "ccID"
  ,offset [bo,br,cm] "CostCentre"         "link"

  ,offset [bo,br,cm] "StgBacktraceHeader" "bt"

  ,offset [bo,cmp]   "StgClosure"         "payload"

  ,offset [bo,br,cm] "StgEntCounter"      "allocs"
  ,offset [bo,br,cm] "StgEntCounter"      "allocd"
  ,offset [bo,br,cm] "StgEntCounter"      "registeredp"
  ,offset [bo,br,cm] "StgEntCounter"      "link"
  ,offset [bo,br,cm] "StgEntCounter"      "entry_count"

  ,size   both    "StgUpdateFrame"
  ,size   [Cmm]   "StgCatchFrame"
  ,size   [Cmm]   "StgStopFrame"

  ,size   both       "StgMutArrPtrs"
  ,offset [bo,br,cm] "StgMutArrPtrs" "ptrs"
  ,offset [bo,br,cm] "StgMutArrPtrs" "size"

  ,size   both       "StgArrWords"
  ,offset [bo,br,cm] "StgArrWords"   "bytes"
  ,offset [bo,cmp]   "StgArrWords"   "payload"

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
  ,profOnly $
   offset_ [bo,cr,cm] "StgTSO" "prof.cccs" "StgTSO_cccs"
  ,offset  [bo,cr,cm] "StgTSO" "stackobj"

  ,offset [bo,br]    "StgStack"    "sp"
  ,offset [bo]       "StgStack"    "stack"
  ,offset [co]       "StgStack"    "stack_size"
  ,offset [co,cr,cm] "StgStack"    "dirty"

  ,profOnly $
   size   [Cmm]      "StgTSOProfInfo"

  ,offset [bo,cr,cm] "StgUpdateFrame" "updatee"

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

  ,size    [Cmm]      "StgFunInfoExtraFwd"
  ,offset  [co,cr,cm] "StgFunInfoExtraFwd" "slow_apply"
  ,offset  [co,cr,cm] "StgFunInfoExtraFwd" "fun_type"
  ,offset  [co,cr,cm] "StgFunInfoExtraFwd" "arity"
  ,offset_ [co,cr,cm] "StgFunInfoExtraFwd" "b.bitmap"
                      "StgFunInfoExtraFwd_bitmap"

  ,size    both       "StgFunInfoExtraRev"
  ,offset  [co,cr,cm] "StgFunInfoExtraRev" "slow_apply_offset"
  ,offset  [co,cr,cm] "StgFunInfoExtraRev" "fun_type"
  ,offset  [co,cr,cm] "StgFunInfoExtraRev" "arity"
  ,offset_ [co,cr,cm] "StgFunInfoExtraRev" "b.bitmap"
                      "StgFunInfoExtraRev_bitmap"

  ,offset  [co,cr,cm] "StgLargeBitmap" "size"
  ,offset  [co,cm]    "StgLargeBitmap" "bitmap"

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
   -- The amount of ([Haskell]) stack to leave free for saving
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
  where
    bo  = doOffset_ both      --Haskell and Cmm  offset
    ho  = doOffset_ [Haskell] --Haskell          offset
    co  = doOffset_ [Cmm]     --Cmm              offset
    br  = mkRep_    both      --Haskell and Cmm  rep
    hr  = mkRep_    [Haskell] --Haskell          rep
    cr  = mkRep_    [Cmm]     --Cmm              rep
    cm  = mkMacro_            --Cmm              macro
    cmp = mkMacroPayload_     --Cmm              payload macro
    crg = mkRepGcptr_         --Cmm              gcptr rep
    profOnly :: [Mix] -> [Mix]
    profOnly xs = map f xs
      where f (Left (lang,(_way, wanted))) = Left (lang,([Profiling], wanted))
            f x                            = x

data Fst a b = Fst a
data Snd a b = Snd b

--Platform Independant Constants
data Indie  = Macro        Name
            | MacroPayload Name
            | RepGcptr     Name

--Platform Dependant Constants
type Wanted = ([Way],Named Fst)
type Result = Named Snd

data Named   f = Named Name (ExprVal f)
data ExprVal f = Rep  (f CExpr   Integer) -- note [GetRep]
               | Word (f CExpr   Integer)
               | Int  (f CExpr   Integer)
               | Nat  (f CExpr   Integer)
               | Bool (f CPPExpr Bool   )

-- [GetRep]
-- GetRep is for defining REP_x to be b32 etc
-- These are both the C-- types used in a load
--    e.g.  b32[addr]
-- and the names of the CmmTypes in the compiler
--    b32 :: CmmType

data Target = Haskell | Cmm deriving Eq
both :: [Target]
both = [Haskell, Cmm]

data Way    = Standard | Profiling deriving (Eq, Show)
allWays :: [Way]
allWays = [Standard, Profiling]

type Name = String
newtype CExpr = CExpr String
newtype CPPExpr = CPPExpr String

type Mix = Either ([Target],Wanted) Indie
wanteds :: Target -> [Wanted]
indies  :: [Indie]

indies = do
  Right indie <- mixs
  return indie

wanteds target = do
  Left (targets, wanted) <- mixs
  if target `elem` targets then
    return wanted
  else []

intOffset :: [Target] -> Name -> String -> [Mix]
intOffset ts nameBase cExpr = [mkInt ts ("OFFSET_" ++ nameBase) cExpr]

doOffset_,mkRep_ ::[Target] -> String -> String -> Name -> Mix
doOffset_ ts theType theField nameBase = mkWord ts name expr
  where
    name = "OFFSET_" ++ nameBase
    expr = "offsetof(" ++ theType ++ "," ++ theField ++ ")"
mkRep_ ts theType theField nameBase = mkRep ts name expr
  where
    name = "REP_" ++ nameBase
    expr = "FIELD_SIZE(" ++ theType ++ ", " ++ theField ++ ")"

size :: [Target] -> String -> [Mix]
size ts theType = [mkWord ts name expr]
  where
    name = "SIZEOF_"    ++ theType
    expr = "TYPE_SIZE(" ++ theType ++ ")"

mkMacro_, mkMacroPayload_, mkRepGcptr_
  :: String -> String -> Name-> Mix
mkMacro_        _theType _theField name = Right $ Macro        name
mkMacroPayload_ _theType _theField name = Right $ MacroPayload name
mkRepGcptr_     _theType _theField name = Right $ RepGcptr     name

mkRep, mkInt, mkWord, mkNat, mkBool
  :: [Target] -> Name -> String -> Mix
mkRep  ts name expr = Left (ts, (allWays, Named name $ Rep  $ Fst $ CExpr   expr))
mkInt  ts name expr = Left (ts, (allWays, Named name $ Int  $ Fst $ CExpr   expr))
mkWord ts name expr = Left (ts, (allWays, Named name $ Word $ Fst $ CExpr   expr))
mkNat  ts name expr = Left (ts, (allWays, Named name $ Nat  $ Fst $ CExpr   expr))
mkBool ts name expr = Left (ts, (allWays, Named name $ Bool $ Fst $ CPPExpr expr))

offset_ :: [String -> String -> Name -> Mix] -> String -> String -> Name -> [Mix]
offset  :: [String -> String -> Name -> Mix] -> String -> String ->         [Mix]
offset_ fs theType theField nameBase = [f theType theField nameBase | f <- fs ]
offset  fs theType theField          = offset_ fs theType theField nameBase
  where nameBase = theType ++ "_" ++ theField

getResults :: Way -> Target -> IO [Result]
getResults way target = do
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
    ,"#define FIELD_SIZE(s_type, field) \\"
    ,"((size_t)sizeof(((s_type*)0)->field))"
    ,"#define TYPE_SIZE(type) (sizeof(type))"
    ,"#define FUN_OFFSET(sym)\\"
    ,"(offsetof(Capability,f.sym) - offsetof(Capability,r))"
    ,""
    ,"#pragma GCC poison sizeof"
    ] ++ concatMap doWanted theseWanteds

  execute verbose gccProg (gccFlags' ++ wayFlags)
  xs <- readProcess nmProg [oFile] ""
  let ls = lines xs
      ms = map parseNmLine ls
      m = Map.fromList $ catMaybes ms
  mapM (lookupResult m) theseWanteds
  where
    wayFlags = case way of Standard -> []
                           Profiling -> ["-DPROFILING"]
    theseWanteds = do
      (ways, wanted) <- wanteds target
      if way `elem` ways then return wanted else []

    prefix = "derivedConstant"
    -- We add 1 to the value, as some platforms will make a symbol
    -- of size 1 when for
    --     char foo[0];
    -- We then subtract 1 again when parsing.
    doWanted (Named name expr) = doExpr (prefix ++ name) expr
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
              [(size, "")] ->
                case x2 of
                  "C" ->
                    let x3' = case x3 of
                          '_' : rest -> rest
                          _          -> x3
                    in case stripPrefix prefix x3' of
                      Just name -> Just (name, size)
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
    lookupSmall :: Map String Integer -> Name -> IO Integer
    lookupSmall m name
          = case Map.lookup name m of
              Just v
               | v >   2^(28 :: Int) ||
                 v < -(2^(28 :: Int)) ->
                  die ("Value too large for GetWord: " ++ show v)
               | otherwise -> return v
              Nothing -> die ("Can't find " ++ show name)

    lookupResult :: Map String Integer -> Named Fst -> IO Result
    lookupResult m (Named name (Word{}))
          = do v <- lookupSmall m name
               return $ Named name $ Word $ Snd $ v - 1
    lookupResult m (Named name (Int{}))
          = do mag <- lookupSmall m (name ++ "Mag")
               sig <- lookupSmall m (name ++ "Sig")
               return $ Named name $ Int $ Snd $ (mag - 1) * (sig - 2)
    lookupResult m (Named name (Nat{}))
          = do v0 <- lookupSmall m (name ++ "0")
               v1 <- lookupSmall m (name ++ "1")
               v2 <- lookupSmall m (name ++ "2")
               v3 <- lookupSmall m (name ++ "3")
               let v = (v0 - 1)
                       + shiftL (v1 - 1) 16
                       + shiftL (v2 - 1) 32
                       + shiftL (v3 - 1) 48
               return $ Named name $ Nat $ Snd v
    lookupResult m (Named name (Bool{}))
          = do v <- lookupSmall m name
               case v of
                    1 -> return $ Named name $ Bool $ Snd True
                    2 -> return $ Named name $ Bool $ Snd False
                    _ -> die ("Bad boolean: " ++ show v)
    lookupResult m (Named name (Rep{}))
          = do v <- lookupSmall m name
               return $ Named name $ Rep $ Snd $ v - 1

showType :: ExprVal a -> String
showType Rep{}  = "Int"
showType Word{} = "Int"
showType Int{}  = "Int"
showType Nat{}  = "Integer"
showType Bool{} = "Bool"

haskellise :: Name -> Name
haskellise (c : cs) = toLower c : cs
haskellise "" = ""

writeHaskellType :: IO ()
writeHaskellType = do
  outFile <- requireOption "no output file" o_outputFile
  writeFile outFile $ unlines
    ["data PlatformConstants = PlatformConstants {"
               -- Now a kludge that allows the real entries to
               -- all start with a comma, which makes life a
               -- little easier
    ,"    pc_platformConstants :: ()"
    , unlines $ concatMap doNamed $ map snd $ wanteds Haskell
    ,"  } deriving Read"
    ]
  where
    doNamed (Named name typ) = ["    , pc_" ++ name ++ " :: " ++ showType typ]

writeHaskellWrappers :: IO ()
writeHaskellWrappers = do
  outFile <- requireOption "no output file" o_outputFile
  writeFile outFile $ unlines $ concatMap doNamed $ map snd $ wanteds Haskell
  where
    doNamed (Named _name (Rep {})) = []
    doNamed (Named name typ) =
      [haskellise name ++ " :: DynFlags -> " ++ showType typ
      ,haskellise name ++ " dflags = pc_" ++ name
       ++ " (sPlatformConstants (settings dflags))"
      ]

writeHaskellExports :: IO ()
writeHaskellExports = do
  outFile <- requireOption "no output file" o_outputFile
  writeFile outFile $ unlines $ concatMap doNamed $ map snd $ wanteds Haskell
  where
    doNamed (Named _name (Rep {} )) = []
    doNamed (Named name _value    ) = ["    " ++ haskellise name ++ ","]

withoutWay :: Way -> [([Way], a)] -> [a]
withoutWay way xs = do
  (ways, x) <- xs
  if way `elem` ways then [] else return x

assertNoWithoutWay :: Way -> [([Way], a)] -> String
assertNoWithoutWay way xs = do
  if null $ withoutWay way xs then ""
  else error ("There are some wanteds without " ++ show way)

writeValues :: Target -> IO ()
writeValues Haskell = do
  outFile <- requireOption "no output file" o_outputFile
  results_profiling <- getResults Profiling Haskell
  results_standard  <- getResults Standard  Haskell
  writeFile outFile $ unlines
    ["platformConstants_profiling :: PlatformConstants"
    ,"platformConstants_profiling =  PlatformConstants{"
    ,"  pc_platformConstants = ()"
    ,unlines $ concatMap doResult results_profiling
    ,assertNoWithoutWay Profiling (wanteds Haskell)
    ,"}"
    ,"platformConstants_standard :: PlatformConstants"
    ,"platformConstants_standard =  PlatformConstants{"
    ,"  pc_platformConstants = ()"
    ,unlines $ concatMap doResult   results_standard
    ,unlines $ concatMap doProfOnly $ withoutWay Standard $ wanteds Haskell
    ,"}"
    ]
  where
    mkError name = "error \"" ++ name ++ " is for profiling only\""
    doProfOnly (Named name _v) =  ["  , pc_" ++ name ++ " = " ++ mkError name]
    doResult (Named name value) = ["  , pc_" ++ name ++ " = " ++ showVal value]
    showVal (Rep  (Snd v)) = show v
    showVal (Word (Snd v)) = show v
    showVal (Int  (Snd v)) = show v
    showVal (Nat  (Snd v)) = show v
    showVal (Bool (Snd v)) = show v

writeValues Cmm = do
  outFile <- requireOption "no output file" o_outputFile
  results_profiling <- getResults Profiling Cmm
  results_standard  <- getResults  Standard Cmm
  writeFile outFile $ unlines
      ["/* This file is created automatically.  Do not edit by hand.*/"
      ,"#if defined(PROF)"
      , unlines $ concatMap doResult results_profiling
      , assertNoWithoutWay Profiling $ wanteds Cmm
      ,"#else /*PROF*/"
      , unlines $ concatMap doResult   results_standard
      , unlines $ concatMap doProfOnly $ withoutWay Standard $ wanteds Cmm
      ,"#endif"
      , unlines $ concatMap doIndie indies
      ]
  where
    errortext = "error not defined when prof is off"
    doProfOnly (Named name _expr) = [def name errortext]
    doResult (Named name value) = [def name $ showVal value]
    showVal (Rep  (Snd v)) = "b" ++ show (v * 8)
    showVal (Word (Snd v)) = show v
    showVal (Int  (Snd v)) = show v
    showVal (Nat  (Snd v)) = show v
    showVal (Bool (Snd v)) = show $ fromEnum v

    doIndie (RepGcptr nameBase) =        [def ("REP_" ++ nameBase) " gcptr"]
    doIndie (Macro nameBase) =           [def x y]
      where x = nameBase ++ "(__ptr__)"
            y = "REP_" ++ nameBase ++ "[__ptr__+OFFSET_" ++ nameBase ++ "]"
    doIndie (MacroPayload nameBase) =    [def x y]
      where x = nameBase ++ "(__ptr__,__ix__)"
            y = "W_[__ptr__ + OFFSET_" ++ nameBase ++ " + WDS(__ix__)]"

    def x y =                          "#define" ++ " " ++ x ++ " " ++ y

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
    Gen_Haskell_Type     -> writeHaskellType
    Gen_Haskell_Wrappers -> writeHaskellWrappers
    Gen_Haskell_Exports  -> writeHaskellExports
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
    where emptyOptions = Options {
                             o_verbose = False,
                             o_mode = Nothing,
                             o_tmpdir = Nothing,
                             o_outputFile = Nothing,
                             o_gccProg = Nothing,
                             o_gccFlags = [],
                             o_nmProg = Nothing
                         }
          f opts [] = return opts
          f opts ("-v" : args')
              = f (opts {o_verbose = True}) args'
          f opts ("--gen-haskell-type" : args')
              = f (opts {o_mode = Just Gen_Haskell_Type}) args'
          f opts ("--gen-haskell-wrappers" : args')
              = f (opts {o_mode = Just Gen_Haskell_Wrappers}) args'
          f opts ("--gen-haskell-exports" : args')
              = f (opts {o_mode = Just Gen_Haskell_Exports}) args'
          f opts ("--gen-cmm-values" : args')
              = f (opts {o_mode = Just (Gen_Values Cmm)}) args'
          f opts ("--gen-haskell-values" : args')
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

data Mode = Gen_Haskell_Type
          | Gen_Haskell_Wrappers
          | Gen_Haskell_Exports
          | Gen_Values Target
