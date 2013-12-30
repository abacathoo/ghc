  --------------------------------------------------
  -- Code generation for backtraces               --
  --                                              --
  -- (c) William Kenyon 2013                      --
  --------------------------------------------------
module StgCmmBacktrace (emitPushTracepoint, initTracepoints,
                        curBacktrace,
                        saveCurrentBacktrace, restoreCurrentBacktrace,
                        enterBacktraceThunk, enterBacktraceFun) where

#include "HsVersions.h"
import Util (debugIsOn)
--for asserts

import StgCmmMonad
import StgCmmForeign
import StgCmmUtils

import MkGraph
import CmmExpr
import CmmUtils

import BasicTypes
import BacktraceTypes
import DynFlags
import CLabel
import FastString
import Outputable
import qualified Module

btType :: DynFlags -> CmmType
btType = bWord

saveCurrentBacktrace :: FCode LocalReg
saveCurrentBacktrace = do
  dflags <- getDynFlags
  local_bt <- newTemp (btType dflags)
  emitAssign (CmmLocal local_bt) curBacktrace
  return local_bt

restoreCurrentBacktrace :: LocalReg -> FCode ()
restoreCurrentBacktrace local_bt = emit $ assignCurBacktrace $ CmmReg $
                                     CmmLocal local_bt

enterBacktraceThunk :: CmmExpr -> FCode ()
enterBacktraceThunk closure = do
  dflags <- getDynFlags
  emit $ assignCurBacktrace $ backtraceFrom dflags closure


enterBacktraceFun :: Bool -> CmmExpr -> FCode ()
enterBacktraceFun True  _       = return () -- top-level function, nothing to do
enterBacktraceFun False closure = do
  dflags <- getDynFlags
  emitRtsCall Module.rtsPackageId (fsLit "enterFunBacktrace")
    [(CmmReg (CmmGlobal BaseReg), AddrHint),
     (backtraceFrom dflags closure, AddrHint)] False

backtraceFrom :: DynFlags
              -> CmmExpr --A closure pointer
              -> CmmExpr --The backtrace from that closure
backtraceFrom dflags cl =
  CmmLoad (cmmOffsetB dflags cl (offsetBt dflags))
    (btType dflags)

offsetBt :: DynFlags -> Int
offsetBt dflags
  | gopt Opt_SccProfilingOn dflags = oFFSET_StgBacktraceHeader_bt dflags
  | otherwise = oFFSET_StgBacktraceHeader_bt dflags -
                (wORD_SIZE dflags * pROF_HDR_SIZE dflags)


emitPushTracepoint :: Tracepoint -> FCode ()
emitPushTracepoint tp = do
  dflags <- getDynFlags
  MASSERT(gopt Opt_BacktraceOn dflags)
  let cap = cmmSubWord dflags (CmmReg baseReg)
              (mkIntExpr dflags (oFFSET_Capability_r dflags))
  emitCCall [] cfun
    [(cap,AddrHint),
     (CmmLit (CmmLabel (mkCTracepointLabel tp)),AddrHint)]
  where
    cfun = (CmmLit (CmmLabel (mkForeignLabel
                              (fsLit "pushTracepoint") Nothing
                              ForeignLabelInExternalPackage IsFunction)))

initTracepoints :: [Tracepoint] -> FCode ()
initTracepoints tps =
         mapM_ emitTracepointDecl tps

emitTracepointDecl :: Tracepoint -> FCode ()
emitTracepointDecl tp = do
  dflags <- getDynFlags
  MASSERT(gopt Opt_BacktraceOn dflags)
  label <- newByteStringCLit $ bytesFS $ tp_name tp
  modl  <- newByteStringCLit $ bytesFS $ Module.moduleNameFS $ Module.moduleName
                             $ tp_mod tp
  loc   <- newByteStringCLit $ bytesFS $ mkFastString $ showPpr dflags
                             $ tp_loc tp
  let lits = [ label --char *label
             , modl  --char *module
             , loc   --char *srcloc
             ]
  emitDataLits (mkCTracepointLabel tp) lits

curBacktrace :: CmmExpr
curBacktrace = CmmReg (CmmGlobal CurrentBacktrace)

assignCurBacktrace :: CmmExpr -> CmmAGraph
assignCurBacktrace = mkAssign $ CmmGlobal CurrentBacktrace
