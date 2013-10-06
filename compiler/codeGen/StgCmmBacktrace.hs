  --------------------------------------------------
  -- Code generation for backtraces               --
  --                                              --
  -- (c) William Kenyon 2013                      --
  --------------------------------------------------
module StgCmmBacktrace (initTracepoints) where

#include "HsVersions.h"
import Util (debugIsOn)
--for asserts

import StgCmmMonad
import StgCmmForeign
import StgCmmUtils

import CmmExpr
import CmmUtils

import BasicTypes
import BacktraceTypes
import DynFlags
import CLabel
import FastString
import Outputable
import qualified Module

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
