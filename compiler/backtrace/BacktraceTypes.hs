--------------------------------------------------
-- Types for backtracing                        --
--                                              --
-- (c) William Kenyon 2013                      --
--------------------------------------------------

module BacktraceTypes(Tracepoint(..)) where
import Outputable
import Module
import SrcLoc
import FastString
import Unique
import Data.Data
import Util

data Tracepoint =  Tracepoint { tp_key  :: Int
                              , tp_name :: FastString
                              , tp_mod  :: Module
                              , tp_loc  :: SrcSpan }
  deriving (Data, Typeable)

instance Eq Tracepoint where
  tp1 == tp2 = isEqual $ compare tp1 tp2

instance Ord Tracepoint where
  tp1 `compare` tp2 = thenCmp (tp_mod tp1 `compare` tp_mod tp2)
                              (tp_key tp1 `compare` tp_key tp2)


instance Outputable Tracepoint where
  ppr tp = getPprStyle $ \ sty ->
    if codeStyle sty
      then pprTracepointLbl tp
      else text $ unpackFS $ tp_name tp

pprTracepointLbl :: Tracepoint -> SDoc
pprTracepointLbl (Tracepoint{ tp_key  = k
                            , tp_name = n
                            , tp_mod = m })
  = ppr m <> char '_' <> ztext (zEncodeFS n) <> char '_'
          <> ppr (mkUniqueGrimily k) <> text "_tracepoint"
