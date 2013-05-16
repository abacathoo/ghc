/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/
/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009-2012
 *
 * Macros for profiling operations in STG code
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef LWTPROFILING_H
#define LWTPROFILING_H

#include <stdio.h>

#include "BeginPrivate.h"
#include "Rts.h"



#ifdef DEBUG
void debugLwtCC (LwtCostCentre* cc);
void debugLwtCCS(LwtCostCentreStack *ccs);
#endif


#include "EndPrivate.h"

#endif /* LWTPROFILING_H */
