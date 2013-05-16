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

#ifndef RTS_LWT_CCS_H
#define RTS_LWT_CCS_H




// Returns non-zero if the RTS is a profiling version
int rts_isProfiled(void);
// Starts up lwt profiling
void rts_initLwtProfiling(void);
void rts_dumpCLCCS(void);

void freeLwtProfiling  (void);
/* -----------------------------------------------------------------------------
 * Data Structures 
 * ---------------------------------------------------------------------------*/  

typedef struct LwtCostCentre_ {
    StgInt ccID;
    
    char * label;
    char * module;
    char * srcloc;

    StgInt is_caf;            // non-zero for a CAF cost centre
} LwtCostCentre;

typedef struct LwtCostCentreStack_ {
    StgInt ccsID;

    struct LwtCostCentre_ *cc;                       // Cost centre at the top of the stack
    struct LwtCostCentreStack_ *prevStack;   // parent
    struct LwtCostCentreStack_ *root;        // root of stack
    struct LwtIndexTable_ *indexTable;                 // cache of previous pushes to this ccs
    StgWord    depth;                        // number of items in the stack
} LwtCostCentreStack;

//linked list of childeren to a cost centre stack
typedef struct LwtIndexTable_ {
    struct LwtCostCentre_ *cc;
    struct LwtCostCentreStack_ *ccs;
    struct LwtIndexTable_ *next;
} LwtIndexTable;

/* -----------------------------------------------------------------------------
   Pre-defined cost centres and cost centre stacks
   -------------------------------------------------------------------------- */

#if IN_STG_CODE //should never happen

shouldNeverHitThisRight?

extern StgWord LCC_MAIN;	
extern StgWord LCCS_MAIN;      // Top CCS

#else


extern LwtCostCentreStack * savedCLCCS;    //saved by Interpreter
                                    //used by dumpCLCCS function

extern LwtCostCentre      LCC_MAIN;	
extern LwtCostCentreStack LCCS_MAIN;      // Top CCS

#endif /* IN_STG_CODE */



/* -----------------------------------------------------------------------------
 * Functions 
 * ---------------------------------------------------------------------------*/

void                 enterFunLwtCCS    (Capability *cap, LwtCostCentreStack *lccs);
LwtCostCentreStack * pushLwtCostCentre(LwtCostCentreStack*lccs,LwtCostCentre*cc);

#endif /* LWT_CCS_H */
