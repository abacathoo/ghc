/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2000
 *
 * Support for profiling
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h"
#include "LwtProfiling.h"


#include "Arena.h"

#include "Printer.h"
#include "Capability.h"

#include <string.h>

#ifdef DEBUG
#include "Trace.h"
#endif

/*
 * Profiling allocation arena.
 */
Arena *prof_arena;




#ifdef THREADED_RTS
Mutex ccs_mutex;
#endif



/*
 *    MAIN   is the root of the cost-centre stack tree.  If there are
 *           no _scc_s in the program, all costs will be attributed
 *           to MAIN.
 */

LwtCostCentre LCC_MAIN = {
    label : "MAIN",
    module : "MAIN",
    srcloc : "<built-in>",
    is_caf : CC_NOT_CAF
};

LwtCostCentreStack LCCS_MAIN = {
    cc : &LCC_MAIN,
    prevStack : NULL,
    indexTable : NULL,
    depth : 0
};

LwtCostCentreStack * savedCLCCS = NULL;


/*
 * Static Functions
 */

static  LwtCostCentreStack * appendCCS       ( LwtCostCentreStack *ccs1,
                                               LwtCostCentreStack *ccs2 );
static  LwtCostCentreStack * actualPush_     ( LwtCostCentreStack *ccs, 
                                               LwtCostCentre *cc,
                                               LwtCostCentreStack *new_ccs );

static  LwtCostCentreStack * checkLoop       ( LwtCostCentreStack *ccs,
                                               LwtCostCentre *cc );

static  LwtCostCentreStack * actualPush      ( LwtCostCentreStack *, 
                                               LwtCostCentre * );

static  LwtCostCentreStack * isInIndexTable  ( LwtIndexTable *, LwtCostCentre * );
static  LwtIndexTable *      addToIndexTable ( LwtIndexTable *, LwtCostCentreStack *, LwtCostCentre *);


static LwtCostCentreStack * enterFunEqualStacks (LwtCostCentreStack *,LwtCostCentreStack *,LwtCostCentreStack *);

static LwtCostCentreStack *
enterFunCurShorter (LwtCostCentreStack *, LwtCostCentreStack *, StgWord n);

/* -----------------------------------------------------------------------------
   Initialise the profiling environment
   -------------------------------------------------------------------------- */

void
rts_initLwtProfiling (void)
{
    // initialise our arena
    prof_arena = newArena();
   

#ifdef THREADED_RTS
    initMutex(&ccs_mutex);
#endif
}

void
freeLwtProfiling (void)
{
    arenaFree(prof_arena);
}


/* -----------------------------------------------------------------------------
   Set CCCS when entering a function.

   The algorithm is as follows.

     ccs ++> ccsfn  =  ccs ++ dropCommonPrefix ccs ccsfn

   where

     dropCommonPrefix A B
        -- returns the suffix of B after removing any prefix common
        -- to both A and B.

   e.g.

     <a,b,c> ++> <>      = <a,b,c>
     <a,b,c> ++> <d>     = <a,b,c,d>
     <a,b,c> ++> <a,b>   = <a,b,c>
     <a,b>   ++> <a,b,c> = <a,b,c>
     <a,b,c> ++> <a,b,d> = <a,b,c,d>

   -------------------------------------------------------------------------- */

// implements  c1 ++> c2,  where c1 and c2 are equal depth
//
static LwtCostCentreStack *
enterFunEqualStacks (LwtCostCentreStack *ccs0,
                     LwtCostCentreStack *ccsapp,
                     LwtCostCentreStack *ccsfn)
{
    ASSERT(ccsapp->depth == ccsfn->depth);
    if (ccsapp == ccsfn) return ccs0;
    return pushLwtCostCentre(enterFunEqualStacks(ccs0,
                                              ccsapp->prevStack,
                                              ccsfn->prevStack),
                          ccsfn->cc);
}

// implements  c1 ++> c2,  where c2 is deeper than c1.
// Drop elements of c2 until we have equal stacks, call
// enterFunEqualStacks(), and then push on the elements that we
// dropped in reverse order.
//
static LwtCostCentreStack *
enterFunCurShorter (LwtCostCentreStack *ccsapp, LwtCostCentreStack *ccsfn, StgWord n)
{
    if (n == 0) {
        ASSERT(ccsfn->depth == ccsapp->depth);
        return enterFunEqualStacks(ccsapp,ccsapp,ccsfn);;
    } else {
        ASSERT(ccsfn->depth > ccsapp->depth);
        return pushLwtCostCentre(enterFunCurShorter(ccsapp, ccsfn->prevStack, n-1),
                              ccsfn->cc);
    }
}

void enterFunLwtCCS (Capability *cap, LwtCostCentreStack *ccsfn)
{
    LwtCostCentreStack *ccsapp;

    // common case 1: both stacks are the same
    if (ccsfn == cap->r.rCLCCS) {
        return;
    }

    // common case 2: the function stack is empty, or just CAF
    if (ccsfn->prevStack == &LCCS_MAIN) {
        return;
    }

    ccsapp = cap->r.rCLCCS;
    // common case 3: the stacks are completely different (e.g. one is a
    // descendent of MAIN and the other of a CAF): we append the whole
    // of the function stack to the current CCS.
    if (ccsfn->root != ccsapp->root) {
        cap->r.rCLCCS = appendCCS(ccsapp,ccsfn);
        return;
    }

    // uncommon case 4: ccsapp is deeper than ccsfn
    if (ccsapp->depth > ccsfn->depth) {
        nat i, n;
        LwtCostCentreStack *tmp = ccsapp;
        n = ccsapp->depth - ccsfn->depth;
        for (i = 0; i < n; i++) {
            tmp = tmp->prevStack;
        }
        cap->r.rCLCCS = enterFunEqualStacks(ccsapp,tmp,ccsfn);
        return;
    }

    // uncommon case 5: ccsfn is deeper than CCCS
    if (ccsfn->depth > ccsapp->depth) {
        cap->r.rCLCCS = enterFunCurShorter(ccsapp, ccsfn,
                                         ccsfn->depth - ccsapp->depth);
        return;
    }

    // uncommon case 6: stacks are equal depth, but different
    cap->r.rCLCCS = enterFunEqualStacks(ccsapp,ccsapp,ccsfn);
}


/* -----------------------------------------------------------------------------
   Cost-centre stack manipulation
   -------------------------------------------------------------------------- */

#ifdef DEBUG
static LwtCostCentreStack * _pushLwtCostCentre ( LwtCostCentreStack *ccs, LwtCostCentre *cc );
LwtCostCentreStack *
pushLwtCostCentre ( LwtCostCentreStack *ccs, LwtCostCentre *cc )
#define pushLwtCostCentre _pushLwtCostCentre
{
    IF_DEBUG(interpreter,
	     traceBegin("pushing %s on ", cc->label);
	     debugLwtCCS(ccs);
	     traceEnd(););
	     
    return pushLwtCostCentre(ccs,cc);
}
#endif

/* Append ccs1 to ccs2 (ignoring any CAF cost centre at the root of ccs1 */

#ifdef DEBUG
static LwtCostCentreStack *_appendCCS ( LwtCostCentreStack *ccs1, LwtCostCentreStack *ccs2 );
static LwtCostCentreStack *
appendCCS ( LwtCostCentreStack *ccs1, LwtCostCentreStack *ccs2 )
#define appendCCS _appendCCS
{
  IF_DEBUG(interpreter,
          if (ccs1 != ccs2) {
            debugBelch("Appending ");
	    //            debugCCS(ccs1);
            debugBelch(" to ");
	    //            debugCCS(ccs2);
            debugBelch("\n");});
  return appendCCS(ccs1,ccs2);
}
#endif

static LwtCostCentreStack *
appendCCS ( LwtCostCentreStack *ccs1, LwtCostCentreStack *ccs2 )
{
    if (ccs1 == ccs2) {
        return ccs1;
    }

    if (ccs2 == &LCCS_MAIN || ccs2->cc->is_caf == CC_IS_CAF) {//TODO: inspect CC_IS_CAF macro
        // stop at a CAF element
        return ccs1;
    }

    return pushLwtCostCentre(appendCCS(ccs1, ccs2->prevStack), ccs2->cc);
}

// Pick one:
// #define RECURSION_DROPS
#define RECURSION_TRUNCATES

LwtCostCentreStack *
pushLwtCostCentre (LwtCostCentreStack *ccs, LwtCostCentre *cc)
{
    LwtCostCentreStack *temp_ccs, *ret;
    LwtIndexTable *ixtable;

    if (ccs == EMPTY_STACK) {
        ACQUIRE_LOCK(&ccs_mutex);
        ret = actualPush(ccs,cc);
    }
    else
    {
        if (ccs->cc == cc) {
            return ccs;
        } else {
            // check if we've already memoized this stack
            ixtable = ccs->indexTable;
            temp_ccs = isInIndexTable(ixtable,cc);
      
            if (temp_ccs != EMPTY_STACK) {
                return temp_ccs;
            } else {

                // not in the IndexTable, now we take the lock:
                ACQUIRE_LOCK(&ccs_mutex);

                if (ccs->indexTable != ixtable)
                {
                    // someone modified ccs->indexTable while
                    // we did not hold the lock, so we must
                    // check it again:
                    temp_ccs = isInIndexTable(ixtable,cc);
                    if (temp_ccs != EMPTY_STACK)
                    {
                        RELEASE_LOCK(&ccs_mutex);
                        return temp_ccs;
                    }
                }
                temp_ccs = checkLoop(ccs,cc);
                if (temp_ccs != NULL) {
                    // This CC is already in the stack somewhere.
                    // This could be recursion, or just calling
                    // another function with the same CC.
                    // A number of policies are possible at this
                    // point, we implement two here:
                    //   - truncate the stack to the previous instance
                    //     of this CC
                    //   - ignore this push, return the same stack.
                    //
                    LwtCostCentreStack *new_ccs;
#if defined(RECURSION_TRUNCATES)
                    new_ccs = temp_ccs;
#else // defined(RECURSION_DROPS)
                    new_ccs = ccs;
#endif
                    ccs->indexTable = addToIndexTable (ccs->indexTable,
                                                       new_ccs, cc);
                    ret = new_ccs;
                } else {
                    ret = actualPush (ccs,cc);
                }
            }
        }
    }

    RELEASE_LOCK(&ccs_mutex);
    return ret;
}

static LwtCostCentreStack *
isInIndexTable(LwtIndexTable *it, LwtCostCentre *cc)
{
    while (it!=EMPTY_TABLE)
    {
        if (it->cc == cc)
            return it->ccs;
        else
            it = it->next;
    }
  
    /* otherwise we never found it so return EMPTY_TABLE */
    return EMPTY_TABLE;
}


static LwtIndexTable *
addToIndexTable (LwtIndexTable *it, LwtCostCentreStack *new_ccs, LwtCostCentre *cc)
{
    LwtIndexTable *new_it;

    new_it = arenaAlloc(prof_arena, sizeof(LwtIndexTable));

    new_it->cc = cc;
    new_it->ccs = new_ccs;
    new_it->next = it;

    return new_it;
}

static LwtCostCentreStack *
checkLoop (LwtCostCentreStack *ccs, LwtCostCentre *cc)
{
    while (ccs != EMPTY_STACK) {
        if (ccs->cc == cc)
            return ccs;
        ccs = ccs->prevStack;
    }
    return NULL;
}

static LwtCostCentreStack *
actualPush (LwtCostCentreStack *ccs, LwtCostCentre *cc)
{
    LwtCostCentreStack *new_ccs;

    // allocate space for a new LwtCostCentreStack
    new_ccs = (LwtCostCentreStack *) arenaAlloc(prof_arena, sizeof(LwtCostCentreStack));

    return actualPush_(ccs, cc, new_ccs);
}

static LwtCostCentreStack *
actualPush_ (LwtCostCentreStack *ccs, LwtCostCentre *cc, LwtCostCentreStack *new_ccs)
{
    /* assign values to each member of the structure */
//    new_ccs->ccsID = CCS_ID++;
    new_ccs->cc = cc;
    new_ccs->prevStack = ccs;
    new_ccs->root = ccs->root;
    new_ccs->depth = ccs->depth + 1;

    new_ccs->indexTable = EMPTY_TABLE;

    /* Initialise the various _scc_ counters to zero
     */

    /* Initialize all other stats here.  There should be a quick way
     * that's easily used elsewhere too
     */

    /* update the memoization table for the parent stack */
    if (ccs != EMPTY_STACK) {
        ccs->indexTable = addToIndexTable(ccs->indexTable, new_ccs, cc);
    }

    /* return a pointer to the new stack */
    return new_ccs;
}
#ifdef DEBUG
void
debugLwtCCS( LwtCostCentreStack *ccs )
{
    debugBelch("<");
    for (; ccs && ccs != &LCCS_MAIN; ccs = ccs->prevStack ) {
        debugBelch("%p(%d): %s.%s\n", ccs->cc, ccs->cc->ccID, ccs->cc->module, ccs->cc->label);
        if (ccs->prevStack && ccs->prevStack != &LCCS_MAIN) {
            debugBelch(",");
        }
    }
    debugBelch(">");
}


void
debugLwtCC( LwtCostCentre *cc )
{

  debugBelch("CostCentre at %p\n",cc);
  debugBelch("id %p: %d\n"    ,&(cc->ccID)      ,cc->ccID);
  debugBelch("label %p: %s\n" ,&(cc->label)     ,cc->label);
  debugBelch("module %p: %s\n",&(cc->module)    ,cc->module);
  debugBelch("srcloc %p: %s\n",&(cc->srcloc)    ,cc->srcloc);
  debugBelch("caf %p: %d\n"   ,&(cc->is_caf)    ,cc->is_caf);

}
#endif /* DEBUG */

static void dumpLwtCCS(LwtCostCentreStack *ccs);
void rts_dumpCLCCS(void) {
  dumpLwtCCS(savedCLCCS);
}


static void
dumpLwtCCS( LwtCostCentreStack *ccs )
{
    printf("Stack Trace: \n");
    for (; ccs && ccs != &LCCS_MAIN; ccs = ccs->prevStack ) {
        printf("%s.%s at %s\n", ccs->cc->module, ccs->cc->label, ccs->cc->srcloc);
    }
    debugBelch("\n");
}
