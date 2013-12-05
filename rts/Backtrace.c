/****************************/
/* (c) William Kenyon, 2013 */
/* 			    */
/* Backtrace Support	    */
/****************************/

#include "Rts.h"
#include "Capability.h"

void
pushTracepoint(Capability* cap, Tracepoint* tp){
    StgBacktrace *bt;
    bt = (StgBacktrace *) allocate(cap,sizeofW(StgBacktrace));
    SET_HDR(bt,&stg_Backtrace_info,CCS_SYSTEM /*TODO*/);
    bt->tp = tp;
    bt->link = (StgBacktrace *)(cap->r.rCurrentBacktrace);
    cap->r.rCurrentBacktrace = (struct StgBacktrace_ *)bt;
}

void
enterFunBacktrace(StgRegTable* tab,StgBacktrace* bt){
    return;
}

void fprintBacktrace(StgBacktrace *bt){
    while (bt) {
	fprintf(stderr,"\n %s.%s", bt->tp->module, bt->tp->label);
	bt = bt->link;
    }
    fprintf(stderr,"\n");
    return;
}
