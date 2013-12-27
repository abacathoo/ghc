/*****************************************************************************/
/* (c) William Kenyon, 2013						     */
/* 									     */
/* Prototypes for Backtraces in the RTS                                      */
/* 									     */
/* Do not #include this file directly: #include "Rts.h" instead.	     */
/* 									     */
/* To understand the structure of the RTS headers, see the wiki:	     */
/*   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes */
/*****************************************************************************/

#ifndef RTS_BACKTRACE_PROTOTYPES_H
#define RTS_BACKTRACE_PROTOTYPES_H

void pushTracepoint(Capability*,Tracepoint*);
void enterFunBacktrace(StgRegTable*,StgBacktrace*);
void fprintBacktrace(StgBacktrace*);

extern Tracepoint rootTracepoint;
extern StgBacktrace rootBacktrace;

#endif /* RTS_BACKTRACE_PROTOTYPES_H */
