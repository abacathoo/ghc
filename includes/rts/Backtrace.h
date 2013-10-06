/*****************************************************************************/
/* (c) William Kenyon, 2013						     */
/* 									     */
/* Runtime System Backtrace API						     */
/* 									     */
/* Do not #include this file directly: #include "Rts.h" instead.	     */
/* 									     */
/* To understand the structure of the RTS headers, see the wiki:	     */
/*   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes */
/*****************************************************************************/

#ifndef RTS_BACKTRACE_H
#define RTS_BACKTRACE_H

typedef struct {
    char * label;
    char * module;
    char * srcloc;
} Tracepoint;

#endif
