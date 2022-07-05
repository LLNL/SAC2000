#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#define	MSYNCH	MDFL

#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void /*FUNCTION*/ xsynch(nerr)
int *nerr;
{
	int jdfl, jdfl_, nb, ndttmi[MSYNCH][6], ndttmo[MSYNCH][6], 
	 ndx1, ndx2, nlen;
	float begi[MSYNCH], bego[MSYNCH], dtnew;

	float *const Begi = &begi[0] - 1;
	float *const Bego = &bego[0] - 1;

	static int lbegin = FALSE ; /* Fix bug but add option to allow
					old buggy behaviour. maf 970908 */

	/*=====================================================================
	 * PURPOSE:  To execute the action command SYNCH.
	 *           This command synchonizes in time all files in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     LROUND
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     LROUND
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, LKLOG, CFMT, CRESP, VFLIST, VFEVEN,
	 *             GETFIL, COPYI, SYNCH, PUTFIL
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831020:  Moved from XSC to internal DFM command.
	 *    830915:  Merged SYNCH and SYNCH2 using new shell.
	 *    830124:  Modifications due to new set of parsing/checking functions.
	 *    810624:  Original version of XSC shell.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870224
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE */

	/* - Parse each token in command (if any): */

	while ( lcmore( nerr ) ){

	    /* -- "ROUND [ON/OFF]":  change begin time rounding option. */
	    if( lklog( "R$",3, &cmdfm.lround ) )
	    { /* do nothing */ }

	    /* -- "BEGIN [ON/OFF]":  set both begin times to zero if ON. */
	    else if( lklog( "B#EGIN$",8 , &lbegin ) )
	    { /* do nothing */ }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	}

	/* - The above loop is over when one of two conditions has beenmet:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0
	 *   (2) All the tokens in the command have been successfully parsed */

	if( *nerr != 0 )
	    return ;

	/* CHECKING PHASE: */

	/* - Test for a non-null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
	    return ;

	/* - Make sure each file is an evenly spaced time series file. */

	vfeven( nerr );
	if( *nerr != 0 )
	    return ;

	/* EXECUTION PHASE: */

	/* - Save beginning offset and reference time for each file. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    jdfl_ = jdfl - 1;
	    getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		return ;
	    Begi[jdfl] = *begin;
	    copyi( nzdttm, &ndttmi[jdfl_][0], 6 );
	}

	/* - Calculate new reference times and beginning offsets. */

	synch( ndttmi, begi, cmdfm.ndfl, ndttmo, bego ,lbegin );

	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    jdfl_ = jdfl - 1;

	    /* -- Get next file from the memory manager.
	     *    (Header is moved into common blocks CMHDR and KMHDR.) */

	    getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		return ;

	    /* -- Update time header fields.
	     *    New beginning offsets are set to exact value or they
	     *    may be rounded to the nearest multiple of DELTA. */

	    dtnew = Bego[jdfl] - *begin;
	    if( cmdfm.lround ){
		nb = (int)( Bego[jdfl]/ *delta + sign( 0.5, *begin ) );
		*begin = *delta*(float)( nb );
	    }
	    else{
		*begin = *begin + dtnew;
	    }
	    *ennd = *begin + *delta*(float)( *npts - 1 );
	    if( *a != cmhdr.fundef )
		*a = *a + dtnew;
	    if( *f != cmhdr.fundef )
		*f = *f + dtnew;
	    if( *o != cmhdr.fundef )
		*o = *o + dtnew;
	    if( *t0 != cmhdr.fundef )
		*t0 = *t0 + dtnew;
	    if( *t1 != cmhdr.fundef )
		*t1 = *t1 + dtnew;
	    if( *t2 != cmhdr.fundef )
		*t2 = *t2 + dtnew;
	    if( *t3 != cmhdr.fundef )
		*t3 = *t3 + dtnew;
	    if( *t4 != cmhdr.fundef )
		*t4 = *t4 + dtnew;
	    if( *t5 != cmhdr.fundef )
		*t5 = *t5 + dtnew;
	    if( *t6 != cmhdr.fundef )
		*t6 = *t6 + dtnew;
	    if( *t7 != cmhdr.fundef )
		*t7 = *t7 + dtnew;
	    if( *t8 != cmhdr.fundef )
		*t8 = *t8 + dtnew;
	    if( *t9 != cmhdr.fundef )
		*t9 = *t9 + dtnew;
	    copyi( &ndttmo[jdfl_][0], nzdttm, 6 );

	    /* -- Return file to memory manager. */

	    putfil( jdfl, nerr );
	    if( *nerr != 0 )
		return ;

	}

	return;
} /* end of function */

