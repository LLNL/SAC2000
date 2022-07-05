#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MINDATALEN	201
#define	MLENSCRATCH	4297

#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sam.h"
void /*FUNCTION*/ xenvelope(nerr)
int *nerr;
{
	int j, j_, jdfl, jdfl_, ndxhilbert, ndxscratch, ndxsignal, 
	 nlenmn, nlenmx, nlnsignal, notused, ntused;

        float *Sacmem1, *Sacmem2;

	/*=====================================================================
	 * PURPOSE: To parse and execute the action command ENVELOPE.
	 *          This command computes the envelope of a function.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  sam/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    hdr:     depmin, depmax, depmen
	 *    mem:     sacmem
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  vflist, vfeven, getfil, setmsg, apimsg, allamb, firtrn,
	 *             extrma, putfil, relamb
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    MLENSCRATCH:  Size of scratch space needed for transform. [ip]
	 *    ndxscratch:   Index in SACMEM array for scratch space. [i]
	 *    ndxhilbert:   Index in SACMEM array for Hilbert transform. [i]
	 *    ndxy:         Index in SACMEM array for current signal. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890223:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890223
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* CHECKING PHASE: */

	/* - Test for a non-null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Make sure each file is an evenly spaced time series file. */

	vfeven( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Determine minimum and maximum signal size.
	 *   Make sure minimum is not too small for fir filter subroutine.
	 *   Use maximum to allocate space for the Hilbert transform. */

	nlenmn = MLARGE;
	nlenmx = 0;
	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;
		getfil( jdfl, FALSE, &ntused, &ntused, &ntused, nerr );
		if( *nerr != 0 )
			goto L_8888;
		nlenmn = min( nlenmn, *npts );
		nlenmx = max( nlenmx, *npts );
		}

	if( nlenmn < MINDATALEN ){
		*nerr = 1613;
		setmsg( "ERROR", *nerr );
		apimsg( MINDATALEN );
		goto L_8888;
		}

	/* - EXECUTION PHASE: */

	/* - Allocate temporary blocks for scratch space and Hilbert transform. */

	allamb( &cmmem, MLENSCRATCH, &ndxscratch, nerr );
	if( *nerr != 0 )
		goto L_8888;

	allamb( &cmmem, nlenmx, &ndxhilbert, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get next file from the memory manager.
		 *    (Header is moved into common blocks CMHDR and KMHDR.) */
		getfil( jdfl, TRUE, &nlnsignal, &ndxsignal, &notused, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Compute the Hilbert transform. */
		firtrn( "HILBERT", cmmem.sacmem[ndxsignal], nlnsignal, cmmem.sacmem[ndxscratch], 
		 cmmem.sacmem[ndxhilbert] );

		/* -- Compute the envelope by taking the square root of the sum
		 *    of the squares of the signal and its Hilbert transform. */

                Sacmem1 = cmmem.sacmem[ndxsignal];
                Sacmem2 = cmmem.sacmem[ndxhilbert];
		for( j = 0; j <= (nlnsignal - 1); j++ ){
			*Sacmem1 = sqrt( powi(*Sacmem1,2) + powi(*Sacmem2,2) );
                        Sacmem1++;
                        Sacmem2++;
			}

		/* -- Update any header fields that may have changed. */
		extrma( cmmem.sacmem[ndxsignal], 1, nlnsignal, depmin, depmax, 
		 depmen );

		/* -- Return file to memory manager. */
		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

		}

	/* - Release scratch space. */

	relamb( cmmem.sacmem, ndxscratch, nerr );
	if( *nerr != 0 )
		goto L_8888;

	relamb( cmmem.sacmem, ndxhilbert, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

} /* end of function */

