#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ xsqr(nerr)
int *nerr;
{
	int j, j_, jdfl, jdfl_, ndx1, ndx2, nlen;

	float *Sacmem;

	/*=====================================================================
	 * PURPOSE:  To execute the action command SQR.
	 *           This command squares data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag.
	 *=====================================================================
	 * MODULE/LEVEL:  UOM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     DEPMIN, DEPMAX, DEPMEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  VFLIST, VFTIME, GETFIL, EXTRMA, PUTFIL
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Check to make sure all files are time series files. */

	vftime( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get the next file in DFL, moving header to CMHDR. */

		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Square each value in the dependent array. */
                Sacmem = cmmem.sacmem[ndx1];
		for( j = ndx1; j <= (ndx1 + nlen - 1); j++ ){
                        *Sacmem = powi(*Sacmem,2);
                        Sacmem++;
			}

		/* -- Update any header fields that may have changed. */

		extrma( cmmem.sacmem[ndx1], 1, nlen, depmin, depmax, depmen );

		/* -- Reverse the steps used in getting the next file in DFL. */

		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

		}

	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820817:  Documented subroutine.
	 *    820202:  Original version.
	 *===================================================================== */

} /* end of function */

