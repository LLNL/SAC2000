#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ xbenioff(nerr)
int *nerr;
{
	int j, j_, jdfl, jdfl_, ndxx, ndxy, nlen;
	float xjunk;

        float *Sacmem;

	/*=====================================================================
	 * PURPOSE: To parse and execute the action command BENIOFF.
	 *          This command applies a Benioff filter to data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  XSC/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     DEPMIN, DEPMAX, DEPMEN
	 *    MEM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870211:  Converted to an internal command.
	 *    841206:  Original XSC version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870211
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

	/* EXECUTION PHASE: */

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get next file from the memory manager.
		 *    (Header is moved into common blocks CMHDR and KMHDR.) */
		getfil( jdfl, TRUE, &nlen, &ndxy, &ndxx, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Initialize filter for this file. */
		xjunk = filtb( 0, delta );

		/* -- Filter this data file. */
                Sacmem = cmmem.sacmem[ndxy];
		for( j = ndxy; j <= (ndxy + nlen - 1); j++ ){
			*Sacmem = filtb( 1, Sacmem );
                        Sacmem++;
			}

		/* -- Update any header fields that may have changed. */
		extrma( cmmem.sacmem[ndxy], 1, nlen, depmin, depmax, depmen );

		/* -- Return file to memory manager. */
		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

		}

	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

} /* end of function */

