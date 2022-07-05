#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/uom.h"
void /*FUNCTION*/ xadd(nerr)
int *nerr;
{
	int lchn;
	int j, j_, jcon, jdfl, jdfl_, ndx1, ndx2, nlen;
	float con;

        float *Sacmem;

	/* Ind
	 *=====================================================================
	 * PURPOSE:  To execute the action command ADD.
	 *           This command adds a constant to data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  UOM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *    UOM:     CONADD()
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    UOM:     CONADD()
	 *    HDR:     DEPMIN, DEPMAX, DEPMEM
	 *=====================================================================
	 * ADDROUTINES CALLED:
	 *    SACLIB:  LCMORE, LCREAL, CFMT, CRESP,
	 *             VFLIST, VFTIME, GETFIL, PUTFIL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    CON:     Constant currently being used in subtraction.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	jcon = 0;
	lchn = FALSE;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "v":  constant to add. */
		if( lcreal( &con ) ){
			jcon = jcon + 1;
			Conadd[jcon] = con;
			lchn = TRUE;

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* - Fill remainder of array with last input constant. */

	if( lchn ){
		for( j = jcon + 1; j <= MDFL; j++ ){
			j_ = j - 1;
			Conadd[j] = con;
			}
		}

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

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get next file from memory manager. */
		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Add appropriate constant to each data point. */
		con = Conadd[jdfl];
                Sacmem = cmmem.sacmem[ndx1];
		for( j = ndx1; j <= (ndx1 + nlen - 1); j++ ){
			*(Sacmem++) += con;
			}

		/* -- Recompute extrema. */
		*depmen = *depmen + con;
		*depmin = *depmin + con;
		*depmax = *depmax + con;

		/* -- Give file back to memory manager. */
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
	 *    820701:  Documented subroutine.
	 *    820701:  Changed to newest set of parsing and checking functions.
	 *===================================================================== */

} /* end of function */

