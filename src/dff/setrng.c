#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ setrng()
{
	int jdfl, ndx1, ndx2, nerr, nlen;

	/*=====================================================================
	 * PURPOSE:  To calculate and set the dependent variable range
	 *           for all files in DFL.
	 *=====================================================================
	 * MODULE/LEVEL:  GAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    VLARGE
	 *    DFM:     NDFL
	 *    HDR:     DEPMIN, DEPMAX
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GAM:     RNGMIN, RNGMAX
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GETFIL, GTOUTM
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Initialize range variables. */
	cmgam.rngmin = VLARGE;
	cmgam.rngmax = -VLARGE;

	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){

		/* -- Get header from memory manager. */
		getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, &nerr );
		if( nerr != 0 )
			goto L_8888;

		/* -- Adjust range variables. */
		cmgam.rngmin = fmin( cmgam.rngmin, *depmin );
		cmgam.rngmax = fmax( cmgam.rngmax, *depmax );
	}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820818:  Original version.
	 *===================================================================== */

} /* end of function */

