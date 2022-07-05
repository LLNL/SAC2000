#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ pkfilt(diff, fdold, fdnew, rmnabs)
double diff, fdold;
float *fdnew, *rmnabs;
{



	/*=====================================================================
	 * PURPOSE: To filter (high pass) trace data for picking module.
	 *          The running mean absolute value of the filtered data is
	 *          also returned.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    DIFF:    First difference (current point - last point). [f]
	 *    FDOLD:   Last value of filtered data. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    FDNEW:   Current value of filtered data. [f]
	 *    RMNABS:  Running mean absolute value of filtered data. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     C1, C4
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Compute new value of filtered (high pass) data. */
	*fdnew = diff + cmeam.c1*fdold;

	/* - Compute the running mean absolute value of the filtered data. */

	*rmnabs = *rmnabs + cmeam.c6*(fabs( *fdnew ) - *rmnabs);

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    801101:  Factored from original subroutine PK1.
	 *===================================================================== */

} /* end of function */

