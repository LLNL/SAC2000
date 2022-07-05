#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getvspace(xvsmin, xvsmax, yvsmin, yvsmax)
float *xvsmin, *xvsmax, *yvsmin, *yvsmax;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the current viewspace range.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    xvsmin:  Minimum x viewspace value. [f]
	 *    xvsmax:  Maximum x viewspace value. [f]
	 *    yvsmin:  Minimum y viewspace value. [f]
	 *    yvsmax:  Maximum y viewspace value. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     xvs, yvs
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861021:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861021
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Report current common block values. */
	*xvsmin = Xvs[1];
	*xvsmax = Xvs[2];
	*yvsmin = Yvs[1];
	*yvsmax = Yvs[2];

L_8888:
	return;

} /* end of function */

