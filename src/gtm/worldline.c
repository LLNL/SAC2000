#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ worldline(xwloc1, ywloc1, xwloc2, ywloc2)
double xwloc1, ywloc1, xwloc2, ywloc2;
{

	/*=====================================================================
	 * PURPOSE:  To draw a line between two world locations.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xwloc1:  First X world coordinate. [f]
	 *    ywloc1:  First Y world coordinate. [f]
	 *    xwloc2:  Second X world coordinate. [f]
	 *    ywloc2:  Second Y world coordinate. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gtm/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   worldmove, worlddraw
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	worldmove( xwloc1, ywloc1 );
	worlddraw( xwloc2, ywloc2 );

L_8888:
	return;

} /* end of function */

