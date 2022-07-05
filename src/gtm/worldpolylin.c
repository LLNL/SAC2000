#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ worldpolyline(xwloc, ywloc, number)
float xwloc[], ywloc[];
int number;
{
	int j, j_;

	float *const Xwloc = &xwloc[0] - 1;
	float *const Ywloc = &ywloc[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To draw a line through a set of world locations.
	 *=====================================================================
	 * SPECIAL NOTE:  This polyline subroutine does NOT conform to the
	 *                SIGGRAPH standard in that it moves to the first
	 *                data point as opposed to drawing to it from the CP.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xwloc:   Array of x world coordinates. [fa]
	 *    ywloc:   Array of y world coordinates. [fa]
	 *    number:  Length of xwloc and ywloc arrays. [i]
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
	worldmove( Xwloc[1], Ywloc[1] );

	for( j = 2; j <= number; j++ ){
		j_ = j - 1;
		worlddraw( Xwloc[j], Ywloc[j] );
		}

L_8888:
	return;

} /* end of function */

