#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ getcontpoint(number, point, link, action)
int number;
float point[];
int *link, *action;
{
	float *const Point = &point[0] - 1;
        int *Isacmem;

	/*=====================================================================
	 * PURPOSE:  To get information about an existing contouring line point.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  The point number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    point:   The point. First value is x, second is y. [f2]
	 *    link:    The link to the next point in segment. [i]
	 *    action:  Action to perform at new point. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mem:          sacmem, isacmem
	 *    contouring:   numpoints, indexpoints, indexlinks, indexaction
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900405:  Added action attribute storage.
	 *    900315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900405
	 *===================================================================== */
	/* PROCEDURE: */
	if( number <= cmcontouring.numpoints ){
		Point[1] = *(cmmem.sacmem[cmcontouring.indexpoints] + 2*(number - 1));
		Point[2] = *(cmmem.sacmem[cmcontouring.indexpoints] + 2*(number - 1) + 1);
                Isacmem = (int*)cmmem.sacmem[cmcontouring.indexaction];
		*action = *(Isacmem + number - 1);
                Isacmem = (int*)cmmem.sacmem[cmcontouring.indexlinks];
		*link = *(Isacmem + number - 1);
	}
	else{
		fprintf( stdout, "Illegal point number:%d getcontpoint \n", number );
		exit(0);
	}

L_8888:
	return;

} /* end of function */

