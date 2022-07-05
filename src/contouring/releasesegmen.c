#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/mem.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ releasesegments()
{
	int nerr;



	/*=====================================================================
	 * PURPOSE:  To release storage for contour line segments.
	 *=====================================================================
	 * MODULE/LEVEL:  contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *     mem:         sacmem, isacmem
	 *     contouring:  indexlevels, indexstarts, indexstops
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *     contouring:  indexlevels, indexstarts, indexstops
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *     sac:  relamb
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900315
	 *===================================================================== */
	/* PROCEDURE: */
	nerr = 0;

	/* - Release space for contour level values. */

	relamb( cmmem.sacmem, cmcontouring.indexlevels, &nerr );
	cmcontouring.indexlevels = 0;

	/* - Release space for segment start point numbers. */

	relamb( cmmem.sacmem, cmcontouring.indexstarts, &nerr );
	cmcontouring.indexstarts = 0;

	/* - Release space for segment stop point numbers. */

	relamb( cmmem.sacmem, cmcontouring.indexstops, &nerr );
	cmcontouring.indexstops = 0;

L_8888:
	return;

} /* end of function */

