#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MPOINTS	3

#include "../../inc/mach.h"
#include "../../inc/contouring.h"
void /*FUNCTION*/ calccontlabels()
{
	int nextcontseg;
	int jaction[MPOINTS], jlevel, jlink[MPOINTS], jpoint[MPOINTS], 
	 jsegment, jstart, jstop, nerr;
	float point[MPOINTS][2];

	int *const Jaction = &jaction[0] - 1;
	int *const Jlink = &jlink[0] - 1;
	int *const Jpoint = &jpoint[0] - 1;


	/*=====================================================================
	 * PURPOSE: To calculate contouring line label locations.
	 *=====================================================================
	 * MODULE/LEVEL:  contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    contouring:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:   AllocSegments, CalcContLabel1, CalcContLabel2, 
	 *           CalcContLabel3, CalcContLabel4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900821:  Shortened parameter names to 15 characters to keep things
	 *             working under SunOS 3.5:
	 *                  indexseglabelstatus -> indexseglabelst
	 *                  indexseglabelnumber -> indexseglabelnu
	 *                  indexseglabelfirst  -> indexseglabelfi
	 *    900419:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900419
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Allocate additional space in which to store segment label information. */
	cmcontouring.maxlabels = 4*cmcontouring.numsegments;
	alloclabels( cmcontouring.maxsegments, cmcontouring.maxlabels, 
	 &cmcontouring.indexseglabelst, &cmcontouring.indexseglabelnu, 
	 &cmcontouring.indexseglabelfi, &cmcontouring.indexlabelpoint, 
	 &cmcontouring.indexlabeltype, &cmcontouring.indexlabelangle, 
	 &cmcontouring.indexlabeltext, &nerr );
	cmcontouring.numlabels = 0;

	/* - First pass to determine initial candidate locations. */

	calccontlabel1();

	/* - Second pass to iterate to select locations from candidate locations. */

	/*      call CalcContLabel2 */

	/* - Third pass to either select or reject remaining locations. */

	/*      call CalcContLabel3 */

	/* - Fourth pass to modify segment storage to accomidate selected labels. */

	calccontlabel4();

L_8888:
	return;

} /* end of function */

