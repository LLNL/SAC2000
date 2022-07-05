#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	IABOVE	8
#define	IBELOW	4
#define	ILEFT	1
#define	IRIGHT	2

#include "../../inc/mach.h"
void /*FUNCTION*/ clipdp(xdp, ydp, ildp, xrect, yrect, ncdp)
float xdp[], ydp[];
int ildp[];
float xrect[], yrect[];
int *ncdp;
{
	int _l0, j1, j2;
	float dxdp, dydp;

	int *const Ildp = &ildp[0] - 1;
	float *const Xdp = &xdp[0] - 1;
	float *const Xrect = &xrect[0] - 1;
	float *const Ydp = &ydp[0] - 1;
	float *const Yrect = &yrect[0] - 1;


	/*=====================================================================
	 * PURPOSE: To clip a line segment to a rectangle.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    XDP:     Array of unclipped x data points [len=2].
	 *    YDP:     Array of unclipped y data points [len=2].
	 *    ILDP:    Array of unclipped data point location values [len=2].
	 *             See subroutine LOCDP for complete definition.
	 *    XRECT:   Array of x rectangle values [len=2; left edge first].
	 *    YRECT:   Array of y rectangle values [len=2; bottom edge first].
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    XDP:     Array of clipped x data points [len=2].
	 *    YDP:     Array of clipped y data points [len=2].
	 *    NCDP:    Length of clipped data arrays.
	 *             =0 if none of line segment was inside rectangle.
	 *             =2 if all or part of line segment was inside rectangle.
	 *=====================================================================
	 * MODULE/LEVEL: GAM/4
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Values for parameters IABOVE, IBELOW, ILEFT, and IRIGHT must be
	 *   the same in the clipping subroutines: CLIP, BLANK, and LOCDP.
	 *   These parameters are defined in source of subroutine LOCDP.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LOCDP
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    J1,J2:   Indices into XDP, YDP, and ILDP arrays.
	 *             Indices are swapped instead of array values in algorithm.
	 *=====================================================================
	 * ASSUMPTIONS:
	 * -  XRECT(2) > XRECT(1) and YRECT(2) > YRECT(1).
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Each pass through loop moves one end closer to the rectangle.
	 *   Complete clipping may require several iterations. */
L_2000:
	;

	/* -- Set up indices so that the "J1" point is outside rectangle. */
	if( Ildp[1] != 0 ){
		j1 = 1;
		j2 = 2;
		}
	else{
		j1 = 2;
		j2 = 1;
		}

	/* -- Compute length of line segment in x and y directions. */
	dxdp = Xdp[j2] - Xdp[j1];
	dydp = Ydp[j2] - Ydp[j1];

	/* -- Move "J1" point toward rectangle. */
	if( ( Ildp[j1] & IBELOW ) != 0 ){
		Xdp[j1] = Xdp[j1] + (Yrect[1] - Ydp[j1])*dxdp/dydp;
		Ydp[j1] = Yrect[1];
		}
	else if( ( Ildp[j1] & IABOVE ) != 0 ){
		Xdp[j1] = Xdp[j1] + (Yrect[2] - Ydp[j1])*dxdp/dydp;
		Ydp[j1] = Yrect[2];
		}
	else if( ( Ildp[j1] & ILEFT ) != 0 ){
		Ydp[j1] = Ydp[j1] + (Xrect[1] - Xdp[j1])*dydp/dxdp;
		Xdp[j1] = Xrect[1];
		}
	else if( ( Ildp[j1] & IRIGHT ) != 0 ){
		Ydp[j1] = Ydp[j1] + (Xrect[2] - Xdp[j1])*dydp/dxdp;
		Xdp[j1] = Xrect[2];
		}

	/* -- Recompute the location of the "J1" point. */
	locdp( Xdp[j1], Ydp[j1], xrect, yrect, &Ildp[j1] );

	/* -- Return if both points are now inside rectangle. */
	if( Ildp[1] + Ildp[2] == 0 ){
		*ncdp = 2;
		goto L_8888;
		/* -- Return if it can be determined that entire segment is outside. */
		}
	else if( ( Ildp[1] & Ildp[2] ) != 0 ){
		*ncdp = 0;
		goto L_8888;
		/* -- Otherwise continue the iterative clipping procedure. */
		}
	else{
		goto L_2000;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    840105:  Corrected bug involving line segment outside rectangle.
	 *    811222:  Original version.
	 *===================================================================== */

} /* end of function */

