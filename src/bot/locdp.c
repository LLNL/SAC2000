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
void /*FUNCTION*/ locdp(x, y, xr, yr, ilocdp)
double x, y;
float xr[], yr[];
int *ilocdp;
{

	float *const Xr = &xr[0] - 1;
	float *const Yr = &yr[0] - 1;


	/* Ind
	 *=====================================================================
	 * PURPOSE: To determine the location of a data point relative to
	 *          a rectangular region.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    X:       X data point value.
	 *    Y:       Y data point value.
	 *    XR:      Array of x rectangle values [len=2; left edge first].
	 *    YR:      Array of y rectangle values [len=2; bottom edge first].
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ILOCDP:  Location of data point relative to rectangle.
	 *             The four rightmost bits mean the following if set:
	 *             Bit 4: data point is above top of rectangle.
	 *             Bit 3: data point is below bottom of rectangle.
	 *             Bit 2: data point is to right of rectangle.
	 *             Bit 1: data point is to left of rectangle.
	 *             There are therefore nine possible values for ILOC:
	 *             = Bits 1001 ( 9) if above and to the left of rectangle.
	 *             = Bits 1000 ( 8) if above rectangle.
	 *             = Bits 1010 (10) if above and to the right of rectangle.
	 *             = Bits 0001 ( 1) if to the left of rectangle.
	 *             = Bits 0000 ( 0) if inside rectangle.
	 *             = Bits 0010 ( 2) if to the right of rectangle.
	 *             = Bits 0101 ( 5) if below and to the left of rectangle.
	 *             = Bits 0100 ( 4) if below rectangle.
	 *             = Bits 0110 ( 6) if below and to the right of rectangle.
	 *=====================================================================
	 * MODULE/LEVEL: GAM/4
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Values for parameters IABOVE, IBELOW, ILEFT, and IRIGHT must be
	 *   the same in the clipping subroutines: CLIP, BLANK, and LOCDP.
	 *   The meaning of these parameters is given below.
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    IABOVE:  Constant to indicate above rectangle [= bits 1000 (8)].
	 *    IBELOW:  Constant to indicate below rectangle [= bits 0100 (4)].
	 *    ILEFT:   Constant to indicate left of rectangle [= bits 0010 (2)].
	 *    IRIGHT:  Constant to indicate right of rectangle [= bits 0001 (1)].
	 *=====================================================================
	 * REFERENCES:
	 * - Newman, William M. and Robert F. Sproull, PRINCIPLES OF INTERACTIVE
	 *   COMPUTER GRAPHICS, McGraw-Hill, 1939, pp 123-124.
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Assume data point is inside rectangle. */
	*ilocdp = 0;

	/* - Check to see if data point is above or below rectangle. */

	if( y > Yr[2] ){
		*ilocdp = IABOVE;
		}
	else if( y < Yr[1] ){
		*ilocdp = IBELOW;
		}

	/* - Check to see if data point is to left or right of rectangle. */

	if( x > Xr[2] ){
		*ilocdp = *ilocdp + IRIGHT;
		}
	else if( x < Xr[1] ){
		*ilocdp = *ilocdp + ILEFT;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    811222:  Original version.
	 *===================================================================== */

} /* end of function */

