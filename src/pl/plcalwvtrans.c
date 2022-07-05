#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"
void /*FUNCTION*/ plcalwvtrans()
{



	/*=====================================================================
	 * *** INTERNAL SUBROUTINE: NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To calculate the world (input) to viewport (plot)
	 *           coordinate mapping transformation.
	 *=====================================================================
	 * MODULE/LEVEL:  gem/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gem:     xpmnu, xpmxu, ypmnu, ypmxu, ximnz, ximxz, yimnz, yimxz
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     xmpip1, xmpip2, ympip1, ympip2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900307:  Original version from bottom of PLMAP.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900307
	 *===================================================================== */
	/* PROCEDURE: */
	/* - The mapping constants (slope and intercept) needed to convert from
	 *   input coordinates to plot coordinates  (i.e. normalized device
	 *   coordinates in the range 0. to 1.) are calculated here. */
	cmgem.xmpip1 = (cmgem.xpmxu - cmgem.xpmnu)/(cmgem.ximxz - cmgem.ximnz);
	cmgem.xmpip2 = -cmgem.xmpip1*cmgem.ximxz + cmgem.xpmxu;
	cmgem.ympip1 = (cmgem.ypmxu - cmgem.ypmnu)/(cmgem.yimxz - cmgem.yimnz);
	cmgem.ympip2 = -cmgem.ympip1*cmgem.yimxz + cmgem.ypmxu;

L_8888:
	return;

} /* end of function */

