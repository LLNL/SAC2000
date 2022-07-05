#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gtm.h"
void /*FUNCTION*/ calwvtransform()
{



	/*=====================================================================
	 * *** INTERNAL SUBROUTINE: NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To calculate the world to viewport mapping transformation.
	 *=====================================================================
	 * MODULE/LEVEL:  gtm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gtm:     xwcmin, xwcmax, ywcmin, ywcmax,
	 *             xvpmin, xvpmax, yvpmin, yvpmax
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gtm:     xmpwv1, xmpwv2, ympwv1, ympwv2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861027:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861027
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Define world/viewport mapping transformation. */
	cmgtm.xmpwv1 = (cmgtm.xvpmax - cmgtm.xvpmin)/(cmgtm.xwcmax - cmgtm.xwcmin);
	cmgtm.xmpwv2 = -cmgtm.xmpwv1*cmgtm.xwcmax + cmgtm.xvpmax;
	cmgtm.ympwv1 = (cmgtm.yvpmax - cmgtm.yvpmin)/(cmgtm.ywcmax - cmgtm.ywcmin);
	cmgtm.ympwv2 = -cmgtm.ympwv1*cmgtm.ywcmax + cmgtm.yvpmax;

L_8888:
	return;

} /* end of function */

