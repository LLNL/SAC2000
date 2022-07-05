#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ getratio2(aspect)
float *aspect;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the aspect ratio (ratio of y viewport
	 *           size to x viewport size) for graphics device 2 (SGF).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ASPECT:  Aspect ratio. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	*aspect = 0.75;

L_8888:
	return;

} /* end of function */

