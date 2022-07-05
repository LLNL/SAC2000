#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ getdevicerat2(ratio)
float *ratio;
{

	/*=====================================================================
	 * PURPOSE:  To inquire about the graphics screen aspect ratio.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    RATIO:   Aspect ratio of graphics device 2. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861026
	 *===================================================================== */
	/* PROCEDURE: */
	*ratio = 0.75;

L_8888:
	return;

} /* end of function */

