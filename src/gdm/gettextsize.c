#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ gettextsize(width, height)
float *width, *height;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the current size of graphics text.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    width:   The width of a single character in viewport units. [f]
	 *    height:  The height of a single line in viewport units. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     twidth, thgt
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	*width = cmgdm.twidth;
	*height = cmgdm.thgt;

L_8888:
	return;

} /* end of function */

