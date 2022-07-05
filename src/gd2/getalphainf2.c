#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ getalphainfo2(nlines, erase)
int nlines;
char *erase;
{

	/*=====================================================================
	 * PURPOSE: To inquire about text attributes of graphics device 2.
	 *=====================================================================
	 * MODULE/LEVEL: GD2/4
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NLINES:  Number of text lines per screen. [i]
	 *    ERASE:   Text to send to erase terminal screen. [c]
	 *             Set to all blanks is terminal has scrolling capability.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - This is a no-op for this graphics device. */
L_8888:
	return;

} /* end of function */

