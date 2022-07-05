#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ cursortext2(xloc, yloc, ktext)
double xloc, yloc;
char *ktext;
{

	/*=====================================================================
	 * PURPOSE:  To perform "cursor text input function on device 2 (SGF.)
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    XLOC:    X location of cursor when character was struck. [f]
	 *    YLOC:    Y location of cursor. [f]
	 *    KTEXT:   Text entered in response to cursor. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870323:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870323
	 *===================================================================== */
	/* PROCEDURE: */
	/* - This is a no-op for this device-driver. */
L_8888:
	return;

} /* end of function */

