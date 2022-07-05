#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ cursor2(xloc, yloc, kchar)
double xloc, yloc;
int kchar;
{

	/*=====================================================================
	 * PURPOSE:  To perform "graphics input function on device 2 (SGF).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    XLOC:    X location of cursor when character was struck. [f]
	 *    YLOC:    Y location of cursor. [f]
	 *    KCHAR:   Character struck in response to cursor. [c1]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *===================================================================== */
	/* PROCEDURE: */
	/* - This is a no-op for this graphics device. */
L_8888:
	return;


} /* end of function */

