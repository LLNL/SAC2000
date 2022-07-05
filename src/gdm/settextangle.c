#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ settextangle(angle)
double angle;
{



	/*=====================================================================
	 * PURPOSE:  To change the text angle.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    angle:   Angle in degrees counter-clockwise from horizontal. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     tangle
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861017:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Save value passed in. */
	cmgdm.tangle = angle;
	if( Lgdon[2] )
		settextangle2( cmgdm.tangle );

L_8888:
	return;

} /* end of function */

