#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getmaxdevices(number)
int *number;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the maximum number of graphics devices.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    number:  Maximum number of graphics devices in library. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     mgd
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870514:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870514
	 *===================================================================== */
	/* PROCEDURE: */
	*number = MGD;

L_8888:
	return;

} /* end of function */

