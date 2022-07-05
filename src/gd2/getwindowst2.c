#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ getwindowstat2(number, exists)
int number;
int *exists;
{

	/*=====================================================================
	 * PURPOSE: To get graphics window attributes for device 2 (SGF.)
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NUMBER:  The number of the graphic window. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    EXISTS:  Set to .TRUE. if graphics window exists. [l]
	 *             Potential error numbers: 0201.
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861201:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850506
	 *===================================================================== */
	/* PROCEDURE: */
	/* - This is a no-op for this graphics device. */
	*exists = FALSE;

L_8888:
	return;

} /* end of function */

