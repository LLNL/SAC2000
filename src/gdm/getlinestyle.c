#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getlinestyle(istyle)
int *istyle;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the current linestyle attribute.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    istyle:  Linestyle attribute. [i]
	 *             = 1 for a solid line.
	 *             > 1 for device-specific linestyles.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     iline
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861026
	 *===================================================================== */
	/* PROCEDURE: */
	*istyle = cmgdm.iline;

L_8888:
	return;

} /* end of function */

