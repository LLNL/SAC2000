#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getvspacetype(lfull, ratio)
int *lfull;
float *ratio;
{



	/*=====================================================================
	 * PURPOSE:  To get the current viewspace attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    lfull:   Set to .TRUE. for the full viewspace option. [l]
	 *             Set to .FALSE. for the fixed aspect ratio option.
	 *    ratio:   Current fixed aspect ratio if lfull is .FALSE. [f]
	 *             Unused if lfull is .TRUE.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lvsful,vsrat
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890927:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890927
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return current variables from common. */
	*lfull = cmgdm.lvsful;
	*ratio = cmgdm.vsrat;

L_8888:
	return;

} /* end of function */

