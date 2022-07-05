#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ setvspacetype(lfull, ratio)
int lfull;
double ratio;
{



	/*=====================================================================
	 * PURPOSE:  To define the type of viewspace.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    lfull:   Set to .TRUE. for the full viewspace option. [l]
	 *             Set to .FALSE. for the fixed aspect ratio option.
	 *    ratio:   Desired fixed aspect ratio if lfull is .FALSE. [f]
	 *             Ignored if lfull is .TRUE.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lvsful,vsrat
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861021:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861021
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Save input variables in common. */
	cmgdm.lvsful = lfull;
	cmgdm.vsrat = ratio;

L_8888:
	return;

} /* end of function */

