#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ setvspaceclip(lclip)
int lclip;
{



	/*=====================================================================
	 * PURPOSE:  To turn viewspace clipping on or off.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    lclip:   Set to .TRUE. to turn viewspace clipping on. [l]
	 *             Set to .FALSE. to turn viewspace clipping off.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lvsclip
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870501:  Original version.
	 *====================================================================
	 * DOCUMENTED/REVIEWED:  870501
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Save input variables in common. */
	cmgdm.lvsclip = lclip;

L_8888:
	return;

} /* end of function */

