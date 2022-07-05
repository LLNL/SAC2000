#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ settextjust(khorz, kvert)
char *khorz, *kvert;
{



	/*=====================================================================
	 * PURPOSE:  To change the graphics text justification.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    khorz:   Horizontal text justification desired. [c]
	 *             = 'LEFT' for left justification.
	 *             = 'CENTER' for centered justification.
	 *             = 'RIGHT' for right justification.
	 *             (Only the first character need be entered.)
	 *    kvert:   Vertical text justification desired. [c]
	 *             = 'BOTTOM' for bottom justification.
	 *             = 'CENTER' for centered justification.
	 *             = 'TOP' for TOP justification.
	 *             (Only the first character need be entered.)
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     ihjust, ivjust
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861017:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Only check first character, but check for upper and lower case. */
	/* - Errors result in default (LEFT, BOTTOM). */
	if( khorz[0] == 'L' || khorz[0] == 'l' ){
		cmgdm.ihjust = 1;
		}
	else if( khorz[0] == 'C' || khorz[0] == 'c' ){
		cmgdm.ihjust = 2;
		}
	else if( khorz[0] == 'R' || khorz[0] == 'r' ){
		cmgdm.ihjust = 3;
		}
	else{
		cmgdm.ihjust = 1;
		}

	if( kvert[0] == 'B' || kvert[0] == 'b' ){
		cmgdm.ivjust = 1;
		}
	else if( kvert[0] == 'C' || kvert[0] == 'c' ){
		cmgdm.ivjust = 2;
		}
	else if( kvert[0] == 'T' || kvert[0] == 't' ){
		cmgdm.ivjust = 3;
		}
	else{
		cmgdm.ivjust = 1;
		}

L_8888:
	return;

} /* end of function */

