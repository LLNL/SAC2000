#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ calstatus()
{
	int jgd, jgd_;



	/*=====================================================================
	 * *** INTERNAL SUBROUTINE: NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE: To compute several graphic device status variables.
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     mgd, lgdon, igdtyp
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     lactiv, lpasiv, lcur
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - For each graphics device that is on:
	 * -- If it is a passive device set lpasiv to .TRUE.
	 * -- If it is an active device set lactiv to .TRUE. */
	cmgdm.lactiv = FALSE;
	cmgdm.lpasiv = FALSE;
	for( jgd = 1; jgd <= MGD; jgd++ ){
		jgd_ = jgd - 1;
		if( Lgdon[jgd] ){
			if( Igdtyp[jgd] <= 1 ){
				cmgdm.lpasiv = TRUE;
				}
			else{
				cmgdm.lactiv = TRUE;
				}
			}
		}

	/* - See if there is a graphics device that is on that can perform
	 *   graphics input (cursor) functions. */

	cmgdm.lcur = FALSE;
	cmgdm.igdcur = 0;
	jgd = 1;
L_3000:
	if( jgd <= MGD && cmgdm.igdcur <= 0 ){
		if( Lgdon[jgd] && Igdtyp[jgd] >= 3 ){
			cmgdm.lcur = TRUE;
			cmgdm.igdcur = jgd;
			}
		jgd = jgd + 1;
		goto L_3000;
		}

L_8888:
	return;


} /* end of function */

