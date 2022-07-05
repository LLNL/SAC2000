#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getstatus(kstat, ltrue)
char *kstat;
int *ltrue;
{



	/*=====================================================================
	 * PURPOSE: To inquire about several graphic device status variables.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kstat:   Name of desired status variable. [c]
	 *             ='ANY' to see if any devices are currently on.
	 *             ='ACTIVE' to see if any active devices are currently on.
	 *             ='PASSIVE' to see if any passive devices are on.
	 *             ='CURSOR' to see if any active devices with cursor are on.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ltrue:   Set to .TRUE. if the attribute is true. [l]
	 *             Set to .FALSE. if attibute is not true or unknown.
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lactiv, lpasiv, lcur
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	if( memcmp(kstat,"AN",2) == 0 || memcmp(kstat,"an",2) == 0 ){
		*ltrue = cmgdm.lactiv || cmgdm.lpasiv;
		}
	else if( memcmp(kstat,"AC",2) == 0 || memcmp(kstat,"ac",2) == 0 ){
		*ltrue = cmgdm.lactiv;
		}
	else if( kstat[0] == 'P' || kstat[0] == 'p' ){
		*ltrue = cmgdm.lpasiv;
		}
	else if( kstat[0] == 'C' || kstat[0] == 'c' ){
		*ltrue = cmgdm.lcur;
		}
	else{
		*ltrue = FALSE;
		}

L_8888:
	return;

} /* end of function */

