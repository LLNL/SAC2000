#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getdevicename(number, name, name_s)
int number;
char *name;   int name_s;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about the name of a graphics device.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    number:  The number of the graphics device. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    name:    The name of the device, up to 12 characters. [c]
	 *             Set to all blanks, if the a bad device number is input.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     mgd, kgdnam
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870514:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870514
	 *===================================================================== */
	/* PROCEDURE: */
	if( number > 0 && number <= MGD ){
		fstrncpy( name, name_s-1, kmgdm.kgdnam[number - 1],
                                   strlen(kmgdm.kgdnam[number - 1]));
		}
	else{
		fstrncpy( name, name_s-1, " ", 1 );
		}

L_8888:
	return;

} /* end of function */

