#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ cursoroff()
{
	int jgd, jgd_, nerr;



	/*=====================================================================
	 * PURPOSE: To reverse what cursoron did.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     iflcur
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     iflcur
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  enddevice, begindevice
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831027:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	/* - For each graphics device:
	 *   (1) Turn in back on, if it was temporarily turned off.
	 *   (2) Turn in off, if it was temporarily turned on.
	 *   (3) Reset flags to zero. */
	for( jgd = 1; jgd <= MGD; jgd++ ){
		jgd_ = jgd - 1;
		if( Iflcur[jgd] < 0 ){
			begindevice( (char*)kmgdm.kgdnam[jgd_],13, &nerr );
			Iflcur[jgd] = 0;
			}
		else if( Iflcur[jgd] > 0 ){
			enddevice( (char*)kmgdm.kgdnam[jgd_],13, &nerr );
			Iflcur[jgd] = 0;
			}
		}

L_8888:
	return;

} /* end of function */

