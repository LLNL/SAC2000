#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ cursoron()
{
	int jgd, jgd_, nerr;



	/*=====================================================================
	 * PURPOSE: To tell graphics library that the next plots will be using
	 *          the cursor.  This routine temporarily turns off all graphics
	 *          devices, except for the one enabled for graphics input.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon, igdcur
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
	 *   (1) If it is on and not the cursor device, turn it off.
	 *   (2) If it is off and the cursor device, turn in on.
	 *   (3) Otherwise, do nothing. */
	/* - Set a flags so that cursoroff will know how to undo what cursoron did. */
	for( jgd = 1; jgd <= MGD; jgd++ ){
		jgd_ = jgd - 1;
		if( Lgdon[jgd] && jgd != cmgdm.igdcur ){
			enddevice( (char*)kmgdm.kgdnam[jgd_],13, &nerr );
			Iflcur[jgd] = -1;
			}
		else if( !Lgdon[jgd] && cmgdm.igdcur == jgd ){
			begindevice( (char*)kmgdm.kgdnam[jgd_],13, &nerr );
			Iflcur[jgd] = 1;
			}
		else{
			Iflcur[jgd] = 0;
			}
		}

L_8888:
	return;

} /* end of function */

