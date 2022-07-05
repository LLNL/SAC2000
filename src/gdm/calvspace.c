#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ calvspace()
{
	float vsdes, vsdev;



	/*=====================================================================
	 * *** INTERNAL SUBROUTINE: NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To calculate the current graphics viewspace.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lvsful, vsrat
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     xvs, yvs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getratio
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Major restructuring.
	 *    840911:  Major modifications to allow for any viewspace ratio.
	 *             Moved from dependent to independent sources.
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861021
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine the minimum viewspace (y to x) ratio of all active
	 *   graphics devices. */
	getratio( &vsdev );

	/* - Determine desired viewspace ratio.  This is either a the
	 *   viewspace value calculated above or a specific requested ratio. */

	if( cmgdm.lvsful ){
		vsdes = vsdev;
		}
	else{
		vsdes = cmgdm.vsrat;
		}

	/* - Calculate the viewspace limits so that the desired viewspace
	 *   will fit onto all of the active graphics devices. */

	if( vsdes <= vsdev ){
		Xvs[1] = 0.;
		Xvs[2] = 1.;
		Yvs[1] = 0.;
		Yvs[2] = vsdes;
		}
	else{
		Xvs[1] = 0.5*(1.0 - vsdev/vsdes);
		Xvs[2] = 1.0 - Xvs[1];
		Yvs[1] = 0.;
		Yvs[2] = vsdev;
		}

L_8888:
	return;

} /* end of function */

