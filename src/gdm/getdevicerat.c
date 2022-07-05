#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getdeviceratio(ratio)
float *ratio;
{
	float ratio1, ratio2, ratio3, ratio4, ratio5;
	void getdevicerat3(), getdevicerat4(), getdevicerat5();



	/*=====================================================================
	 * *** INTERNAL SUBROUTINE:  NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To inquire about the current physical device aspect ratio.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ratio:   Aspect ratio of the currently active graphics device. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getdevicerat1, getdevicerat2, getdevicerat3, getdevicerat4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870217:  Changed name from getscreenatr.
	 *    861026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861026
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine the minimum screen aspect (y to x) ratio of all active
	 *   graphics devices. */
	*ratio = VLARGE;
	if( Lgdon[1] ){
		getdevicerat1( &ratio1 );
		*ratio = fmin( *ratio, ratio1 );
		}
	if( Lgdon[2] ){
		getdevicerat2( &ratio2 );
		*ratio = fmin( *ratio, ratio2 );
		}
	if( Lgdon[3] ){
		getdevicerat3( &ratio3 );
		*ratio = fmin( *ratio, ratio3 );
		}
	if( Lgdon[4] ){
		getdevicerat4( &ratio4 );
		*ratio = fmin( *ratio, ratio4 );
		}
	if( Lgdon[5] ){
		getdevicerat5( &ratio5 );
		*ratio = fmin( *ratio, ratio5 );
		}

	if( *ratio == VLARGE )
		*ratio = 1.0;

L_8888:
	return;

} /* end of function */

