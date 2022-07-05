#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gdm.h"
void /*FUNCTION*/ getratio(ratio)
float *ratio;
{
	float ratio1, ratio2, ratio3, ratio4, ratio5;
	void getratio3(), getratio4(), getratio5();



	/*=====================================================================
	 * PURPOSE:  To inquire about the current viewspace aspect ratio.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ratio:   Minimum aspect ratio of all active graphics devices. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    vlarge
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getratio1, getratio2, getratio3, getratio4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861026
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine the minimum viewspace (y to x) ratio of all active
	 *   graphics devices. */
	*ratio = VLARGE;
	if( Lgdon[1] ){
		getratio1( &ratio1 );
		*ratio = fmin( *ratio, ratio1 );
	}
	if( Lgdon[2] ){
		getratio2( &ratio2 );
		*ratio = fmin( *ratio, ratio2 );
	}
	if( Lgdon[3] ){
		getratio3( &ratio3 );
		*ratio = fmin( *ratio, ratio3 );
	}
	if( Lgdon[4] ){
		getratio4( &ratio4 );
		*ratio = fmin( *ratio, ratio4 );
	}
	if( Lgdon[5] ){
		getratio5( &ratio5 );
		*ratio = fmin( *ratio, ratio5 );
	}

	if( *ratio == VLARGE )
		*ratio = 1.0;

L_8888:
	return;

} /* end of function */

