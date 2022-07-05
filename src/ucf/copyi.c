#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ copyi(isrce, isink, ncopy)
int isrce[], isink[], ncopy;
{
	int j, j_, lsink, lsrce;
	void zmemad();

	int *const Isink = &isink[0] - 1;
	int *const Isrce = &isrce[0] - 1;


	/* ind
	 *=====================================================================
	 * PURPOSE: To copy integer arrays within memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     ISRCE:  Source array; elements moved from here. [i]
	 *     NCOPY:  Number of elements to move. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     ISINK:  Sink array; elements are moved to here. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZMEMAD
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine memory addresses of start of source and sink arrays. */
	zmemad( &Isrce[1], &lsrce );
	zmemad( &Isink[1], &lsink );

	/* - No-op if the source and sink are in same location or
	 *   NCOPY is non-positive. */

	if( lsrce == lsink || ncopy <= 0 ){

		/* - Copy arrays in normal order if the location of the source array
		 *   is in a higher memory location than the sink array OR
		 *   there is no overlap. */

		}
	else if( lsrce > lsink || (lsrce + 2*ncopy) < lsink ){
		for( j = 1; j <= ncopy; j++ ){
			j_ = j - 1;
			Isink[j] = Isrce[j];
			}

		/* - Copy arrays in reverse order if the location of the source array
		 *   is in a lower memory location than the sink array. */

		}
	else{
		for( j = ncopy; j >= 1; j-- ){
			j_ = j - 1;
			Isink[j] = Isrce[j];
			}

		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    Fixed bug involving computation of overlap.
	 *    831005:  Made independent by adding call to ZMEMAD.
	 *    830810:  Replaced with version based upon ZCOPY.
	 *    800822:  Fixed bug in the logic governing forward/reverse copying.
	 *    800103:  Original Prime version.
	 *===================================================================== */

} /* end of function */

