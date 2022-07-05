#include <stdio.h>
#include <stdlib.h>
#include <math.h>
void /*FUNCTION*/ copy(srce, sink, ncopy)
int srce[], sink[], ncopy;
{
	int j, lsink, lsrce;
	void zmemad();

	int *const Sink = &sink[0] - 1;
	int *const Srce = &srce[0] - 1;


	/*=====================================================================
	 * PURPOSE: To copy real arrays within memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *      SRCE:  Source array; elements moved from here.
	 *     NCOPY:  Number of elements to move. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      SINK:  Sink array; elements are moved to here.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZMEMAD
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    LSRCE:   Location of source array in memory. [i]
	 *    LSINK:   Location of sink array in memory. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Use ZMEMAD to get memory locations of source and sink arrays.
	 *  (These locations are in 16-bit words or
	 *   half a floating point value.) */
	zmemad( &Srce[1], &lsrce );
	zmemad( &Sink[1], &lsink );

	/* - No-op if the source and sink are in same location or
	 *   NCOPY is non-positive. */

	if( lsrce == lsink || ncopy <= 0 )
	{ /* do nothing */ }

	/* - Copy arrays in normal order if the location of the source array
	 *   is in a higher memory location than the sink array OR
	 *   there is no overlap. */
	else if( lsrce > lsink || (lsrce + 2*ncopy) < lsink ){
		for( j = 1; j <= ncopy; j++ ){
			Sink[j] = Srce[j];
		}
	}

	/* - Copy arrays in reverse order if the location of the source array
	 *   is in a lower memory location than the sink array. */
	else{
		for( j = ncopy; j >= 1; j-- ){
			Sink[j] = Srce[j];
		}
	}

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850315:  Now treating arrays as integer not real.
	 *    841221:  Fixed bug in computing array overlap.
	 *    831005:  Made independent by adding call to ZMEMAD.
	 *    800822:  Fixed bug in the logic governing forward/reverse copying.
	 *    800103:  Original Prime version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850109
	 *===================================================================== */

} /* end of function */

