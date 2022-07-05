#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"

void flushbuffer3(int* nerr);
void flushbuffer4(int* nerr);
void flushbuffer5(int* nerr);

void flushbuffer(int* nerr)
{
	



	/*=====================================================================
	 * *** INTERNAL SUBROUTINE:  NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To flush the buffers of all graphics devices.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  flushbuffer1, flushbuffer2, flushbuffer3, flushbuffer4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861014:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861014
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Flush the buffers for each active graphics device. */

	if( Lgdon[1] ){
		flushbuffer1( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[2] ){
		flushbuffer2( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[3] ){

		flushbuffer3( nerr );

		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[4] ){
		flushbuffer4( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[5] ){

		flushbuffer5( nerr );

		if( *nerr != 0 )
			goto L_8888;
		}

L_8888:
	return;

} /* end of function */

