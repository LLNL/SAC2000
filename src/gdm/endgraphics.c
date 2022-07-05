#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ endgraphics(nerr)
int *nerr;
{
	void enddevice3(), enddevice4(), enddevice5();



	/*=====================================================================
	 * PURPOSE:  To end (terminate) the graphics library.
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
	 * GLOBAL INPUT:
	 *    gdm:     lginit
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  enddevice1, endgraphicsd2, enddevice3, enddevice4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861010:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861010
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Terminate each active graphic device. */

	if( Lgdon[1] ){
		enddevice1( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[2] ){
		enddevice2( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[3] ){
		enddevice3( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[4] ){
		enddevice4( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[5] ){
		enddevice5( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	/* - Turn "initialized" flag off. */

	cmgdm.lginit = FALSE;

L_8888:
	return;

} /* end of function */

