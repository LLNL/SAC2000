#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ erase(nerr)
int *nerr;
{
	void erase3(), erase4(), erase5();



	/*=====================================================================
	 * PURPOSE: To erase the screen (or window) of all active devices.
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  erase1, erase2, erase3, erase4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *     870217:  Changed name from erasescreen.
	 *     831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870217
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Perform erase screen action if necessary. */

	if( Lgdon[1] ){
		erase1();
		/*       if(nerr.ne.0)go to 8888 */
		}

	if( Lgdon[2] ){
		erase2();
		/*       if(nerr.ne.0)go to 8888 */
		}

	if( Lgdon[3] ){
		erase3( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[4] ){
		erase4( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[5] ){
		erase5( nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

L_8888:
	return;

} /* end of function */

