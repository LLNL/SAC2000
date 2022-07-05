#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ createwindow(number, xwinmn, xwinmx, ywinmn, ywinmx, 
	 nerr)
int *number;
double xwinmn, xwinmx, ywinmn, ywinmx;
int *nerr;
{
	float ratio, xwmn, xwmx, ywmn, ywmx;
	void createwindow3(), createwindow4(), getdevicerat3(), getdevicerat4();
        void createwindow5(), getdevicerat5();


	/*=====================================================================
	 * PURPOSE: To create a new graphics window.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NUMBER:  Number of graphic window to "turn on". [i]
	 *    XWINMN:  Minimum x location of window on screen. [f]
	 *    XWINMX:  Maximum x location of window on screen. [f]
	 *             Units for x locations is 0.0 (left) to 1.0 (right).
	 *    YWINMN:  Minimum y location of window on screen. [f]
	 *    YWINMX:  Maximum y location of window on screen. [f]
	 *             Units for y locations are 0.0 (bottom) to RATIO (top),
	 *             where RATIO is the screen aspect ratio obtained
	 *             by a call to GETSCREENRATIO.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0201.
	 *=====================================================================
	 * MODULE/LEVEL:  GDM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GDM:     MGD
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GDM:     LGDON
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861201:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850506
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Check input window number for correctness. */

	/* - Turn each window on if needed. */

	if( Lgdon[1] ){
		getdevicerat1( &ratio );
		xwmn = fmax( 0.0, xwinmn );
		xwmx = fmin( 1.0, xwinmx );
		ywmn = fmax( 0.0, ywinmn );
		ywmx = fmin( ratio, ywinmx );
		createwindow1( *number, xwmn, xwmx, ywmn, ywmx, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[2] ){
		getdevicerat2( &ratio );
		xwmn = fmax( 0.0, xwinmn );
		xwmx = fmin( 1.0, xwinmx );
		ywmn = fmax( 0.0, ywinmn );
		ywmx = fmin( ratio, ywinmx );
		createwindow2( *number, xwmn, xwmx, ywmn, ywmx, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[3] ){
		getdevicerat3( &ratio );
		xwmn = fmax( 0.0, xwinmn );
		xwmx = fmin( 1.0, xwinmx );
		ywmn = fmax( 0.0, ywinmn );
		ywmx = fmin( 1.0, ywinmx );
		createwindow3( number, &xwmn, &xwmx, &ywmn, &ywmx, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[4] ){
		getdevicerat4( &ratio );
		xwmn = fmax( 0.0, xwinmn );
		xwmx = fmin( 1.0, xwinmx );
		ywmn = fmax( 0.0, ywinmn );
		ywmx = fmin( ratio, ywinmx );
		createwindow4( number, &xwmn, &xwmx, &ywmn, &ywmx, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	if( Lgdon[5] ){
		getdevicerat5( &ratio );
		xwmn = fmax( 0.0, xwinmn );
		xwmx = fmin( 1.0, xwinmx );
		ywmn = fmax( 0.0, ywinmn );
		ywmx = fmin( 1.0, ywinmx );
		createwindow5( number, &xwmn, &xwmx, &ywmn, &ywmx, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

L_8888:
	return;

} /* end of function */

