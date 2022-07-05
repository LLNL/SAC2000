#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"

void getwindowstat3(int *win_num, int *exists);
void getwindowstat4(int *win_num, int *exists);
void getwindowstat5(int *win_num, int *exists);



void /*FUNCTION*/ getwindowstatus(nwindow, exists)
int *nwindow;
int *exists;
{



	/*=====================================================================
	 * PURPOSE: To get attributes of a graphics window.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    nwindow: Graphics window number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    exists:  Set to .TRUE. if window currently exists. [l]
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getwindowstat1, getwindowstat2,
	 *             getwindowstat3, getwindowstat4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870217:  Changed name form getwindowatr.
	 *    870127:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870127
	 *===================================================================== */
	/* PROCEDURE: */
	*exists = FALSE;

	/* - Inquire about the existence of the requested graphics window. */

	if( Lgdon[1] )
		getwindowstat1( *nwindow, exists );

	if( Lgdon[2] )
		getwindowstat2( *nwindow, exists );

	if( Lgdon[3] )
		getwindowstat3( nwindow, exists );

	if( Lgdon[4] )
		getwindowstat4( nwindow, exists );

	if( Lgdon[5] )
		getwindowstat5( nwindow, exists );

L_8888:
	return;

} /* end of function */

