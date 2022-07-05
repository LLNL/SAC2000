#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ xwindow(nerr)
int *nerr;
{
	int iwin;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command WINDOW.
	 *          This command sets the graphics window attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  GDM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GDM:     MWINDOWS
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GDM:     XWINDOWMIN, XWINDOWMAX, YWINDOWMIN, YWINDOWMAX
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCIRC, LKRRCP
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861230:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861230
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	iwin = 1;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Set up window number. */
		if( lcirc( 1, MWINDOWS, &iwin ) ){

			/* -- Set up window size. */
			}
		else if( lkrrcp( "XSIZE$",7, 0., 1., &Xwindowmin[iwin], &Xwindowmax[iwin] ) ){

			}
		else if( lkrrcp( "YSIZE$",7, 0., 1., &Ywindowmin[iwin], &Ywindowmax[iwin] ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

