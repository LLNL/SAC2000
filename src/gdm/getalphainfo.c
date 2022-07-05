#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getalphainfo(nlines, erase, erase_s)
int *nlines;
char *erase;   int erase_s;
{
	void getalphainfo3(), getalphainfo4(), getalphainfo5();



	/*=====================================================================
	 * PURPOSE: To get information about the alphanumeric characteristics
	 *          of the active graphics device.
	 *=====================================================================
	 * MODULE/LEVEL: gdm/4
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nlines:  Number of text lines per screen. [i]
	 *    erase:   Text to send to erase terminal screen. [c]
	 *             Set to all blanks is terminal has scrolling capability.
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     igdtxt
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getalphainfo1, getalphainfo2,
	 *             getalphainfo3, getalphainfo4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870217:  Changed name from getscreenatr.
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Inquire about text values from appropriate graphics devies. */
/*	if( cmgdm.igdtxt == 1 ){
		getalphainfo1( nlines, erase,erase_s );
		}
else */
	if( cmgdm.igdtxt == 2 ){
		getalphainfo2( *nlines, erase );
		}
	else if( cmgdm.igdtxt == 3 ){
		getalphainfo3( nlines, erase,erase_s );
		}
	else if( cmgdm.igdtxt == 4 ){
		getalphainfo4( nlines, erase,erase_s );
		}
	else if( cmgdm.igdtxt == 5 ){
		getalphainfo5( nlines, erase,erase_s );
		}
	else{
		*nlines = 23;
		fstrncpy( erase, erase_s-1, " ", 1 );
		}

L_8888:
	return;

} /* end of function */

