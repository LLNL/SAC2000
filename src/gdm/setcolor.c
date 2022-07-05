#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ setcolor(number)
int number;
{
	int ncolor;
	void setcolor3(), setcolor4(), setcolor5();



	/*=====================================================================
	 * PURPOSE: To set the current color attribute.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *   number:   The number of the desired color. [i]
	 *             = 1 for "normal" device color.
	 *             > 1 for a device-specific color.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     nctsize, lgdon
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     icolor
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   setcolor1, setcolor2, setcolor3, setcolor4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900911:  Bug fix: setcolorN was only being called if a new color
	 *             was being set. Doesn't allow for changing devices. (bkh)
	 *    831026:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Range check the requested color number. */
	ncolor = min( cmgdm.nctsize, max( 0, number ) );
	/*      if(ncolor.eq.0)ncolor=nctsize */

	/* - Save the current color without regard to the device being used. */
	cmgdm.icolor = ncolor;

	/* -- Set color for all active graphics devices. */
	if( Lgdon[1] )
		setcolor1( ncolor );
	if( Lgdon[2] )
		setcolor2( ncolor );
	if( Lgdon[3] )
		setcolor3( &ncolor );
	if( Lgdon[4] )
		setcolor4( &ncolor );
	if( Lgdon[5] )
		setcolor5( &ncolor );

L_8888:
	return;

} /* end of function */

