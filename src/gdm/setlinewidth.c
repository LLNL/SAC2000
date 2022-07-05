#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ setlinewidth(nwidth)
int nwidth;
{
	void setwidth3(), setwidth4(), setwidth5();



	/*=====================================================================
	 * PURPOSE: To set the current linewidth attribute.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *   number:   The number of the desired linewidth. [i]
	 *             = 1 for "normal" device linewidth.
	 *             > 1 for a device-specific linewidth.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   setwidth1, setwidth2, setwidth3, setwidth4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920527:  Original version
	 *===================================================================== */
	/* PROCEDURE: */
	/* - The reason for this next test is.... that the sgftops conversion
	 *   utility checks for a width option, passwd by the prn script,
	 *   which makes all the lines a specified width, and so the 
	 *   WIDTH command will override that width with other setlinewidth
	 *   commands throughout the Postscript file. */
	if( !cmgem.lwidth )
		goto L_8888;

	/* -- Set line width for all active graphics devices. */
	if( Lgdon[1] )
		setwidth1( nwidth );
	if( Lgdon[2] )
		setwidth2( nwidth );
	if( Lgdon[3] )
		setwidth3( nwidth );
	if( Lgdon[4] )
		setwidth4( nwidth );
	if( Lgdon[5] )
		setwidth5( nwidth );

L_8888:
	return;

} /* end of function */

