#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ setlinestyle(istyle)
int istyle;
{
	void setlinestyle3(), setlinestyle4(), setlinestyle5();



	/*=====================================================================
	 * PURPOSE:  To change the linestyle attribute.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    istyle:  Linestyle attribute. [i]
	 *             = 1 for a solid line.
	 *             > 1 for device-specific linestyles.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     lgdon
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:     iline
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  setlinestyle1, setlinestyle2, setlinestyle3, setlinestyle4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    841108:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If linestyle is different than current one: */
	if( istyle > 0 && istyle != cmgdm.iline ){

		/* -- Save new linestyle. */
		cmgdm.iline = istyle;

		/* -- Send new linestyle request to all active graphics devices. */
		if( Lgdon[1] )
			setlinestyle1( cmgdm.iline );
		if( Lgdon[2] )
			setlinestyle2( cmgdm.iline );
		if( Lgdon[3] )
			setlinestyle3( &cmgdm.iline );
		if( Lgdon[4] )
			setlinestyle4( &cmgdm.iline );
		if( Lgdon[5] )
			setlinestyle5( &cmgdm.iline );

		}

L_8888:
	return;

} /* end of function */

