#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"

void xgmtmap(int* nerr);


void /*FUNCTION*/ xfksc(index, nerr)
int index, *nerr;
{



	/*=====================================================================
	 * PURPOSE: To execute a fks command given its index number.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    INDEX:   The index number of the command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0901.
	 *=====================================================================
	 * MODULE/LEVEL: fks/1
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    (none)
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    (none)
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  MAP, BBFK
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    901201:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Jump to correct command based upon its index number. */

	switch( index ){
		case 1: goto L_100;
		case 2: goto L_200;
		case 3: goto L_300;
                case 4: goto L_400;
		}

	/* - Error return if bad index value. */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in XGAMC",9 );
	goto L_8888;

	/* - Command 01: MAP */

L_100:
	xmap( nerr );
	goto L_8888;

	/* - Command 02: BBFK */

L_200:
	;
	xbbfk( nerr );
	goto L_8888;

	/* - Command 03: BEAM */

L_300:
	;
	xbeam( nerr );
	goto L_8888;

        /* - Command 04: GMAP */

L_400:
        ;
        xgmtmap( nerr );
        goto L_8888;

L_8888:
	return;

} /* end of function */

