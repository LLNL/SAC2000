#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bom.h"
void /*FUNCTION*/ xbomc(index, nerr)
int index, *nerr;
{



	/*=====================================================================
	 * PURPOSE: To execute a MOD command given its index number.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    INDEX:   The index number of the command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0901.
	 *=====================================================================
	 * MODULE/LEVEL: BOM/1
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM, XMERGE, XADDF, XSUBF, XMULF, XDIVF, XBOEC
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Jump to correct command based upon its index number. */

	switch( index ){
		case 1: goto L_100;
		case 2: goto L_200;
		case 3: goto L_300;
		case 4: goto L_400;
		case 5: goto L_500;
		case 6: goto L_600;
		}

	/* - Error return if bad index value. */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in XBOMC",9 );
	goto L_8888;

	/* - Command 01: MERGE */

L_100:
	xmerge( nerr );
	goto L_8888;

	/* - Command 02: ADDF */

L_200:
	xaddf( nerr );
	goto L_8888;

	/* - Command 03: SUBF */

L_300:
	xsubf( nerr );
	goto L_8888;

	/* - Command 04: MULF */

L_400:
	xmulf( nerr );
	goto L_8888;

	/* - Command 05: DIVF */

L_500:
	xdivf( nerr );
	goto L_8888;

	/* - Command 06: BINOPERR */

L_600:
	xboec( nerr );
	goto L_8888;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820824:  Original version.
	 *===================================================================== */

} /* end of function */

