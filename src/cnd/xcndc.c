#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ xcndc(index, nerr)
int index, *nerr;
{



	/*=====================================================================
	 * PURPOSE: To execute a CND command given its index number.
	 *          This module contains conditional execution commands.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    INDEX:   The index number of the command to execute.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0901.
	 *=====================================================================
	 * MODULE/LEVEL: CND/1
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870901:  Original version developed by Mandy Goldner.
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
		case 7: goto L_700;
		case 8: goto L_800;
		}

	/* - Error return if bad index value. */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in XCNDC",9 );
	goto L_8888;

	/* - Command 01: IF */

L_100:
	xif( nerr );
	goto L_8888;

	/* - Command 02: ELSEIF */

L_200:
	xelseif( nerr );
	goto L_8888;

	/* - Command 03: ELSE */

L_300:
	xelse( nerr );
	goto L_8888;

	/* - Command 04: ENDIF */

L_400:
	xendif( nerr );
	goto L_8888;

	/* - Command 05: DO */

L_500:
	xdo( nerr );
	goto L_8888;

	/* - Command 06: WHILE */

L_600:
	xwhile( nerr );
	goto L_8888;

	/* - Command 07: ENDDO */

L_700:
	xenddo( nerr );
	goto L_8888;

	/* - Command 08: BREAK */

L_800:
	xbreak( nerr );
	goto L_8888;

L_8888:
	return;

} /* end of function */

