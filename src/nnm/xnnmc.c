#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xnnmc(index, nerr)
int index, *nerr;
{

	/*=====================================================================
	 * PURPOSE: To execute a neural net module command given its index number.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    index:   The index number of the command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 0910.
	 *=====================================================================
	 * MODULE/LEVEL: nnm/1
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  gtoutm, 
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890306:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890306
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Jump to correct command based upon its index number. */

	switch( index ){
		case 1: goto L_100;
		}

	/* - Error return if bad index value. */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in XNNMC",9 );
	goto L_8888;

	/* - Command 01: WRITENN --- write a neural net file to disk. */

L_100:
	xwritenn( nerr );
	goto L_8888;

L_8888:
	return;

} /* end of function */

