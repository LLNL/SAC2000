#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/site.h"
void /*FUNCTION*/ xsitecom(index, nerr)
int index, *nerr;
{



	/*=====================================================================
	 * PURPOSE: To execute commands in the Site Command Module.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    index:   Index number of the command to execute. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  site/2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900804:  Changed from idm to site.
	 *    831107:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900804
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Jump to correct command based upon its index number. */
	switch( index ){
		case 1: goto L_100;
		}

	/* - Error return if bad index value. */

	*nerr = 901;
	setmsg( "ERROR", *nerr );
	apcmsg( "in XSITECOM",12 );
	goto L_8888;

	/* - Command 01: TESTSITE --- Test the site command module logic. */

L_100:
	xtestsite( nerr );
	goto L_8888;

L_8888:
	return;

} /* end of function */

