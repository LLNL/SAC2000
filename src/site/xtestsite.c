#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/site.h"
void /*FUNCTION*/ xtestsite(nerr)
int *nerr;
{



	/*=====================================================================
	 * PURPOSE: To execute the "testsite" command.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  site/3
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900804:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900804
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Acknowledge that this test routine has been executed. */

	setmsg( "OUTPUT", 99 );
	apcmsg( "Executing Site Command TESTSITE.",33 );
	outmsg();
	clrmsg();

L_8888:
	return;

} /* end of function */

