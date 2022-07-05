#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ xtimeaxis(nerr)
int *nerr;
{
	int ldummy;



	/*=====================================================================
	 * PURPOSE:  To execute the TIMEAXIS command.
	 *           This command controls the time axis properties
	 *           of the record section plot (PLOTRS).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1001.
	 *=====================================================================
	 * MODULE/LEVEL:  sss/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    sss:     itaop, talen, tasca
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lckey, lcreal
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860304:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860304
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "FIXED [v]":  change time axis size option to fixed. */
		if( lckey( "FIXED$",7 ) ){
			cmsss.itaop = 1;
			ldummy = lcreal( &cmsss.talen );

			/* -- "SCALED [v]":  change time axis size option to scaled. */
			}
		else if( lckey( "SCALED$",8 ) ){
			cmsss.itaop = 2;
			ldummy = lcreal( &cmsss.tasca );

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

