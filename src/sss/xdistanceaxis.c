#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ xdistanceaxis(nerr)
int *nerr;
{
	int ldummy;



	/*=====================================================================
	 * PURPOSE:  To execute the DISTANCEAXIS command.
	 *           This command controls the distance axis properties
	 *           of the record section plot (PLOTRS).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1001.
	 *=====================================================================
	 * MODULE/LEVEL:  SSS/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    SSS:     IDAOP, DALEN, DASCA
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LCREAL
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "FIXED [v]":  change distance axis size option to fixed.
		 *    ("LENGTH" is an obsolete but allowed form of this option.) */
		if( lckey( "FIXED$",7 ) || lckey( "LENGTH$",8 ) ){
			cmsss.idaop = 1;
			ldummy = lcreal( &cmsss.dalen );

			/* -- "SCALED [v]":  change distance axis size option to scaled.
			 *    ("PERCM" is an obsolete but allowed form of this option.) */
			}
		else if( lckey( "SCALED$",8 ) || lckey( "PERCM$",7 ) ){
			cmsss.idaop = 2;
			ldummy = lcreal( &cmsss.dasca );

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

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860304:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860304
	 *===================================================================== */

} /* end of function */

