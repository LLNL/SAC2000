#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ xbreak(nerr)
int *nerr;
{



	/*=====================================================================
	 * PURPOSE: To parse the action command BREAK.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  CND/2
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    ndolevel: Decremented by one.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	if( cnd.ndolevel > 0 ){
		skipdo( nerr );

		/* - Raise error condition if not in an do condition. */

		}
	else{
		*nerr = 1;

		}

L_8888:
	return;

} /* end of function */

