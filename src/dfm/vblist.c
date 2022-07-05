#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bom.h"
void /*FUNCTION*/ vblist(nerr)
int *nerr;
{



	/*=====================================================================
	 * PURPOSE: To verify that there is at least one file in binop file list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *             Set to 1803 if there are no files in DFL.
	 *=====================================================================
	 * MODULE/LEVEL:  BOM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    BOM:     NBFL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	if( cmbom.nbfl <= 0 ){
		*nerr = 1803;
		setmsg( "ERROR", *nerr );
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820721:  Documented subroutine.
	 *    820721:  Original version.
	 *===================================================================== */

} /* end of function */

