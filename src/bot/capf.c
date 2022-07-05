#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ capf(nerr)
int *nerr;
{

	/* Ind
	 *=====================================================================
	 * PURPOSE:  To close the alphanumeric pick file (APF) if open.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     LAPFOP, NAPFUN
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EAM:     LAPFOP
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZCLOSE
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Close APF if open. */
	if( cmeam.lapfop )
		zcloses( &cmeam.napfun, nerr );

	/* - Set flag showing APF is closed. */

	cmeam.lapfop = FALSE;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820810:  Documented subroutine.
	 *    820303:  Only call ZCLOSE if LCIPF is .TRUE.
	 *===================================================================== */

} /* end of function */

