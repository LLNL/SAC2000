#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ qxlim()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of XLIM parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GAM:     LRTWXL, KRTWXL, ORTWXL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPRTW
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    830121:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	reprtw( "XLIM option$",13, cmgam.lrtwxl, (char*)kmgam.krtwxl,9, 
	 cmgam.ortwxl );

L_8888:
	return;

} /* end of function */

