#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/smm.h"
void /*FUNCTION*/ qmtw()
{
	char kmwt[9];



	/*=====================================================================
	 * PURPOSE: To report the current values of MTW parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    SMM:     LMTW, KMTW, OMTW    
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPRTW
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870728:  Original version based on qcut.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	reprtw( "MTW option$",12, cmsmm.lmtw, kmwt,9, cmsmm.omtw );

L_8888:
	return;

} /* end of function */

