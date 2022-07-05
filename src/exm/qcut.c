#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
void /*FUNCTION*/ qcut()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of CUT parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     LCUT, KCUT, OCUT    
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPRTW
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920325:  Bug fix by Guy Tytgat, Alaska. Change kmut to kcut.
	 *    870728:  Original version based on qcut.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	reprtw( "CUT option$",12, cmdfm.lcut, (char*)kmdfm.kcut,9, cmdfm.ocut );

L_8888:
	return;

} /* end of function */

