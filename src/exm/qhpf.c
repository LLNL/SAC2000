#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/eam.h"
void /*FUNCTION*/ qhpf()
{



	/*=====================================================================
	 * PURPOSE: To report the current HYPO pick file parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     KHPFNM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPAV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870728:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	repav( "HYPO pick file$",16, kmeam.khpfnm,MCPFN+1 );

L_8888:
	return;

} /* end of function */

