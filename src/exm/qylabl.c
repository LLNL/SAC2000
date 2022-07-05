#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qylabl()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of the YLABEL command parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LYLAB, KYLAB, TSYLAB, KSIDES, IYLABP
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV, REPKV, REPRV, REPAV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820316:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	replv( "YLABEL option$",15, cmgem.lylab );
	repkv( "Text of ylabel$",16, kmgem.kylab,145 );
	reprv( "SIZE of ylabel$",16, cmgem.tsylab );
	repav( "LOCATION of ylabel$",20, (char*)kmgem.ksides[cmgem.iylabp - 1]
	 ,9 );

L_8888:
	return;

} /* end of function */

