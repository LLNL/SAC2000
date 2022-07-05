#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qxlabl()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of the XLABEL command parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LXLAB, KXLAB, TSXLAB, KSIDES, IXLABP
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
	replv( "XLABEL option$",15, cmgem.lxlab );
	repkv( "Text of xlabel$",16, kmgem.kxlab,145 );
	reprv( "SIZE of xlabel$",16, cmgem.tsxlab );
	repav( "LOCATION of xlabel$",20, (char*)kmgem.ksides[cmgem.ixlabp - 1]
	 ,9 );

L_8888:
	return;

} /* end of function */

