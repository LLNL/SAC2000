#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qtitle()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of the TITLE command parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LTITL, KTITL, TSTITL, KSIDES, ITITLP
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
	replv( "TITLE option$",14, cmgem.ltitl );
	repkv( "Text of title$",15, kmgem.ktitl,145 );
	reprv( "SIZE of title$",15, cmgem.tstitl );
	repav( "LOCATION of title$",19, (char*)kmgem.ksides[cmgem.ititlp - 1]
	 ,9 );

L_8888:
	return;

} /* end of function */

