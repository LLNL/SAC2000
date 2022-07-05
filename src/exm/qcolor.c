#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qcolor()
{
	char kcol[9];



	/*=====================================================================
	 * PURPOSE: To report the current values of COLOR parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LCOL, ICOL, LICOL, ISKCOL, IBACOL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV, CONVCOLORNUM, REPAV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820316:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	replv( "COLOR option$",14, cmgem.lcol );
	convcolornum( cmgem.icol, kcol,9 );
	repav( "DATA color$",12, kcol,9 );
	replv( "INCREMENT data color$",22, cmgem.licol );
	convcolornum( cmgem.iskcol, kcol,9 );
	repav( "SKELETON color$",16, kcol,9 );
	convcolornum( cmgem.ibacol, kcol,9 );
	repav( "BACKGROUND color$",18, kcol,9 );

L_8888:
	return;

} /* end of function */

