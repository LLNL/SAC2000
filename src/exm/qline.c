#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qline()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of LINE parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LLINE, ICLINE, LILINE, IILINE, NILINE
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV, REPIV, REPIVL
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    910301:  Changed iline to icline. iline was in gem and gam. (wct)
	 *    870728:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	replv( "LINE option$",13, cmgem.lline );
	if( cmgem.lline ){
		repiv( "Current linestyle$",19, cmgem.icline );
		replv( "Line INCREMENT option$",23, cmgem.liline );
		if( cmgem.liline )
			repivl( "Line increment LIST$",21, cmgem.iiline, cmgem.niline );
		}

L_8888:
	return;

} /* end of function */

