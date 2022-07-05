#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qwidth()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of line WIDTH parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LWIDTH, IWIDTH, LIWIDTH, NIWIDTH, IIWIDTH 
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920528:  Original version - copied from qcolor and modified.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */
	replv( "WIDTH option$",14, cmgem.lwidth );
	if( cmgem.lwidth ){
		replv( "WIDTH INCREMENT option$",24, cmgem.liwidth );
		repiv( "Current WIDTH value$",21, cmgem.iwidth );
		repiv( "Current SKELETON WIDTH value$",30, cmgem.iskwidth );
		if( cmgem.liwidth )
			repivl( "WIDTH increment LIST:$",23, cmgem.iiwidth, cmgem.niwidth );
		}
L_8888:
	return;

} /* end of function */

