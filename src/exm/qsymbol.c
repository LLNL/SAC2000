#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qsymbol()
{



	/*=====================================================================
	 * PURPOSE: To report the current values of SYMBOL parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LSYM, ISYM, SYMSZ, SYMSP, LISYM, IISYM, NISYM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV, REPIV, REPRV, REPIVL
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870728:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	replv( "SYMBOL option$",15, cmgem.lsym );
	if( cmgem.lsym ){
		repiv( "Current SYMBOL$",16, cmgem.isym );
		reprv( "Symbol SIZE$",13, cmgem.symsz );
		reprv( "Symbol SPACEING$",17, cmgem.symsp );
		replv( "Symbol INCREMENT option$",25, cmgem.lisym );
		if( cmgem.lisym )
			repivl( "Symbol increment LIST$",23, cmgem.iisym, cmgem.nisym );
		}

L_8888:
	return;

} /* end of function */

