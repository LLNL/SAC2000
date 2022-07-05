#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ qgtext()
{
	char kline[MCPFN+1];
	int igtqua;


	/*=====================================================================
	 * PURPOSE: To report the current values of GTEXT parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MUNOUT, KFTERM
	 *    GEM:     IGTQUA, IGTFNT, TSDEF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPIV, REPRV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870728:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	if( igtqua == 1 || igtqua == 2 ){
                sprintf(kline,"   %s", "HARDWARE text being used.");
		aplmsg( kline,MCPFN+1 );
		}
	else{
                sprintf(kline,"   %s", "SOFTWARE text being used.");
		aplmsg( kline,MCPFN+1 );
		}
	repiv( "Text FONT$",11, cmgem.igtfnt );
	reprv( "Text SIZE$",11, cmgem.tsdef );

L_8888:
	return;

} /* end of function */






