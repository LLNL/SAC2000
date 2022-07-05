#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ qfid()
{



	/*=====================================================================
	 * PURPOSE:  To report about the current values of the FILEID parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GAM:   LFIDRQ, KFIDTP, IFIDTP, KFIDLC, IFIDLC
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV, REPAV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820921:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	replv( "FILEID display$",16, cmgam.lfidrq );
	repav( "Fileid display TYPE$",21, (char*)kmgam.kfidtp[cmgam.ifidtp - 1]
	 ,9 );
	repav( "Fileid display LOCATION$",25, (char*)kmgam.kfidlc[cmgam.ifidlc - 1]
	 ,9 );

L_8888:
	return;

} /* end of function */

