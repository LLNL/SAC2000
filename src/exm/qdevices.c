#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ qdevices()
{
	char kline[MCMSG+1], name[13];
	int j, j_, number;

	/*=====================================================================
	 * PURPOSE:  To report on the available graphics devices.
	 *=====================================================================
	 * MODULE/LEVEL:  exm/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT, MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getmaxdevices, getdevicename
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870514:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870514
	 *===================================================================== */
	/* PROCEDURE: */
        sprintf(kline,"   %s", "Available graphics devices are:");
	aplmsg( kline,MCMSG+1 );

	getmaxdevices( &number );
	for( j = 1; j <= number; j++ ){
		j_ = j - 1;
		getdevicename( j, name,13 );
		if( strcmp(name,"            ") != 0 ){
                        sprintf(kline,"%s",name);
			aplmsg( kline,MCMSG+1 );
			}
		}

L_8888:
	return;

} /* end of function */

