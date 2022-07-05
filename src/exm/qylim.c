#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ qylim()
{
	char kline[MCMSG+1];
	int lall;
	int j, j_;


	/*=====================================================================
	 * PURPOSE: To report the current values of YLIM parameters.
	 *=====================================================================
	 * MODULE/LEVEL:  exm/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *    gam:     nylim, kylims, ylims, rngmin, rngmax
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    830121:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	lall = FALSE;

        sprintf(kline,"   %s", "YLIM option(s) are:" );
	aplmsg( kline,MCMSG+1 );
	for( j = 1; j <= cmgam.nylim; j++ ){
		j_ = j - 1;
		if( strcmp(kmgam.kylims[j_],"ON      ") == 0 ){
                        sprintf(kline,"     %.4s%12.5g%12.5g", kmgam.kylims[j_]
			 , cmgam.ylims[j_][0], cmgam.ylims[j_][1] );
			aplmsg( kline,MCMSG+1 );
			}
		else if( strcmp(kmgam.kylims[j_],"ALL     ") == 0 ){
                        sprintf(kline,"     %.4s", kmgam.kylims[j_] );
			aplmsg( kline,MCMSG+1 );
			lall = TRUE;
			}
		else{
                        sprintf(kline,"     %.4s", kmgam.kylims[j_] );
			aplmsg( kline,MCMSG+1 );
			}
		}

	if( lall ){
                sprintf(kline,"   %s%12.5g%12.5g", "Range of dependent variable is:"
		 , cmgam.rngmin, cmgam.rngmax );
		aplmsg( kline,MCMSG+1 );
		}

L_8888:
	return;

} /* end of function */

