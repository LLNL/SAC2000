#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ replv(ktext, ktext_s, lv)
char *ktext;   int ktext_s;
int lv;
{
	char kline[MCMSG+1], kvalue[9];
	int nctext;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE: To report the value of a logical variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktext:   Text to accompany value of variable. [c]
	 *    lv:      Value of logical variable to report. [l]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPW, MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexc, aplmsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kvalue:  Character variable used to encode value of LV.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of text. */
	nctext = indexc( ktext,ktext_s, '$' );

	/* - Encode value of variable. */

	if( lv ){
		strcpy( kvalue, "ON      " );
		}
	else{
		strcpy( kvalue, "OFF     " );
		}

	/* - Write text and value of variable to message subsystem. */

        strtemp = malloc(nctext+1);
        strncpy(strtemp,ktext,nctext);
        strtemp[nctext] = '\0';

        sprintf(kline,"   %s%s%s", strtemp, " is ", kvalue );
	aplmsg( kline,MCMSG+1 );

        free(strtemp);

L_8888:
	return;

} /* end of function */

