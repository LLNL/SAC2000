#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ repav(ktext, ktext_s, av, av_s)
char *ktext;   int ktext_s;
char *av;   int av_s;
{
	char kline[MCMSG+1];
	int nct, ncv;
        char *strtemp1, *strtemp2;

	/*=====================================================================
	 * PURPOSE: To report the value of a alphanumeric variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    text:    Text to accompany value of variable. [c]
	 *    av:      Value of alphanumeric variable to report. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Changed from direct terminal output to message subsystem.
	 *    820315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of text and value. */
	nct = indexc( ktext,ktext_s, '$' );
	ncv = indexb( av,av_s );

	/* - Encode text and value of variable and send to message subsystem. */

        strtemp1 = malloc(nct+1);
        strtemp2 = malloc(ncv+1);
        strncpy(strtemp1,ktext,nct);
        strtemp1[nct] = '\0';
        strncpy(strtemp2,av,ncv);
        strtemp2[ncv] = '\0';

        sprintf(kline,"   %s%s%s", strtemp1, " is ", strtemp2 );
	aplmsg( kline,MCMSG+1 );

        free(strtemp1); 
        free(strtemp2);
L_8888:
	return;

} /* end of function */

