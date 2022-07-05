#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ repkv(ktext, ktext_s, kv, kv_s)
char *ktext;   int ktext_s;
char *kv;   int kv_s;
{
	char kline[MCMSG+1];
	int nct, ncv;
        char *strtemp1, *strtemp2;


	/*=====================================================================
	 * PURPOSE: To report the value of a alphanumeric variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktext:   Text to accompany value of variable. [c]
	 *    kv:      Value of alphanumeric variable to report. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexc, indexb, aplmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Changed from direct terminal output to message subsystem.
	 *    820505:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of text and string. */
	nct = indexc( ktext,ktext_s, '$' );
	ncv = indexb( kv,kv_s );

	/* - Write text and value of variable to message subsystem. */

        strtemp1 = malloc(nct+1);
        strtemp2 = malloc(ncv+1);
        strncpy(strtemp1,ktext,nct);
        strncpy(strtemp2,kv,ncv);
        strtemp1[nct] = '\0';
        strtemp2[ncv] = '\0';

        sprintf(kline,"   %s%s%c%s%c", strtemp1
	 , " is ", '\'', strtemp2, '\'' );
	aplmsg( kline,MCMSG+1 );

        free(strtemp1);
        free(strtemp2);
L_8888:
	return;

} /* end of function */

