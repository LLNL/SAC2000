#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ reprv(ktext, ktext_s, rv)
char *ktext;   int ktext_s;
double rv;
{
	char kline[MCMSG+1];
	int nctext;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE: To report the value of a real variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    text:    Text to accompany value of variable. [c]
	 *    rv:      Value of real variable to report. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexc, aplmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890104:  Changed from direct terminal output to message subsystem.
	 *    820315:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of text. */
	nctext = indexc( ktext,ktext_s, '$' );

	/* - Write text and value of variable to message subsystem. */

        strtemp = malloc(nctext+1);
        strncpy(strtemp,ktext,nctext);
        strtemp[nctext] = '\0';

        sprintf(kline,"   %s%s%12.5g", strtemp, " is ", rv );
	aplmsg( kline,MCMSG+1 );

        free(strtemp);

L_8888:
	return;

} /* end of function */

