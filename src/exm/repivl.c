#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ repivl(ktext, ktext_s, iv, nv)
char *ktext;   int ktext_s;
int iv[], nv;
{
	char kline[MCMSG+1];
	int j, j1, j2, jlines, jlines_, nctext, nlines;
        char *strtemp;


	int *const Iv = &iv[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To report the value of a integer array.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    text:    Text to accompany value of variable. [c]
	 *    iv:      Values of integer array to report. [ia]
	 *    nv:      Length of IV. [i]
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
	 *    870728:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of text. */
	nctext = indexc( ktext,ktext_s, '$' );

	/* - Write text and values of array to message subsystem.. */

        strtemp = malloc(nctext+1);
        strncpy(strtemp,ktext,nctext);
        strtemp[nctext] = '\0';

        sprintf(kline,"   %s%s",strtemp, " is:");

        free(strtemp);

	aplmsg( kline,MCMSG+1 );
	nlines = (nv - 1)/5 + 1;
	j1 = 1;
	for( jlines = 1; jlines <= nlines; jlines++ ){
		jlines_ = jlines - 1;
		j2 = min( j1 + 4, nv );
                sprintf(kline,"%s","           ");
		for( j = j1; j <= j2; j++ ){
                        sprintf(kline+11+((j-1)*5),"%5d",Iv[j] );
			}
		aplmsg( kline,MCMSG+1 );
		j1 = j1 + 5;
		}

L_8888:
	return;

} /* end of function */

