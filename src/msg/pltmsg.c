#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ pltmsg(xloc, yloc)
float *xloc, *yloc;
{
	int j, j_;
	float ytemp;
        char *cattemp;

	/*=====================================================================
	 * PURPOSE:  To write output message to the active graphics devices.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    XLOC:    X plot location for beginning of text. [f]
	 *    YLOC:    Y plot location for beginning of text. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  MSG/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     CHHT
	 *    MSG:     ITPMSG, KLIMSG, NLIMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Add a prefix to first line of message if appropriate.
	 *   (There is an ASCII BEL embedded in the error prefix.) */
	if( cmmsg.itpmsg == 1 ){
                cattemp = malloc(8+strlen(kmmsg.klimsg[0])+1);
                strcpy(cattemp,"\aERROR: ");
                strcat(cattemp,kmmsg.klimsg[0]);
		pltext( cattemp, 8+strlen(kmmsg.klimsg[0])+1, *xloc, *yloc );
                free(cattemp);
		}
	else if( cmmsg.itpmsg == 2 ){
                cattemp = malloc(9+strlen(kmmsg.klimsg[0])+1);
                strcpy(cattemp,"WARNING: ");
                strcat(cattemp,kmmsg.klimsg[0]);
		pltext( cattemp, 9+strlen(kmmsg.klimsg[0])+1, *xloc, *yloc );
                free(cattemp);
		}
	else{
		pltext( (char*)kmmsg.klimsg[0],MCMSG+1, *xloc, *yloc );
		}

	/* - Write remaining lines of current message. */

	ytemp = *yloc;
	for( j = 2; j <= cmmsg.nlimsg; j++ ){
		j_ = j - 1;
		ytemp = ytemp - cmgem.chht;
		pltext( (char*)kmmsg.klimsg[j_],MCMSG+1, *xloc, ytemp );
		}

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860203
	 *===================================================================== */

} /* end of function */

