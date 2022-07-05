#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
#include "../../inc/gdm.h"

void /*FUNCTION*/ wrtmsg(nunit)
FILE *nunit;
{
	int j, j_, nc;
        char *s1, *message;

	/*=====================================================================
	 * PURPOSE:  To write output message to a specific file unit.
	 *=====================================================================
	 *    NUNIT:   Fortran file unit number. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  MSG/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    NUNIT
	 *    MSG:     ITPMSG, KLIMSG, NLIMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870811:  Added logic to determine the end of each line.
	 *    860203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860203
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Add a prefix to first line of message if appropriate.
	 *   (There is an ASCII BEL embedded in the error prefix.) */
	nc = indexb( (char*)kmmsg.klimsg[0],MCMSG+1 );

        strncpy((s1=malloc(nc+1)),kmmsg.klimsg[0],nc);
        s1[nc] = '\0';
        message = (char *)malloc(nc+100+1);

	if( cmmsg.itpmsg == MERRORS ){
                sprintf(message,"%s%s", " ERROR: ", s1 );
                  fprintf(nunit,"\a%s\n", message );
		}
	else if( cmmsg.itpmsg == MWARNINGS ){
                sprintf(message,"%s%s", "WARNING: ", s1 );
                  fprintf(nunit,"%s\n", message );
		}
	else{
                  fprintf(nunit,"%s\n",s1);
		}

        free(s1);
        free(message);

	/* - Write remaining lines of current message. */

	for( j = 2; j <= cmmsg.nlimsg; j++ ){
		j_ = j - 1;
		nc = indexb( (char*)kmmsg.klimsg[j_],MCMSG+1 );
                strncpy((s1=malloc(nc+1)),kmmsg.klimsg[j_],nc);
                s1[nc] = '\0';

                  fprintf(nunit,"%s\n",s1);

                free(s1);
		}

L_8888:
	return;

} /* end of function */

