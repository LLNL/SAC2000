#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ getsmsg(number, kmsg, kmsg_s)
int number;
char *kmsg;   int kmsg_s;
{
	int j, j_;


	/*=====================================================================
	 * PURPOSE:  To get a message from message file on disk.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NUMBER:  Message number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KMSG:    Message from disk file. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  MSG/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    MSG:     NFMSG, IFMSG, KFMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Loop through list of message numbers, looking for a match. */
L_2000:
	for( j = 1; j <= cmmsg.nfmsg; j++ ){
		j_ = j - 1;
		if( number == Ifmsg[j] ){
			fstrncpy( kmsg, kmsg_s-1, kmmsg.kfmsg[j_], strlen(kmmsg.kfmsg[j_]));
			goto L_8888;
			}
		}

	/* - If no match is found, simply encode the error number. */

        sprintf(kmsg,"%s%5d", "Number", number );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860203:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860203
	 *===================================================================== */

} /* end of function */

