#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ pcmsg(kmsg, kmsg_s, nerr)
char *kmsg;   int kmsg_s;
int *nerr;
{
	int ncmsg;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE:  To put a message into the command stack.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kmsg:    Message. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag.  Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/3
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:    indexb, csinit
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    891006:  Deleted obsolete argument ("itpmsg".)
	 *    820423:  Adjustments due to new command parsing system.
	 *    810429:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  891006
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine character length of command message. */

	ncmsg = indexb( kmsg,kmsg_s );

	/* - Initialize command stack with message. */

        strtemp = malloc(ncmsg+1);
        strncpy(strtemp,kmsg,ncmsg);
        strtemp[ncmsg] = '\0';

	csinit( strtemp, ncmsg, nerr );

        free(strtemp);
L_8888:
	return;

} /* end of function */

