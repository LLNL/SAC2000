#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "msg.h"
void /*FUNCTION*/ apcmsg(char *kalpha, int kalpha_s)
{
	int isave, nalpha, nchmax;
        char *s1;

	/*=====================================================================
	 * PURPOSE:  To append alphanumeric string to current message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kalpha:  Alphanumeric string. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *    msg:     MLIMSG, nlimsg, nchmsg, autoout
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:     nlimsg, nchmsg, klimsg
	 *=====================================================================
	 * GLOBAL COUPLING:  See SETMSG.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  indexb, outmsg, clrmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900518:  Added logic to test for an empty string. 
	 *             This corrects a problem with VAX VMS version.
	 *    890104:  Added automatic output mode coding.
	 *    830916:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890104
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of string without trailing blanks. */
	nalpha = indexb( kalpha,kalpha_s );

	/* - Decrease maximum length of first line of message to
	 *   allow room for a prefix to be added later when written. */

	nchmax = MCMSG;
	if( cmmsg.nlimsg == 1 )
	     nchmax = nchmax - 10;

	/* - Start new line of message if there is not enough room. */

	if( (cmmsg.nchmsg + nalpha + 1) > nchmax ){
	     if( cmmsg.nlimsg < MLIMSG ){
		cmmsg.nlimsg = cmmsg.nlimsg + 1;
	     }
	     else if( cmmsg.autoout ){
		outmsg();
		isave = cmmsg.itpmsg;
		clrmsg();
		cmmsg.itpmsg = isave;
	     }
	     else{
		fstrncpy( kmmsg.klimsg[cmmsg.nlimsg - 1], MCMSG, " ", 1 );
	     }
	     cmmsg.nchmsg = 0;
	}

	/* - Append alphanumeric string to current message line.
	 *   Include one trailing blank.  Update character counter. */


	if( nalpha > 0 ){
            strncpy((s1=malloc(nalpha+1)),kalpha,nalpha);
            s1[nalpha] = '\0';

	    subscpy( kmmsg.klimsg[cmmsg.nlimsg - 1], cmmsg.nchmsg,
		     cmmsg.nchmsg + nalpha + 1, MCMSG, s1);

            free(s1);
	}
	cmmsg.nchmsg = cmmsg.nchmsg + nalpha + 1;

L_8888:
	return;

} /* end of function */

