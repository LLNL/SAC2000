#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ inquiremsg(unitnumber, activate, send_)
FILE *unitnumber;
int *activate, send_[];
{
	int jtpmsg, jtpmsg_, junit;

	int *const Send_ = &send_[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To inquire about the current output message system status
	 *           associated with a specific file unit.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    unitnumber:   FILE pointer. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    activate:     Set to .TRUE. if sending of messages is activated. [l]
	 *                  Set to .FALSE. if not.
	 *         send:    Logical array representing the contents being sent. [ia]
	 *                  Only valid if "activate" is .TRUE.
	 *                  Each array element represents a different message type:
	 *                    1 = error messages.
	 *                    2 = warning messages.
	 *                    3 = output messages.
	 *                    4 = commands typed at the terminal.
	 *                    5 = commands executed from a macro file.
	 *                    6 = processed (evaluated) terminal or macro commands.
	 *                  A value of .TRUE. means messages of that type 
	 *                  are being sent to the requested fortran file unit.
	 *                  A value of .FALSE. means they are not.
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    msg:     nunits, iunits, lsend, MTPMSG
	 *=====================================================================
	 * GLOBAL COUPLING:  See setmsg.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890110:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890110
	 *===================================================================== */
	/* PROCEDURE: */
	junit = 1;
L_100:
	if( unitnumber == cmmsg.iunits[junit-1] ){
		*activate = TRUE;
		for( jtpmsg = 1; jtpmsg <= MTPMSG; jtpmsg++ ){
			jtpmsg_ = jtpmsg - 1;
			Send_[jtpmsg] = cmmsg.lsend[junit - 1][jtpmsg_];
			}
		}
	else if( junit < cmmsg.nunits ){
		junit = junit + 1;
		goto L_100;
		}
	else{
		*activate = FALSE;
		for( jtpmsg = 1; jtpmsg <= MTPMSG; jtpmsg++ ){
			jtpmsg_ = jtpmsg - 1;
			Send_[jtpmsg] = FALSE;
			}
		}

L_8888:
	return;

} /* end of function */

