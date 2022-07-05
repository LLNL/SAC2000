#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ sendmesg(unitnumber, activate, send_)
FILE *unitnumber;
int activate, send_[];
{
	int j, j_, jtpmsg, jtpmsg_, junit, junit_, nunitsav;

	int *const Send_ = &send_[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To activate/deactivate the sending of output message 
	 *           to a specific file unit.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    unitnumber:   Fortran file unit number. [i]
	 *    activate:     Set to .TRUE. to activate the sending of messages. [l]
	 *                  Set to .FALSE. to deactivate sending of messages.
	 *         send:    Logical array representing the contents to be sent. [ia]
	 *                  Each array element represents a different message type:
	 *                    1 = error messages.
	 *                    2 = warning messages.
	 *                    3 = output messages.
	 *                    4 = commands typed at the terminal.
	 *                    5 = commands executed from a macro file.
	 *                  A value of .TRUE. activates the sending of that type 
	 *                  of message to the requested fortran file unit.
	 *                  A value of .FALSE. deactivates that type.
	 *=====================================================================
	 * MODULE/LEVEL:  msg/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    msg:     MUNITS, MTPMSG
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:     nunits, iunits, lsend
	 *=====================================================================
	 * GLOBAL COUPLING:  See setmsg.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881230:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	if( activate ){
		junit = 1;
L_100:
		if( unitnumber == cmmsg.iunits[junit-1] ){
			for( jtpmsg = 1; jtpmsg <= MTPMSG; jtpmsg++ ){
				jtpmsg_ = jtpmsg - 1;
				cmmsg.lsend[junit - 1][jtpmsg_] = Send_[jtpmsg];
				}
			}
		else if( junit < cmmsg.nunits ){
			junit = junit + 1;
			goto L_100;
			}
		else if( cmmsg.nunits < MUNITS ){
			cmmsg.nunits = cmmsg.nunits + 1;
			cmmsg.iunits[cmmsg.nunits-1] = unitnumber;
			for( jtpmsg = 1; jtpmsg <= MTPMSG; jtpmsg++ ){
				jtpmsg_ = jtpmsg - 1;
				cmmsg.lsend[cmmsg.nunits - 1][jtpmsg_] = Send_[jtpmsg];
				}
			}
		}
	else{
		nunitsav = cmmsg.nunits;
		for( junit = 1; junit <= cmmsg.nunits; junit++ ){
			junit_ = junit - 1;
			if( unitnumber == cmmsg.iunits[junit-1] ){
				nunitsav = cmmsg.nunits - 1;
				for( j = junit; j <= nunitsav; j++ ){
					j_ = j - 1;
					cmmsg.iunits[j_] = cmmsg.iunits[j_ + 1];
					for( jtpmsg = 1; jtpmsg <= MTPMSG; jtpmsg++ ){
						jtpmsg_ = jtpmsg - 1;
						cmmsg.lsend[j_][jtpmsg_] = cmmsg.lsend[j_ + 1][jtpmsg_];
						}
					}
				goto L_1001;
				}
			}

L_1001:
		cmmsg.nunits = nunitsav;

		}

L_8888:
	return;

} /* end of function */

