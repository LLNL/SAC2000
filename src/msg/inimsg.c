#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ inimsg()
{
	int send_[MTPMSG];
	int nerr;
        int i;
	int *const Send_ = &send_[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To initialize MSG common block.
	 *=====================================================================
	 * MODULE/LEVEL:  msg/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:  MUNOUT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    msg:   ktpmsg, nummsg, itpmsg, nfmsg, nunits
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  sacmsg, outmsg, sendmesg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881230:  Added activation of standard output for messages.
	 *    860203:  Added options for message storage in common.
	 *    841005:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	strcpy( kmmsg.ktpmsg[MERRORS - 1], "ERRORS  " );
	strcpy( kmmsg.ktpmsg[MWARNINGS - 1], "WARNINGS" );
	strcpy( kmmsg.ktpmsg[MOUTPUT - 1], "OUTPUT  " );
	strcpy( kmmsg.ktpmsg[MCOMMANDS - 1], "COMMANDS" );
	strcpy( kmmsg.ktpmsg[MMACROS - 1], "MACROS  " );
	strcpy( kmmsg.ktpmsg[MPROCESSED - 1], "PROCESSE" );

	cmmsg.nummsg = 0;
	cmmsg.itpmsg = 0;
	cmmsg.autoout = FALSE;

	cmmsg.nfmsg = 0;
	sacmsg( &nerr );
	if( nerr != 0 )
		outmsg();

	cmmsg.nunits = 0;

        for (i=0; i<MUNITS; i++){
          cmmsg.iunits[i] = NULL;
	}

	Send_[MERRORS] = TRUE;
	Send_[MWARNINGS] = TRUE;
	Send_[MOUTPUT] = TRUE;
	Send_[MCOMMANDS] = FALSE;
	Send_[MMACROS] = FALSE;
	Send_[MPROCESSED] = FALSE;
	sendmesg( MUNOUT, TRUE, send_ );

L_8888:
	return;

} /* end of function */

