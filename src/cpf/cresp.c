#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ cresp()
{
	char kbufr[MCMSG+1], kiomsg[9];
	int ic1 = 0, ic2, ic3, icpntr, itype, j, j_, nc, ncmsg, nerr, 
	 nxerr;
	void zgtmsg();
	static char kprmpt[MCMSG+1] = "Please enter correction (or type HELP)  $                                                                                                                       ";
	static char kchang[9] = "CHANGE  ";
	static char kinser[9] = "INSERT  ";
	static char krepla[9] = "REPLACE ";
	static char kerror[9] = "ERROR   ";
	static char kquit[9] = "QUIT    ";
	static char kdelet[9] = "DELETE  ";
	static char ksyntx[9] = "SYNTAX  ";


	/*=====================================================================
	 * PURPOSE:  To perform standard error recovery when a syntax error is
	 *           discovered in the current command.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *    exm:     lcomcr
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     ncom, kcom, itypcm, flnum, ncerr
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     poptok, ctype, upcase
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - Command syntax error condition has been raised.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860819:  Upcased entire response to fix parsing bug.
	 *    831013:  Added command syntax print option.
	 *    830211:  Force response to come from user terminal.
	 *    820913:  Was not clearing error condition correctly.
	 *    820615:  Changes due to having CFMT raise syntax error condition.
	 *    820419:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860819
	 *===================================================================== */
	/* PROCEDURE: */
	nerr = 0;

	/* - Return if command correction option is off. */

	if( !cmexm.lcomcr )
		goto L_8888;

	/* - Prompt user for correction. */

L_1000:
	zgtmsg( kprmpt,MCMSG+1, kiomsg,9 );
	ncmsg = indexb( kiomsg,9 );

	/* - Pop first token off returned message. */

	if( ncmsg > 0 ){
		upcase( kiomsg, ncmsg, kiomsg,9 );
		icpntr = 0;
		poptok( kiomsg, ncmsg, &icpntr, &ic1, &ic2, &itype );
		if( itype <= 0 )
			goto L_1000;
		ic3 = ic2 - ic1 + 1;
		}

	/* - Check token versus allowed responses. */

	/* -- "ERROR", "QUIT", or <cr> means the user wishes to error out.
	 *     Since the command syntax error condition has already been raised
	 *     by CFMT, this is a no-op. */
	if( (memcmp(kiomsg+ic1 - 1,kerror,ic3)  == 0  ||
             memcmp(kiomsg+ic1 - 1,kquit,ic3)   == 0) || ncmsg == 0 ){

		/* -- "CHANGE" means the user wants to change the current token. */
		}
	else if( memcmp(kiomsg+ic1 - 1,kchang,ic3) == 0 ){
		poptok( kiomsg, ncmsg, &icpntr, &ic1, &ic2, &itype );
                fstrncpy( kmcom.kcom[cmcom.jcom - 1],8,kiomsg+ic1 - 1,ic2-ic1+1);
		ctype( &cmcom.jcom );
		cmcom.ncerr = 0;

		/* --"INSERT" means the user wishes to insert a new token. */
		}
	else if( memcmp(kiomsg+ic1 - 1,kinser,ic3) == 0 ){
		cmcom.ncom = cmcom.ncom + 1;
		for( j = cmcom.ncom; j >= (cmcom.jcom + 1); j-- ){
			j_ = j - 1;
			strcpy( kmcom.kcom[j_], kmcom.kcom[j_ - 1] );
			Itypcm[j] = Itypcm[j - 1];
			Flnum[j] = Flnum[j - 1];
			}
		poptok( kiomsg, ncmsg, &icpntr, &ic1, &ic2, &itype );
                fstrncpy( kmcom.kcom[cmcom.jcom - 1],8,kiomsg+ic1 - 1,ic2-ic1+1);
		ctype( &cmcom.jcom );
		cmcom.ncerr = 0;


		/* -- "DELETE" means the user wants to delete the current token. */
		}
	else if( memcmp(kiomsg+ic1 - 1,kdelet,ic3) == 0 ){
		cmcom.ncom = cmcom.ncom - 1;
		for( j = cmcom.jcom; j <= cmcom.ncom; j++ ){
			j_ = j - 1;
			strcpy( kmcom.kcom[j_], kmcom.kcom[j_ + 1] );
			Itypcm[j] = Itypcm[j + 1];
			Flnum[j] = Flnum[j + 1];
			}
		cmcom.ncerr = 0;

		/* -- "REPLACE" means the user wishes to replace the rest of the command. */
		}
	else if( memcmp(kiomsg+ic1 - 1,krepla,ic3) == 0 ){
		j = cmcom.jcom;
L_4000:
		poptok( kiomsg, ncmsg, &icpntr, &ic1, &ic2, &itype );
		if( itype > 0 ){
                        fstrncpy( kmcom.kcom[j - 1],8,kiomsg+ic1 - 1,ic2-ic1+1);
			ctype( &j );
			j = j + 1;
			goto L_4000;
			}
		cmcom.ncom = j - 1;
		cmcom.ncerr = 0;

		/* -- "SYNTAX" means print the command syntax from HELP package. */
		}
	else if( memcmp(kiomsg+ic1 - 1,ksyntx,ic3) == 0 ){
		wrhelp( (char*)kmcom.kcom[0],9, 2, FALSE , &nxerr );
		goto L_1000;

		/* -- Send explanation if the user entered an incorrect response. */
		}
	else{

                fstrncpy( kbufr,MCMSG,"To change the current symbol type: C symbol",43);
		nc = indexb( kbufr,MCMSG+1 );
                kbufr[nc] = '\0';
                fprintf(MUNOUT," %s\n", kbufr);

                fstrncpy( kbufr,MCMSG,"To insert a symbol before location type: I symbol",49);
		nc = indexb( kbufr,MCMSG+1 );
                kbufr[nc] = '\0';
                fprintf(MUNOUT," %s\n", kbufr);

                fstrncpy( kbufr,MCMSG,"To replace the rest of the command line type: R text",52);
		nc = indexb( kbufr,MCMSG+1 );
                kbufr[nc] = '\0';
                fprintf(MUNOUT," %s\n",kbufr);

                fstrncpy( kbufr,MCMSG,"To delete the current symbol type: D",36);
		nc = indexb( kbufr,MCMSG+1 );
                kbufr[nc] = '\0';
                fprintf(MUNOUT," %s\n",kbufr);

                fstrncpy( kbufr,MCMSG,"To error out of the current command type: E, Q, or <cr>",55);
		nc = indexb( kbufr,MCMSG+1 );
                kbufr[nc] = '\0';
                fprintf(MUNOUT," %s\n",kbufr);

                fstrncpy( kbufr,MCMSG,"To see the command syntax type: S",33);
		nc = indexb( kbufr,MCMSG+1 );
                kbufr[nc] = '\0';
                fprintf(MUNOUT," %s\n",kbufr);
		goto L_1000;
		}

L_8888:
	return;

} /* end of function */


