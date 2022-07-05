#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/msg.h"
void /*FUNCTION*/ xecho(nerr)
int *nerr;
{
	int lactivate, lcontentreq, lcontents[MTPMSG], lflag;

	int *const Lcontents = &lcontents[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse the action command ECHO.
	 *          This command controls echoing of output to the terminal.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    
	 *    msg:     MTPMSG, ktpmsg
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    exm:     knametranscript, nuntranscript, imodetranscript,
	 *             itranscript
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, lclog, lckey, cfmt, cresp, inquiremsg, sendmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890324:  Modified command logic.
	 *    890306:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890306
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/*   PARSING PHASE: */

	/* - Get the current contents being sent to the terminal. */

	inquiremsg( MUNOUT, &lactivate, lcontents );

	/* - Loop on each token in command: */

	lflag = TRUE;
	lcontentreq = FALSE;

L_1000:
	if( lcmore( nerr ) ){

		/* -- "ON|OFF": Flag to say whether echoing is desired or not. */
		if( lclog( &lflag ) ){

			/* -- "ERRORS":  errors messages. */
			}
		else if( lckey( "ERRORS$",8 ) ){
			Lcontents[1] = lflag;
			lcontentreq = TRUE;

			/* -- "WARNINGS":  warning messages. */
			}
		else if( lckey( "WARNINGS$",10 ) ){
			Lcontents[2] = lflag;
			lcontentreq = TRUE;

			/* -- "OUTPUT":  output messages. */
			}
		else if( lckey( "OUTPUT$",8 ) ){
			Lcontents[3] = lflag;
			lcontentreq = TRUE;

			/* -- "COMMANDS":  commands typed at the terminal. */
			}
		else if( lckey( "COMMANDS$",10 ) ){
			Lcontents[4] = lflag;
			lcontentreq = TRUE;

			/* -- "MACROS":  command executed from a macro file. */
			}
		else if( lckey( "MACROS$",8 ) ){
			Lcontents[5] = lflag;
			lcontentreq = TRUE;

			/* -- "PROCESSED":  "processed" commands. */
			}
		else if( lckey( "PROCESSED$",11 ) ){
			Lcontents[6] = lflag;
			lcontentreq = TRUE;

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	/*   EXECUTION PHASE: */

	/* - If no content was input assume "commands", "macros", and "processed".
	 *   This is to maintain consistency with a previous version of this command. */

	if( !lcontentreq ){
		Lcontents[4] = lflag;
		Lcontents[5] = lflag;
		Lcontents[6] = lflag;
		}

	/* - Set the new message content to be sent to the terminal. */

	sendmesg( MUNOUT, lactivate, lcontents );

L_8888:
	return;

} /* end of function */

