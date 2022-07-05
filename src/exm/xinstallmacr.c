#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xinstallmacro(nerr)
int *nerr;
{
	char kmsg[MCMSG+1], kname[MCPFN+1];
	int lexist;
	int nchar;

	/*=====================================================================
	 * PURPOSE:  To execute the action command INSTALLMACRO.
	 *           This command installs a SAC macro in global directory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LKCHAR, ZAUXFILE
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920305:  Changed the DO loop to allow installation of more than
	 *             one macro at the time as described in the manual.
	 *             Guy Tytgat.
	 *    871222:  Moved file inquire to zinquire.
	 *    870921:  Moved most of procedure for copying to zauxfile.
	 *    870416:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870416
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

	while( lcmore( nerr ) ){

		/* -- "text":  the name of a macro to install. */
		if( lcchar( MCPFN, kname,MCPFN+1, &nchar ) ){
			modcase( FALSE, kname, nchar, kname );

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}

		/* - The above loop is over when one of two conditions has been met:
		 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
		 *   (2) All the tokens in the command have been successfully parsed. */

		/*   CHECKING PHASE: */

		/* - Make sure macro file exists. */

		zinquire( kname, &lexist );

		if( !lexist ){
			*nerr = 108;
			setmsg( "ERROR", *nerr );
			apcmsg( kname,MCPFN+1 );
			goto L_8888;
			}

		/* - Install macro in the SAC auxiliary directory. */

		zauxfile( "macros",7, kname,MCPFN+1, nerr );

		}

L_8888:
	return;
} /* end of function */

