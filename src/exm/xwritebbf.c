#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ xwritebbf(nerr)
int *nerr;
{
	char kname[MCPFN+1];
	int notused;

	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITEBBF.
	 *           This command writes a blackboard variable file.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      nerr:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    bbs:     knmbbs
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lcchar, writevfile
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890227:  Changes due to modified vars subroutines.
	 *    870916:  Made fully operational.
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870916
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - PARSING PHASE: */

	/* - Loop on each token in command: */

	fstrncpy( kname, MCPFN, " ", 1 );
L_1000:
	if( lcmore( nerr ) ){

		/* -- "name":  the name of the global variable file. */
		if( lcchar( MCPFN, kname,MCPFN+1, &notused ) ){

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

	/* - EXECUTION PHASE: */

	writevfile( kmbbs.knmbbs,MCPFN+1, kname, nerr );

L_8888:
	return;

} /* end of function */

