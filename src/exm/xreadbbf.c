#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/bbs.h"
void /*FUNCTION*/ xreadbbf(nerr)
int *nerr;
{
	int notused;
        int localerr;

	/*=====================================================================
	 * PURPOSE:  To execute the action command READBBF
	 *           This command reads a blackboard variable file.
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
	 *    saclib:  lcmore, cfmt, cresp, lcchar, deletevlist, getvlist
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890324:  Added call to deletevlist prior to getvlist.
	 *    890309:  Changed from call to readvfile to getvlist.
	 *    890227:  Changed due to modified vars subroutines.
	 *    870916:  Made fully operational.
	 *    870301:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Delete the current blackboard list. */

	deletevlist( kmbbs.knmbbs,MCPFN+1, "MEMORY", nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "name":  the name of the global variable file. */
		if( lcchar( MCPFN, kmbbs.knmbbs,MCPFN+1, &notused ) ){

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

	/* - Get (read from disk) the new blackboard list. */

	getvlist( kmbbs.knmbbs,MCPFN+1, " ",2, "SINGLE", nerr );
        if( *nerr != 0 ){
          inibbs();
          inivars();
          createbbs(&localerr);
	}

L_8888:
	return;

} /* end of function */

