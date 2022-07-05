#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ xunsetbb(nerr)
int *nerr;
{
	char kname[MCMSG+1];
	int nchar;

	/*=====================================================================
	 * PURPOSE:  To execute the action command UNSETBB.
	 *           This command unsets or deletes blackboard variables.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      nerr:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    mcmsg
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lckey, lcchar, 
	 *             deletebbs, createbbs, unsetbbv
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kname:   Name of blackboard variable. [c]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880412:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880412
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "ALL": unset all blackboard variables by deleting the current
		 *     blackboard and creating a new one. */
		if( lckey( "ALL#$",6 ) ){
			deletebbs( nerr );
			if( *nerr != 0 )
				goto L_8888;
			createbbs( nerr );
			if( *nerr != 0 )
				goto L_8888;

			/* -- "name":  the name of a specific blackboard variable to unset. */
			}
		else if( lcchar( MCMSG, kname,MCMSG+1, &nchar ) ){
			unsetbbv( kname, nerr, MCMSG );
			if( *nerr != 0 )
				goto L_8888;

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

L_8888:
	return;

} /* end of function */

