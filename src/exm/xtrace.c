#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MCNAME	16

#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ xtrace(nerr)
int *nerr;
{
	char kname[17];
	int ltracebb;
	int indexcomma, ncname;
	static int ltracevar = TRUE;



	/*=====================================================================
	 * PURPOSE: To parse the action command TRACE.
	 *          This command controls the tracing and reporting of values
	 *          of header adn blackboard variables.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPFN
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    exm:     ltracevar, ltracebb
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, tracevariable
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881230:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881230
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/*   PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "ON|OFF":  Turn variable tracing on or off. */
		if( lclog( &ltracevar ) ){

			/* -- "name":  The name of a header or blackboard variable. */
			}
		else if( lcchar( MCNAME, kname,17, &ncname ) ){
			indexcomma = indexa( kname,17, ',', TRUE, TRUE );
			ltracebb = indexcomma == 0;
			tracevariable( ltracevar, ltracebb, kname, nerr );
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

	/*   EXECUTION PHASE: */

L_8888:
	return;

} /* end of function */

