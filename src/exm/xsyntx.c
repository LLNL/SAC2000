#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ xsyntx(nerr)
int *nerr;
{
	char ktoken[9];
	int lintro, lnumbr;
	float fnumb;



	/*=====================================================================
	 * PURPOSE: To parse the action command SYNTAX.
	 *          This command prints syntax info from online help package.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EXM:     KINTRO
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCTOK, ICTOK, WRHELP
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	lintro = TRUE;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "token":  the name of a help package. */
		if( lctok( ktoken,9, &lnumbr, &fnumb ) ){
			lintro = FALSE;
			wrhelp( ktoken,9, 2, FALSE, nerr );
			if( *nerr == 0 ){
				ictok( 1 );
				}
			else if( *nerr < 0 ){
				*nerr = 0;
				goto L_8888;
				}
			else{
				goto L_8888;
				}

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

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    831013:  Original version (from XHELP.)
	 *===================================================================== */

} /* end of function */

