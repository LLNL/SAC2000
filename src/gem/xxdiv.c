#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xxdiv(nerr)
int *nerr;
{



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command XDIV.
	 *          This command sets x axes division spacing attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LXDIV, LNXDIV, XDIV, NXDIV
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LKREAL, LKINT
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Set up nice numbering. */
		if( lckey( "NI$",4 ) ){
			cmgem.lxdiv = FALSE;
			cmgem.lnxdiv = FALSE;

			/* -- Set up fixed division spacings. */
			}
		else if( lkreal( "I$",3, &cmgem.xdiv ) ){
			cmgem.lxdiv = TRUE;
			cmgem.lnxdiv = FALSE;

			/* -- Set up a fixed number of divisions. */
			}
		else if( lkirc( "NU$",4, 1, 100, &cmgem.nxdiv ) ){
			cmgem.lxdiv = FALSE;
			cmgem.lnxdiv = TRUE;

			/* -- Turn power labeling on/off. */
			}
		else if( lklog( "P$",3, &cmgem.lxpowr ) ){

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
	 *    840531:  Added power labeling flag option.
	 *    820610:  Original version (from GEMCOM.)
	 *===================================================================== */

} /* end of function */

