#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xqdp(nerr)
int *nerr;
{
	int ltemp;
	int itemp;



	/*=====================================================================
	 * PURPOSE:  To parse the parameter-setting command QDP.
	 *           This command controls the "quick-and-dirty" plot options.
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
	 *    GEM:     LTQDP, NTQDP, LFQDP, NFQDP
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, LCLOG, CFMT, CRESP
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "ON/OFF/n":  change both terminal and SGF QDP parameters. */
		if( lclog( &ltemp ) ){
			cmgem.ltqdp = ltemp;
			cmgem.lfqdp = ltemp;
			}
		else if( lcint( &itemp ) ){
			cmgem.ntqdp = itemp;
			cmgem.ltqdp = TRUE;
			cmgem.nfqdp = itemp;
			cmgem.lfqdp = TRUE;

			/* -- "TERM ON/OFF/n":  change terminal QDP parameters only. */
			}
		else if( lklogi( "TERM$",6, &cmgem.ltqdp, &cmgem.ntqdp ) ){

			/* -- "SGF ON/OFF/n":  change SGF QDP parameters only. */
			}
		else if( lklogi( "SGF$",5, &cmgem.lfqdp, &cmgem.nfqdp ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;
		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0.
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850219:  Original version.
	 *===================================================================== */

} /* end of function */

