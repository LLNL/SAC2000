#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xtsize(nerr)
int *nerr;
{
	int j, j_;



	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command TSIZE.
	 *           This command sets various text size attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL: GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     MTXSIZ, OTXSIZ, DTXSIZ, OTXRAT, DTXRAT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     TXSIZ, TXRAT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LKRRC, LKREAL
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "OLD/NEW": set text sizes to old or new values. */
		if( lckey( "OLD$",5 ) ){
			for( j = 1; j <= MTXSIZ; j++ ){
				j_ = j - 1;
				Txsiz[j] = Otxsiz[j];
				}
			cmgem.txrat = cmgem.otxrat;
			}
		else if( lckey( "NEW$",5 ) ){
			for( j = 1; j <= MTXSIZ; j++ ){
				j_ = j - 1;
				Txsiz[j] = Dtxsiz[j];
				}
			cmgem.txrat = cmgem.dtxrat;

			/* -- "size v":  define a new value for one of the text sizes. */
			}
		else if( lkrrc( "TINY$",6, 0.0, 1.0, &Txsiz[1] ) ){
			}
		else if( lkrrc( "SMALL$",7, 0.0, 1.0, &Txsiz[2] ) ){
			}
		else if( lkrrc( "MEDIUM$",8, 0.0, 1.0, &Txsiz[3] ) ){
			}
		else if( lkrrc( "LARGE$",7, 0.0, 1.0, &Txsiz[4] ) ){

			/* -- "RATIO v":  set height/width character size ratio. */
			}
		else if( lkreal( "RATIO$",7, &cmgem.txrat ) ){

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
	 *    841105:  Original version.
	 *===================================================================== */

} /* end of function */

