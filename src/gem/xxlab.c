#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xxlab(nerr)
int *nerr;
{
	int ixlabs;
	float taxlab;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command XLABL.
	 *          This command sets the x axis text labeling attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     MCTEXT, KTXSIZ, MTXSIZ, KTXPOS,
	 *             ITOP, IBOT, IRIGHT, ILEFT, HORZ, VERT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LXLAB, IXLABS, IXLABP, IXLABO, NXLAB, KXLAB
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG, LKLIST, LCQUOT
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn titling on/off: */
		if( lclog( &cmgem.lxlab ) ){

			/* -- Define text of x label: */
			}
		else if( lcquot( MCPTXT, kmgem.kxlab,145, &cmgem.nxlab ) ){
			cmgem.lxlab = TRUE;

			/* -- Set x label size: */
			}
		else if( lklist( "S$",3, (char*)kmgem.ktxsiz,9, MTXSIZ, &ixlabs ) ){
			cmgem.tsxlab = Txsiz[ixlabs];

			/* -- Set location of x label: */
			}
		else if( lklist( "L$",3, (char*)kmgem.ksides,9, 4, &cmgem.ixlabp ) ){
			if( cmgem.ixlabp == cmgem.itop || cmgem.ixlabp == cmgem.ibot ){
				taxlab = cmgem.horz;
				}
			else{
				taxlab = cmgem.vert;
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
	 *    830818:  Changes due to new text size and angle attributes.
	 *    820924:  Moved LCQUOT to top of parse loop.
	 *    820614:  Original version.
	 *===================================================================== */

} /* end of function */

