#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xylab(nerr)
int *nerr;
{
	int iylabs;
	float taylab;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command YLABL.
	 *          This command sets the y axis text labeling attributes.
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
	 *    GEM:     LYLAB, TSYLAB, IYLABP, TAYLAB, NYLAB, KYLAB
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG, LKLIST, LCQUOT
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn y labeling on/off: */
		if( lclog( &cmgem.lylab ) ){

			/* -- Define text of y label: */
			}
		else if( lcquot( MCPTXT, kmgem.kylab,145, &cmgem.nylab ) ){
			cmgem.lylab = TRUE;

			/* -- Set y label size: */
			}
		else if( lklist( "S$",3, (char*)kmgem.ktxsiz,9, MTXSIZ, &iylabs ) ){
			cmgem.tsylab = Txsiz[iylabs];

			/* -- Set location of y label: */
			}
		else if( lklist( "L$",3, (char*)kmgem.ksides,9, 4, &cmgem.iylabp ) ){
			if( cmgem.iylabp == cmgem.itop || cmgem.iylabp == cmgem.ibot ){
				taylab = cmgem.horz;
				}
			else{
				taylab = cmgem.vert;
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

