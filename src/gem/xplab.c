#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xplab(nerr)
int *nerr;
{
	int iplabs, notusd, ntemp;
	float temp[3];

	float *const Temp = &temp[0] - 1;


	/* Ind
	 *=====================================================================
	 * PURPOSE: To parse the parameter-setting command PLABEL.
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
	 *    GEM:     MTXSIZ, KTXSIZ, KTXPOS, IHORZ, IVERT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LPLAB, TSPLAB, TAPLAB, NPLAB, KPLAB
	 *             LPLABL(), XPLABL(), YPLABL()
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG, LKLIST, LCQUOT
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Parse positional tokens first. */

	/* -- "n":  the number of the label to modify. */
	if( lcirc( 1, MPLAB, &cmgem.nplab ) ){

		/* -- otherwise, increment the label number. */
		}
	else{
		cmgem.nplab = min( MPLAB, cmgem.nplab + 1 );
		}

	/* - If this is the first reference to this label number,
	 *   assume it is to be placed "below" the last one,
	 *   and using the same size characters. */

	if( memcmp(kmgem.kplab[cmgem.nplab - 1],"        ",8) == 0 ){
		Lplabl[cmgem.nplab] = TRUE;
		cmgem.tsplab[cmgem.nplab] = cmgem.tsplab[cmgem.nplab - 1];
		}

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- Turn plot labeling on/off: */
		if( lclog( &Lplab[cmgem.nplab] ) ){

			/* -- Define text of plot label: */
			}
		else if( lcquot( MCPTXT, (char*)kmgem.kplab[cmgem.nplab - 1]
		 ,145, &notusd ) ){
			Lplab[cmgem.nplab] = TRUE;

			/* -- Set plot label size: */
			}
		else if( lklist( "S$",3, (char*)kmgem.ktxsiz,9, MTXSIZ, &iplabs ) ){
			cmgem.tsplab[cmgem.nplab] = Txsiz[iplabs];

			/* -- Set location of plot label: */
			}
		else if( lkra( "P$",3, 2, 3, temp, &ntemp ) ){
			Lplabl[cmgem.nplab] = FALSE;
			cmgem.xplabl[cmgem.nplab] = Temp[1];
			cmgem.yplabl[cmgem.nplab] = Temp[2];
			if( ntemp == 3 ){
				cmgem.taplab[cmgem.nplab] = Temp[3];
				}
			else{
				cmgem.taplab[cmgem.nplab] = 0.;
				}

			/* -- Set location of plot label to be below previous label. */
			}
		else if( lckey( "B$",3 ) ){
			Lplabl[cmgem.nplab] = TRUE;

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

