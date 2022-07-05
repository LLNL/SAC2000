#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ plplab()
{
	int j, j_;
	float angcur, chhts, chwids, xcur, ycur;


	/*=====================================================================
	 * PURPOSE:  To write plot labels to current frame.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     NPLAB, LPLAB, KPLAB, LPLABL, XPLABL, YPLABL, YVSPMX
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     XPLABL, YPLABL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  SETTEXTSIZE, SETTEXTANGLE, SETTEXTJUST, PLTEXT
	 *===================================================================== */
	/* PROCEDURE: */
	/* -- Save current text size. */
	chwids = cmgem.chwid;
	chhts = cmgem.chht;

	/* - Set up text "current point." */

	settextangle( 0. );
	angcur = 0.;
	settextjust( "LEFT", "BOTTOM" );

	/* - For each label: */

	for( j = 1; j <= MPLAB; j++ ){
		j_ = j - 1;

		/* -- If label is to be plotted: */
		if( Lplab[j] ){

			/* --- Set text size. */
			cmgem.chht = cmgem.tsplab[j];
			cmgem.chwid = cmgem.txrat*cmgem.chht;
			settextsize( cmgem.chwid, cmgem.chht );

			/* --- If location of label is to be "below" last label,
			 *     calculate the position. */
			if( Lplabl[j] ){
				xcur = xcur + cmgem.chht*sin( TORAD*angcur );
				ycur = ycur - cmgem.chht*cos( TORAD*angcur );
				}
			else{
				/* --- Otherwise, use defined text location and orientation. */
				xcur = cmgem.xplabl[j];
				ycur = cmgem.yplabl[j]*cmgem.yvspmx;
				angcur = cmgem.taplab[j];
				settextangle( angcur );
				}

			/* --- Finally ready to actually plot the label. */
			pltext( (char*)kmgem.kplab[j_],145, xcur, ycur );

			}

		}

	/* - Restore current character size. */

	settextsize( chwids, chhts );
	settextangle( 0. );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850208:  Added call to SETTEXTJUST.
	 *    830209:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850208
	 *===================================================================== */

} /* end of function */

