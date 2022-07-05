#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ plgrid(nerr)
int *nerr;
{

	/*=====================================================================
	 * PURPOSE:  To draw grid lines, x and y axes and labels, and title.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:  NONE
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LBDR, TSAXIS, ILIN, ILOG, TSDEF
	 *             IXINT, LXLAB, KXLAB, NXLAB, IXLABP, TSXLAB,
	 *             IYINT, LYLAB, KYLAB, NYLAB, IYLABP, TSYLAB,
	 *             LTITL, KTITL, NTITL, ITITLP, TSTITL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  RECTANGLE, SETTEXTSIZE, XLINAX, XLOGAX, CENTXT, YLINAX, YLOGAX, PL
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Draw border around plot window if requested. */

	if( cmgem.lbdr )
		rectangle( &cmgem.xpmnu, &cmgem.xpmxu, &cmgem.ypmnu, &cmgem.ypmxu );

	/* - X axis annotation. */

	cmgem.chht = cmgem.tsaxis;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );
	if( cmgem.ixint == cmgem.ilin ){
		xlinax();
		}
	else if( cmgem.ixint == cmgem.ilog ){
		xlogax();
		}
	/* - Y axis annotation. */

	if( cmgem.iyint == cmgem.ilin ){
		ylinax();
		}
	else if( cmgem.iyint == cmgem.ilog ){
		ylogax();
		}

	/* - Axes labels and title. */

	if( cmgem.lylab )
		centxt( kmgem.kylab,145, cmgem.nylab, cmgem.iylabp, cmgem.tsylab );
	if( cmgem.lxlab )
		centxt( kmgem.kxlab,145, cmgem.nxlab, cmgem.ixlabp, cmgem.tsxlab );
	if( cmgem.ltitl )
		centxt( kmgem.ktitl,145, cmgem.ntitl, cmgem.ititlp, cmgem.tstitl );

	/* - General plot labels. */

	plplab();



	/* - Reset default text size. */

	cmgem.chht = cmgem.tsdef;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860605:  Fixed bug involving text size of y axis annotation.
	 *    830209:  Added call to PLPLAB.
	 *    811228:  Deleted calls to ZCLIP.
	 *===================================================================== */

} /* end of function */

