#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ pl2d(xarray, yarray, number, incx, incy, nerr)
float xarray[], yarray[];
int number, incx, incy, *nerr;
{

	float *const Xarray = &xarray[0] - 1;
	float *const Yarray = &yarray[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To produce a "standard" x-y plot in current plot window.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    XARRAY:  X data array. [f]
	 *    YARRAY:  Y data array. [f]
	 *    NUMBER:  Number of data pairs to plot. [i]
	 *    INCX:    Increment in XARRAY array between data points. [i]
	 *    INCY:    Increment in YARRAY array. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:
	 *             0902:  Tried to plot negative data on a logarithmic plot.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     LFRAME, ISKLIN, ISKCOL, IBACOL, TSDEF, TXRAT, ITHIN
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     CHHT, CHWID
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  BEGINFRAME, SETLINESTYLE, SETCOLOR,
	 *             PLMAP, PLDTA, PLGRID, PLHOME, ENDFRAME, SETLINEWIDTH
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920526:  Added line-width.  TEXT is always thin.
	 *    830926:  Removed QDP logic.  Put in PLDTA.
	 *    821228:  Removed calls to DISPID and DISPPK.
	 *    811012:  Added call to home cursor after finishing plot.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Set background and skeleton attributes. */

	settexttype( kmgem.kgtqua );
	settextfont( cmgem.igtfnt );
	cmgem.chht = cmgem.tsdef;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );
	setlinestyle( cmgem.isolid );
	setlinewidth( cmgem.ithin );
	setcolor( cmgem.iskcol );

	/* - Begin new frame. */

	if( cmgem.lframe )
		beginframe( FALSE , nerr );
	getvspace( &cmgem.xvspmn, &cmgem.xvspmx, &cmgem.yvspmn, &cmgem.yvspmx );

	/* - Calculate new mapping. */

	plmap( xarray, yarray, number, incx, incy, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Plot data. */
	pldta( xarray, yarray, number, incx, incy, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Draw grid lines, label axes and titles. */

	if( !cmsss.lPlottingTT ) {	/* if statement added to allow prs in 
					   origin right mode.  maf 961004 */
	    plgrid( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}


	/* - Home cursor. */

	plhome();

	/* - End current frame. */

	if( cmgem.lframe )
		endframe( FALSE , nerr );


L_8888:
	return;


} /* end of function */

