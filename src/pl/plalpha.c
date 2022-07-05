#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"
#include "gam.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void /*FUNCTION*/ plalpha(kalpha, kalpha_s, malpha, lprint, nerr)
char *kalpha;   int kalpha_s;
int malpha, *nerr;
int lprint ;
{
#define KALPHA(I_,J_)	(kalpha+(I_)*(kalpha_s)+(J_))
	int lany, lframs, lxgens;
	int idx, ildp, npoints, nc, nlcx, nlcy, nlen;
	float xloc, xpw[2], yloc, ypw[2];
	void zgetgd();

	float *const Xpw = &xpw[0] - 1;
	float *const Ypw = &ypw[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To plot tha alpha text aint with the first file that
	 *           is in memory.  Plots at most malpha items.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KALPHA:  String of text to plot. (ka)
	 *    MALPHA:  Maximum number of string to plot. (i)
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  GAM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     LXGEN, XDELTA, XFIRST, LYLIM, YIMN, YIMX
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  VFLIST, GETSTATUS, BEGINDEVICES, GETFIL, GETXLM,
	 *             GETYLM, INDEXB, BEGINFRAME, PL2D, DISPID, DISPPK, PLHOME,
	 *             ENDFRAME, UPCASE, TEXT, MOVE
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NLCX:    Location in SACMEM array of each file's x data.
	 *    NLCY:    Location in SACMEM array of each file's y data.
	 *    NLEN:    Length of each data file.
	 *    XLOC:    Coordinate in the x direction to put label.
	 *    YLOC:    Coordinate in the y direction to put label.
	 *    xpw:     Two element array containing x plot window. [fa]
	 *    ypw:     Two element array containing y plot window. [fa]
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970130:  Added arguments to dispid() to not plot file number. maf
	 *    920713:  Original version from xp.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Save current values of several GEM parameters. */

	lxgens = cmgem.lxgen;
	lframs = cmgem.lframe;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;


	/* - If no graphics device is open, try to open the default device. */

	getstatus( "ANY", &lany );
	if( !lany ){
		zgetgd( kmgam.kgddef,9 );
		begindevices( kmgam.kgddef,9, 1, nerr );
		if( *nerr != 0 )
			goto L_8888;
	}

	/* EXECUTION PHASE: */


	/* -- Get file from memory manager. */
	getfil( 1, TRUE, &nlen, &nlcy, &nlcx, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* -- Set up x axis data generation parameters if evenly spaced. */
	if( *leven ){
		cmgem.xdelta = *delta;
		cmgem.xfirst = *begin;
		cmgem.lxgen = TRUE;
	}
	else{
		cmgem.lxgen = FALSE;
	}

	/* -- Determine x axis plot limits. */
	getxlm( &cmgem.lxlim, &cmgem.ximn, &cmgem.ximx );

	/* -- Determine y axis plot limits. */
	getylm( &cmgem.lylim, &cmgem.yimn, &cmgem.yimx );

	/* -- Begin next plot frame if requested. */
	if( lframs ){
		beginframe( lprint , nerr );
		if( *nerr != 0 )
			goto L_8888;
		getvspace( &cmgem.xvspmn, &cmgem.xvspmx, &cmgem.yvspmn, &cmgem.yvspmx );
	}

	/* -- Plot the data.  Do not allow PL2D to perform framing. */
	cmgem.lframe = FALSE;
	pl2d( cmmem.sacmem[nlcx], cmmem.sacmem[nlcy], nlen, 1, 1, nerr );
	if( *nerr != 0 )
		goto L_8888;


	/* - Set up plotting window */

	Xpw[1] = cmgem.xpmnu - VSMALL;
	Xpw[2] = cmgem.xpmxu + VSMALL;
	Ypw[1] = cmgem.ypmnu - VSMALL;
	Ypw[2] = cmgem.ypmxu + VSMALL;


	/* -- Plot the alpha strings. */
	npoints = nlen;
	if( npoints > malpha )
		npoints = malpha;
	for( idx = 0; idx < npoints; idx++ ){
		if( cmgem.lxgen ){
			xloc = cmgem.xmpip1*(cmgem.xfirst + idx*cmgem.xdelta) + 
			 cmgem.xmpip2;
		}
		else{
			xloc = cmgem.xmpip1*(*(cmmem.sacmem[nlcx] + idx)) + cmgem.xmpip2;
		}
		yloc = cmgem.ympip1*(*(cmmem.sacmem[nlcy] + idx)) + cmgem.ympip2;
		move( xloc, yloc );
		locdp( xloc, yloc, xpw, ypw, &ildp );

		if( ildp == 0 ){
			nc = indexb( KALPHA(idx,0),kalpha_s );
			text( KALPHA(idx,0),kalpha_s, nc );
		}
	}


	/* -- Plot the frame id, pick display and home cursor. */
	dispid( 0 , 0 );	/* added arguments.  maf 970130 */
	disppk( 0. );
	plhome();
	flushbuffer( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* -- End current plot frame if requested. */
	if( lframs )
		endframe( FALSE , nerr );


	/* - Restore GEM parameters before returning. */

L_8888:
	cmgem.lxgen = lxgens;
	cmgem.lframe = lframs;
	return;

#undef	KALPHA
} /* end of function */

