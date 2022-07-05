#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"
void /*FUNCTION*/ plmap(xarray, yarray, number, incx, incy, nerr)
float xarray[], yarray[];
int number, incx, incy, *nerr;
{
	int jdx, jx, jy, num1, num2, num2m1;
	float delx, dely, fjunk, tmp, xvspdl, yvspdl;

	float *const Xarray = &xarray[0] - 1;
	float *const Yarray = &yarray[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To perform mapping from input to plot coordinate systems.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xarray:  X data array. [f]
	 *    yarray:  Y data array. [f]
	 *    number:  Number of data pairs to plot. [i]
	 *    incx:    Increment in XARRAY array between data points. [i]
	 *    incy:    Increment in YARRAY array. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:  NONE
	 *=====================================================================
	 * MODULE/LEVEL:  gem/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    vlarge, vsmall
	 *    gem:     lxlim, ximn, ximx, lylim, yimn, yimx, yvspmx
	 *             lxgen, xfirst, xdelta, lygen, yfirst, ydelta,
	 *             ximn, ximx, yimn, yimx, ixint, iyint, ilin, ilog,
	 *             lxfudg, lyfudg, lxrev, lyrev, lxmpiw, lympiw
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     ximnu, ximxu, yimnu, yimxu,
	 *             ximnz, ximxz, yimnz, yimxz, xpmnu, xpmxu, ypmnu, ypmxu,
	 *             xmpip1, xmpip2, ympip1, ympip2
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     extrma, plcalwvtrans, setvport, setworld
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900306:  Moved mapping transformation from here to plcalwvtrans.
	 *             Added calls to set viewport and world coordinates.
	 *    870602:  Added coding to scale plot window to current viewspace.
	 *    870112:  Deleted coding to reverse plot window if limits reversed.
	 *    860404:  Made floor for log plots a fraction of maximum.
	 *    860213:  Deleted lnice logic from x and y limit calculations.
	 *    841218:  Fixed bug involving log mapping with non-positive limits.
	 *    831103:  Added scaling for viewspace ratio.
	 *    830929:  Deleted extra mapping from "input" to "world" coordinates.
	 *    830811:  Replaced call to zmnmxr with do-loop.
	 *    810000:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870112
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - X limits are either fixed or are scaled to the input data. */

	if( cmgem.lxlim ){
	    cmgem.ximnu = cmgem.ximn;
	    cmgem.ximxu = cmgem.ximx;
	}
	else{
	    if( cmgem.lxgen ){
		cmgem.ximnu = cmgem.xfirst;
		cmgem.ximxu = cmgem.xfirst + (number - 1)*cmgem.xdelta;
	    }
	    else
		extrma( xarray, incx, number, &cmgem.ximnu, &cmgem.ximxu, &fjunk );
	}

	/* - Y limits are a bit more complicated:
	 *   (1) They can have fixed values.
	 *   (2) They can be scaled to the entire y array if the x data is also scaled.
	 *   (3) They can be scaled to that portion of the y array, where the
	 *       corresponding x value is within the fixed x range.
	 *       This subcase itself has two subcases (getting confused?):
	 *       (3a) X data array present; range checked extrema calculation used.
	 *       (3b) X data to be generated; extrma used but the range of the y
	 *            array search is confined to the equivalent x data limits. */

	if( cmgem.lylim ){
	    cmgem.yimnu = cmgem.yimn;
	    cmgem.yimxu = cmgem.yimx;
	}
	else{
	    if( cmgem.lygen ){
		cmgem.yimnu = cmgem.yfirst;
		cmgem.yimxu = cmgem.yfirst + (number - 1)*cmgem.ydelta;
	    }
	    else{
		if( cmgem.lxlim ){
		    if( cmgem.lxgen ){
			num1 = (int)( (cmgem.ximnu - cmgem.xfirst)/cmgem.xdelta ) + 1;
			if( num1 < 1 )
			    num1 = 1;
			num2 = (int)( (cmgem.ximxu - cmgem.xfirst)/cmgem.xdelta ) + 1;
			if( num2 > number )
			    num2 = number;
			if( num1 <= number && num2 >= 1 ){
			    num2m1 = num2 - num1 + 1;
			    extrma( &Yarray[num1], incy, num2m1, &cmgem.yimnu, 
			     &cmgem.yimxu, &fjunk );
			}
			else{
			    cmgem.yimnu = -1.;
			    cmgem.yimxu = 1.;
			}
		    } /* end if( cmgem.lxgen ) */
		    else{
			jx = 1;
			jy = 1;
			cmgem.yimxu = -VLARGE;
			cmgem.yimnu = VLARGE;
			for( jdx = 1; jdx <= number; jdx++ ){
			    if( Xarray[jx] >= cmgem.ximnu && Xarray[jx] <= cmgem.ximxu ){
				if( Yarray[jy] < cmgem.yimnu )
				    cmgem.yimnu = Yarray[jy];
				if( Yarray[jy] > cmgem.yimxu )
				    cmgem.yimxu = Yarray[jy];
			    }
			    jx = jx + incx;
			    jy = jy + incy;
			} /* end for ( jdx ) */
		    } /* end else associated with if( cmgem.lxgen ) */
		} /* end if( cmgem.lxlim ) */
		else
		    extrma( yarray, incy, number, &cmgem.yimnu, &cmgem.yimxu, &fjunk );
	    } /* end else associated with if( cmgem.lygen ) */
	} /* end else associated with if( cmgem.lylim ) */

	/* - Adjust data limits to produce 'nice' plots if limits not fixed.
	 * - If LXFUDG or LYFUDG is .TRUE. make that data window
	 *   slightly larger than extrema.
	 * - If logarithmic:
	 *   (1) Make sure data limits are positive.
	 *   (2) Change data limits to log10 of data limits.
	 *   (3) If LXFULL or LYFULL is true, make data limits whole decades. */

	if( cmgem.ixint == cmgem.ilin ){
	    cmgem.ximnz = cmgem.ximnu;
	    cmgem.ximxz = cmgem.ximxu;
	    if( cmgem.lxfudg && !cmgem.lxlim ){
		delx = cmgem.xfudg*(cmgem.ximxz - cmgem.ximnz);
		if( delx < VSMALL )
		    delx = 0.00001*fabs( cmgem.ximnz );
		cmgem.ximnz = cmgem.ximnz - delx;
		cmgem.ximxz = cmgem.ximxz + delx;
	    }
	}
	else if( cmgem.ixint == cmgem.ilog ){
	    if( cmgem.ximxu <= 0. )
		cmgem.ximxu = 1.;
	    cmgem.ximxz = log10( cmgem.ximxu );
	    if( cmgem.ximnu <= 0. )
		cmgem.ximnu = fmax( cmgem.floor*cmgem.ximxu, VSMALL );
	    cmgem.ximnz = log10( cmgem.ximnu );
	    if( !cmgem.lxfull ){
		if( cmgem.lxfudg ){
		    delx = cmgem.xfudg*(cmgem.ximxz - cmgem.ximnz);
		    cmgem.ximnz = cmgem.ximnz - delx;
		    cmgem.ximxz = cmgem.ximxz + delx;
		}
		if( cmgem.lxfull ){
		    tmp = cmgem.ximnz;
		    if( tmp < 0. )
			tmp = tmp - 0.99999;
		    cmgem.ximnz = (float)( (int)( tmp ) );
		    tmp = cmgem.ximxz;
		    if( tmp > 0. )
			tmp = tmp + 0.99999;
		    cmgem.ximxz = (float)( (int)( tmp ) );
		} /* end if( cmgem.lxfull ) */
	    } /* end if( !cmgem.lxfull ) */
	} /* end else if( cmgem.ixint == cmgem.ilog ) */

	if( cmgem.iyint == cmgem.ilin ){
	    cmgem.yimnz = cmgem.yimnu;
	    cmgem.yimxz = cmgem.yimxu;
	    if( cmgem.lyfudg && !cmgem.lylim ){
		dely = cmgem.yfudg*(cmgem.yimxz - cmgem.yimnz);
		if( dely < VSMALL )
		    dely = 0.00001*fabs( cmgem.yimnz );
		cmgem.yimnz = cmgem.yimnz - dely;
		cmgem.yimxz = cmgem.yimxz + dely;
	    }
	} /* end if( cmgem.iyint == cmgem.ilin ) */
	else if( cmgem.iyint == cmgem.ilog ){
	    if( cmgem.yimxu <= 0. )
		cmgem.yimxu = 1.;
	    cmgem.yimxz = log10( cmgem.yimxu );
	    if( cmgem.yimnu <= 0. )
		cmgem.yimnu = fmax( cmgem.floor*cmgem.yimxu, VSMALL );
	    cmgem.yimnz = log10( cmgem.yimnu );
	    if( !cmgem.lylim ){
		if( cmgem.lyfudg ){
		    dely = cmgem.yfudg*(cmgem.yimxz - cmgem.yimnz);
		    cmgem.yimnz = cmgem.yimnz - dely;
		    cmgem.yimxz = cmgem.yimxz + dely;
		}
		if( cmgem.lyfull ){
		    tmp = cmgem.yimnz;
		    if( tmp < 0. )
			tmp = tmp - 0.99999;
		    cmgem.yimnz = (float)( (int)( tmp ) );
		    tmp = cmgem.yimxz;
		    if( tmp > 0. )
			tmp = tmp + 0.99999;
		    cmgem.yimxz = (float)( (int)( tmp ) );
		} /* end if( cmgem.lyfull ) */
	    } /* end if( !cmgem.lylim ) */
	} /* end else if( cmgem.iyint == cmgem.ilog ) */

	/* - Map plot window to current viewspace. */

	xvspdl = cmgem.xvspmx - cmgem.xvspmn;
	cmgem.xpmnu = cmgem.xvspmn + cmgem.xpmn*xvspdl;
	cmgem.xpmxu = cmgem.xvspmn + cmgem.xpmx*xvspdl;

	yvspdl = cmgem.yvspmx - cmgem.yvspmn;
	cmgem.ypmnu = cmgem.yvspmn + cmgem.ypmn*yvspdl;
	cmgem.ypmxu = cmgem.yvspmn + cmgem.ypmx*yvspdl;

	/* - Reverse either plot window. */

	if( cmgem.lxrev ){
	    tmp = cmgem.xpmnu;
	    cmgem.xpmnu = cmgem.xpmxu;
	    cmgem.xpmxu = tmp;
	}

	if( cmgem.lyrev ){
	    tmp = cmgem.ypmnu;
	    cmgem.ypmnu = cmgem.ypmxu;
	    cmgem.ypmxu = tmp;
	}

	/* - If XIMNZ eq XIMXZ, make XIMXZ slightly larger. */

	if( cmgem.ximnz == cmgem.ximxz ){
	    if( cmgem.ximnz == 0. ){
		cmgem.ximxz = 0.000001;
	    }
	    else{
		cmgem.ximxz = cmgem.ximnz + 0.1*cmgem.ximxz;
	    }
	}

	/* - Do the same for the y data limits. */

	if( cmgem.yimnz == cmgem.yimxz ){
	    if( cmgem.yimnz == 0. ){
		cmgem.yimxz = 0.000001;
	    }
	    else{
		cmgem.yimxz = cmgem.yimnz + 0.1*cmgem.yimxz;
	    }
	}

	/* - Calculate the input to plot coordinate mapping transformation. */

	plcalwvtrans();

	/* - Set viewport and world limits.
	 ***NOT CURRENTLY BEING USED TO DO MAPPING. STILL IN TRANSITION*** */

	setvport( cmgem.xpmnu, cmgem.xpmxu, cmgem.ypmnu, cmgem.ypmxu );
	setworld( cmgem.ximnz, cmgem.ximxz, cmgem.yimnz, cmgem.yimxz );

} /* end of function */

