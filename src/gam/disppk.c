#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ disppk(tdelay)
double tdelay;
{
	char kpktxt[9];
	int j, j_, npktxt;
	float xploc, xploc1, xploc2, xtloc, xwloc, ypdel, yploc, 
	 yploc1, yploc2, ytloc, ywloc;


	/*
	 *=====================================================================
	 * PURPOSE: To display any defined time picks in the current
	 *          subplot window.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    TDELAY:  Time offset to add to time picks before converting to plot
	 *             coordinates.  Used when files have been shifted relative
	 *             to each other in some of the more complicated plot formats.
	 *=====================================================================
	 * MODULE/LEVEL:  GAM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     FHDR, KHDR, FUNDEF, KUNDEF
	 *    LHF:     MTM, ITMKRF, ITMFNM, KFHDR
	 *    GAM:     IPKTYP(), PKWDTH, PKHGTH
	 *    GEM:     XIMN, XIMX, XPMNU, XPMXU, YPMNU, YPMXU, CHHT,
	 *             XWMNZ, XWMXZ, YWMNZ, YWMXZ, THWRAT,
	 *             LWIDTH, IWIDTH, ITHIN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  SETTEXTSIZE, LINE PLTEXT GETYW
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920603:  Added line-width kludge.
	 *    860623:  Fixed bug in computing Y values.
	 *    820318:  Added two new modes of displaying time picks.
	 *    801029:  Added display of time pick id.
	 *    800903:  Added a time delay to each time pick.
	 *             Changed name from PLPCKS to DISPPK.
	 *    800510:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return if this option has been turned off */
	if( !cmgam.ldsppk )
		return ;

	/* - Change to the smallest text size. */

	cmgem.chht = cmgam.tspk;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );

	/* - Determine length of various line segments used in pick display. */

	ypdel = cmgem.ypmxu - cmgem.ypmnu;
	yploc1 = cmgem.ypmxu - 0.05*ypdel;
	yploc2 = cmgem.ypmnu + 0.05*ypdel;

	/* - Loop on each time field in header: */

	for( j = 1; j <= MTM; j++ ){
	    j_ = j - 1;

	    /* -- If time pick is defined and pick display is not off: */
	    if( Fhdr[Itmfnm[j]] != cmhdr.fundef && Ipktyp[j] > 0 ){

		/* --- Map the input x location in WC to PC. */
		xwloc = Fhdr[Itmfnm[j]] + tdelay;
		xploc = cmgem.xmpip1*xwloc + cmgem.xmpip2;

		/* --- If time pick is within x plot window: */
		if( xploc >= cmgem.xpmnu && xploc <= cmgem.xpmxu ){
		    /* ---- Determine time pick text: either pick id (KTn) or pick name. */
		    if( memcmp(kmhdr.khdr[cmlhf.itmkrf + j_ - 1],kmhdr.kundef,
		     strlen(kmhdr.khdr[cmlhf.itmkrf + j_ - 1])) != 0 )
			strcpy( kpktxt, kmhdr.khdr[cmlhf.itmkrf + j_ - 1]);
		    else
			strcpy( kpktxt, kmlhf.kfhdr[Itmfnm[j] - 1] );
		    npktxt = indexb( kpktxt,9 );
		    /* ---- Display a horizontal line, a vertical line or a cross at pick.
		     *      Also display time pick text at appropriate location. */
		    setlinewidth( cmgem.ithin );
		    if( Ipktyp[j] == 1 ){
			setlinewidth( cmgem.iwidth );
			line( xploc, yploc1, xploc, yploc2 );
			setlinewidth( cmgem.ithin );
			pltext( kpktxt,9, xploc + 0.005, yploc2 + 0.005 );
		    }
		    else{
			xploc = cmgem.xmpip1*xwloc + cmgem.xmpip2;
			getyw( Fhdr[Itmfnm[j]], &ywloc );
			yploc = cmgem.ympip1*ywloc + cmgem.ympip2;
			xploc1 = fmax( cmgem.xpmnu, xploc - 0.5*cmgam.pkwdth );
			xploc2 = fmin( cmgem.xpmxu, xploc + 0.5*cmgam.pkwdth );
			setlinewidth( cmgem.iwidth );
			line( xploc1, yploc, xploc2, yploc );
			if( Ipktyp[j] == 3 ){
			    yploc1 = fmax( cmgem.ypmnu, yploc - 0.5*cmgam.pkhgth );
			    yploc2 = fmin( cmgem.ypmxu, yploc + 0.5*cmgam.pkhgth );
			    line( xploc, yploc1, xploc, yploc2 );
			}
			setlinewidth( cmgem.ithin );
			xtloc = xploc;
			if( (yploc - cmgem.ypmnu) > 0.5*ypdel )
			    ytloc = yploc + 0.005;
			else
			    ytloc = yploc - cmgem.chht - 0.005;
			pltext( kpktxt,9, xtloc, ytloc );
		    } /* end else associated with if ( Ipktyp[j] == 1 ) */
		} /* end if ( xploc ... ) */
	    } /* end if ( Fhdr ... ) */
	} /* end for ( j ) */

} /* end of function */

