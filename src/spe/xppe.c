#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/spe.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ xppe(nerr)
int *nerr;
{
	int lany , lframs ;
	int ifidls;
	float xjunk;

	/*=====================================================================
	 * PURPOSE:  To execute the action command PPE.
	 *           This command plots the prediction error vs frequency.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag.  Set to 0 if no error occurred.
	 *             Potential error numbers:  5003
	 *=====================================================================
	 * MODULE/LEVEL:  SPE/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    SPE:     LCOR, NLNCOR, NDXCOR, NDXPE, MLNPE
	 *    GAM:     IFIDLC, IUR
	 *    GEM:     LFRAME, LTOPAX, LBOTAX, LRIGAX, LLEFAX, LTOPTC, LBOTTC,
	 *             LRIGTC, LLEFTC, LTITLE, LXLAB, KXLAB,NXLAB, IXLAPB, IBOT,
	 *             IXLABS, ITINY, LYLAB, KYLAB, NYLAB, IYLABP, ILEFT, IYLABS,
	 *             LXGEN, XFIRST, XDELTA
	 *    DFM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM, GETSTATUS, BEGINDEVICES, PLSAVE, CRIT,
	 *             BEGINFRAME(NERR), PL2D, DISPID, ENDFRAME(NERR), PLREST
	 *=====================================================================
	 * LOCAL VARIABLES
	 *    IFIDLS:  Used to save and restore fileid attribute. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* CHECKING PHASE: */

	/* - Make sure correlation function has been calculated. */

	if( !cmspe.lcor ){
	    *nerr = 5003;
	    setmsg( "ERROR", *nerr );
	    goto L_8888;
	}

	/* - If no graphics device is open, try to open the default. */

	getstatus( "ANY", &lany );
	if( !lany ){
	    begindevices( kmgam.kgddef,9, 1, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* EXECUTION PHASE: */

	/* - Save plot environment */

	plsave();
	ifidls = cmgam.ifidlc;

	/* - Set up specific options for this plot */

	cmgam.ifidlc = cmgam.iur;
	cmgam.ldsppk = FALSE;
	lframs = cmgem.lframe ;
	cmgem.lframe = FALSE;
	cmgem.ltopax = FALSE;
	cmgem.lbotax = TRUE;
	cmgem.lrigax = FALSE;
	cmgem.llefax = TRUE;
	cmgem.ltoptc = TRUE;
	cmgem.lbottc = TRUE;
	cmgem.lrigtc = TRUE;
	cmgem.lleftc = TRUE;
	cmgem.ltitl = FALSE;
	cmgem.lxlab = TRUE;
	fstrncpy( kmgem.kxlab, 144, "Lag Number", 10 );
	cmgem.nxlab = 10;
	cmgem.ixlabp = cmgem.ibot;
	cmgem.lylab = TRUE;
	fstrncpy( kmgem.kylab, 144, "RMS Prediction Error", 20 );
	cmgem.nylab = 20;
	cmgem.iylabp = cmgem.ileft;

	/* - Set up x axis parameters. */

	cmgem.lxgen = TRUE;
	cmgem.xfirst = 1.;
	cmgem.xdelta = 1.;

	/* - Calculate prediciton error. */

	crit( cmmem.sacmem[cmspe.ndxcor], cmspe.nlncor, cmmem.sacmem[cmspe.ndxpe] );

	/* - Plot prediction error vs lag number. */

	if ( lframs )
	    beginframe( FALSE , nerr );
	pl2d( (float*)&xjunk, cmmem.sacmem[cmspe.ndxpe], MLNPE, 1, 1, nerr );
	if( *nerr != 0 )
	    goto L_7777;

	/* - Plot the file id. */

	dispid( 0 , 0 );	/* added arguments.  maf 970130 */

	/* - End plot and restore plot environment. */

L_7777:
	plhome();
	if ( lframs )
	    endframe( FALSE , nerr );
	plrest();
	cmgam.ifidlc = ifidls;

L_8888:
	cmgem.lframe = lframs ;
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
         *    970130:  Added arguments to dispid() not to plot file number. maf
	 *    841227:  Changes due to major rewrite of SPE subprocess.
	 *    821130:  Changed name from PPECN and made minor mods.
	 *    810120:  Changed to output message retrieval from disk.
	 *    801024:  Added a plot id.
	 *    800925:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850109
	 *===================================================================== */

} /* end of function */

