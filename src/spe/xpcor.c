#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "gem.h"
#include "gdm.h"
#include "gam.h"
#include "spe.h"



void zgetgd(char* name,int name_len);
void /*FUNCTION*/ xpcor(nerr)
int *nerr;
{
	char kcorlb[ 17 ] ;
	int lany , lpcsec , lframs , lprint = FALSE , ltry = FALSE ;
	int ifidls , nlnplt , notused ;
	float pcsec , xjunk ;


	/*=====================================================================
	 * PURPOSE: To parse and execute the action command PCOR.
	 *          This command plots the cross-correlation function.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  SPE/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL, SACMEM
	 *    GAM:     IFIDLC, IUR, FIDBDR, XFIDLC
	 *    GEM:     LFRAME, LTOPAX, LBOTAX, LRIGAX, LLEFAX, LTOPTC, LBOTTC,
	 *             LRIGTC, LLEFTC, LTITLE, LXLAB, KXLAB,NXLAB, IXLAPB, IBOT,
	 *             IXLABS, ITINY, LYLAB, KYLAB, NYLAB, IYLABP, ILEFT, IYLABS,
	 *             LXGEN, XFIRST, XDELTA, 'RIGHT', 'BOTTOM'
	 *    SPE:     LCOR, NLNCOR, SAMFRQ, NDXCOR, LSPEID
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GAM:     YFIDLC
	 *    SPE:     LPCSEC, PCSEC
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCRRC, GTOUTM, GETSTATUS, BEGINDEVICES,
	 *             PLSAVE, BEGINFRAME(NERR), PL2D, DISPID, PLHOME, ENDFRAME(NERR), PL
	 *             PLREST, SETTEXTJUST
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NLNPLT:  Number of lags of correlation function to plot. [i]
	 *    IFIDLS:  Used to save and restore fileid attribute. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){
	    /* -- "XLIM ON|OFF|v":  change width (number of seconds) of plot. */
	    if( lklogr( "XLIM$",6, &lpcsec, &pcsec ) )
	    { /* do nothing */ }

            /* if PRINT option is tried, get printer name */
            else if ( ltry ) {
                lcchar ( MAXPRNTRNAMELEN   , kmgem.kptrName ,
                         MAXPRNTRNAMELEN+1 , &notused ) ;
                terminate ( kmgem.kptrName ) ;
                if ( !lprint )
                    kmgem.kptrName[0] = '\0' ;

                ltry = FALSE ;
            }

            /* -- "PRINT":  print the final product */
            else if( lckey( "PRINT#$", 8 ) ) {
                ltry = TRUE ;
                if ( cmgdm.lbegf ) {
                    setmsg ( "WARNING" , 2403 ) ;
                    outmsg () ;
                    clrmsg () ;
                }
                else {
                    lprint = TRUE ;
                }
            }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
	    goto L_8888;

	/* - CHECKING PHASE: */

	/* - Make sure correlation function has been calculated. */

	if( !cmspe.lcor ){
	    *nerr = 5003;
	    setmsg( "ERROR", *nerr );
	    goto L_8888;
	}

	/* - If no graphics device is open, try to open the terminal. */

	getstatus( "ANY", &lany );
	if( !lany ){
	    zgetgd( kmgam.kgddef,9 );
	    begindevices( kmgam.kgddef,9, 1, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - EXECUTION PHASE: */

	/* - Compute the number of lags to plot. */

	if( lpcsec )
	    nlnplt = min( cmspe.nlncor/2 + 1, (int)( pcsec*cmspe.samfrq ) );
	else
	    nlnplt = cmspe.nlncor/2 + 1;

	/* - Save plot environment */

	plsave();
	ifidls = cmgam.ifidlc;

	/* - Set up specific options for this plot */

	cmgam.ifidlc = cmgam.iur;
	cmgam.xfidlc = cmgam.fidbdr;
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
	fstrncpy( kmgem.kxlab, 144, "Time (seconds)", 14 );
	cmgem.nxlab = 14;
	cmgem.ixlabp = cmgem.ibot;
	cmgem.lylab = TRUE;
	fstrncpy( kmgem.kylab, 144, "Correlation Function", 20 );
	cmgem.nylab = 20;
	cmgem.iylabp = cmgem.ileft;

	/* - Set up x axis parameters. */

	cmgem.lxgen = TRUE;
	cmgem.xfirst = 0.;
	cmgem.xdelta = 1./cmspe.samfrq;

	/* - Plot correlation function vs lag number. */

	if ( lframs )
	    beginframe( lprint , nerr );
	pl2d( (float*)&xjunk, cmmem.sacmem[cmspe.ndxcor], nlnplt, 1, 1, nerr );
	if( *nerr != 0 )
	    goto L_7777;

	/* - Generate file id.  Add the SPE id below it. */

	dispid( 0 , 0 );	/* added arguments.  maf 970130 */
	settextjust( "LEFT", "BOTTOM" );
	if( cmspe.lspeid ){
	    /* -- Blank line between file id and SPE id. */
	    cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;

	    /* -- Number of correlation windows. */
            sprintf(kcorlb,"NUMBER %3d", cmspe.numwin );
	    pltext( kcorlb,17, cmgam.xfidlc, cmgam.yfidlc );
	    cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;

	    /* -- Length of correlation window */
            sprintf(kcorlb,"LENGTH %5.1f", cmspe.winlen );
	    pltext( kcorlb,17, cmgam.xfidlc, cmgam.yfidlc );
	    cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;

	    /* -- Type of correlation window: */
            sprintf(kcorlb,"TYPE %s", kmspe.kwintp[cmspe.iwncor - 1]);
	    pltext( kcorlb,17, cmgam.xfidlc, cmgam.yfidlc );
	    cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;

	    /* -- Prewhitening flag. */
	    if( cmspe.nprewh > 0 )
                sprintf(kcorlb,"PREWHITEN %3d", cmspe.nprewh );
	    else
		strcpy( kcorlb, "PREWHITEN OFF   " );

	    pltext( kcorlb,17, cmgam.xfidlc, cmgam.yfidlc );
	    cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
	} /* end if ( cmspe.lspeid ) */

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
	 *    821130:  Combined PCORPS and PCORCN.
	 *             Changed to new command parsing logic.
	 *    810120:  Changed to output message retrieval from disk.
	 *    801211:  Changes and additions for plot id.
	 *    801024:  Added plot id.
	 *    801001:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850109
	 *===================================================================== */

} /* end of function */

