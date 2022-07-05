#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "gtm.h"
#include "contouring.h"
#include "gem.h"
#include "gam.h"
#include "gdm.h"
#include "hdr.h"
#include "mem.h"
#include "xyz.h"

void ximage(nerr)
int *nerr;
{
	char kret[9];
	int lany, lframesave, lwait, lxlimits, lylimits,
	     lprint = FALSE , ltry = FALSE ;
	int ixstart, ixstop, iystart, iystop, jfile, ncret, 
	 ndxz, nfiles, nlen, notused, nxsize, nysize;
        int jxstart, jxstop, jystart, jystop;
	float vportratio, vspaceratio, xmaximum, xminimum, xstart, xstop, 
	 ymaximum, yminimum, ystart, ystop;
        unsigned int image_width, image_height;
        unsigned int width_return, height_return;
        unsigned int border_width_return;
        unsigned int xloc, yloc, cbarxoffset, cbaryoffset;
	void zgpmsg();
	static char kwait[9] = "Waiting$";
        static char imagetype[6] = "color";
        static int lbinary = FALSE;
        static int lcbar = TRUE;

	/*=====================================================================
	 * PURPOSE:  To execute the action command IMAGE
	 *           This command creates an image plot of xyz data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  xyz/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gem:     lframe, xpmn, xpmx, ypmn, ypmx
	 *    gam:     kgddef, lwaitr, lwaite
	 *    mem:     sacmem
	 *    xyz:     lzllist, zllist, nzllist, lzlmin, zlmin, lzlmax, zlmax,
	 *             lzlines, nzlines, izlines, nzregions, zregions
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     lframe, ximn, ximx, yimn, yimx, ximnz, ximxz, yimnz, yimxz, 
	 *             xpmn, xpmx, ypmn, ypmx, xpmnu, xpmxu, ypmnu, ypmxu
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     vflist, vfxyz, getstatus, begindevice, getfil, getxlm,
	 *             indexb, beginframe, SetContDataMode, PlotContData, 
	 *             dispid, plhome, endframe, zgpmsg, upcase
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kwait:   Message sent to terminal when in wait mode.
	 *    kret:    Message received from terminal when in wait mode.
	 *    nlcz:    Location in SACMEM array of each file's z data.
	 *    nlen:    Length of each data file.
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970130:  Added arguments to dispid() not to plot file number. maf
	 *    901205:  Added text font selection from GTEXT command. 
	 *    900409:  Added coding so that XLIM and YLIM now set data limits.
	 *    900305:  Original version based upon XP.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900305
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- ASPECT ON|OFF:  maintain aspect ratio of data or not. */
	    if( lklog( "ASPECT$",8, &cmxyz.laspect ) )
	    { /* do nothing */ }

	    else if( lklog( "CBAR$",8, &lcbar ) )
	    { /* do nothing */ }

	    /* -- Color image    */
            else if(lckey("C#OLOR$",8)) {
		strcpy(imagetype,"color");
	    }

	    /* -- Greyscale image */
            else if(lckey("G#REY$",7))  {
		strcpy(imagetype,"grey");
	    }

            else if(lckey("G#RAY$",7))  {
		strcpy(imagetype,"grey");
	    }

	    else if( lclog2( "BINARY$",6, "FULL$",8, &lbinary ) )
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
		else if ( Lgdon[1] || Lgdon[3] || Lgdon[4] || Lgdon[5] || 
			 !Lgdon[2] ) {
		    setmsg ( "WARNING" , 2404 ) ;
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

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Check to make sure all files are xyz data files. */

	vfxyz( nerr );
	if( *nerr != 0 ){
	    aplmsg( "Must be XYZ data files to use this command.",44 );
	    goto L_8888;
	}

	/* - If no graphics device is open, try to open the default device. */

	getstatus( "ANY", &lany );
	if( !lany ){
	    begindevice( kmgam.kgddef,9, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* load the pseudocolormap if necessary */

	if( cmgam.cmap == MDEFAULT){
	    setpsctable(nerr);
	    if( *nerr != 0 ) goto L_8888;
	}

	if( (cmgam.cmap == MCOLOR) && (strcmp(imagetype,"grey") == 0) ){
	    /* load the greyscale color table */
	    changectable(cmgdm.nctsize+1,MGREY);
	    cmgam.cmap = MGREY;
	}
	else if((cmgam.cmap == MGREY) && (strcmp(imagetype,"color") == 0)){
	    /* load the full color table */
	    changectable(cmgdm.nctsize+1,MCOLOR);
	    cmgam.cmap = MCOLOR;
	}

	/* EXECUTION PHASE: */

	/* - Save current values of several GEM parameters. */

	lframesave = cmgem.lframe;
	plsave();

        setcolor(cmgdm.nctsize);

	/* - Check WAIT option.  This is on when:
	 * -- A wait request has been made.
	 * -- An active device (normally the user's terminal) is on. */

	if( cmgam.lwaitr ){
	    getstatus( "ACTIVE", &lwait );
	}
	else{
	    lwait = FALSE;
	}

	/* - For each file in DFL: */

	getnfiles( &nfiles );
	for( jfile = 1; jfile <= nfiles; jfile++ ){
	    /* -- Get file from memory manager. */
	    getfil( jfile, TRUE, &nlen, &ndxz, &notused, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Get needed header information. */
	    getnhv( "NXSIZE", &nxsize, nerr , 6 );
	    if( *nerr != 0 )
		goto L_8888;
	    getnhv( "NYSIZE", &nysize, nerr , 6 );
	    if( *nerr != 0 )
		goto L_8888;
	    getfhv( "XMINIMUM", &xminimum, nerr, 8 );
	    if( *nerr != 0 )
		goto L_8888;
	    getfhv( "XMAXIMUM", &xmaximum, nerr, 8 );
	    if( *nerr != 0 )
		goto L_8888;
	    getfhv( "YMINIMUM", &yminimum, nerr, 8 );
	    if( *nerr != 0 )
		goto L_8888;
	    getfhv( "YMAXIMUM", &ymaximum, nerr, 8 );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Get requested x and y data limits and convert to data indices. */
	    getxlm( &lxlimits, &xstart, &xstop );
	    if( lxlimits ){
		cmgem.xdelta = (xmaximum - xminimum)/(float)( nxsize - 1 );
		ixstart = max( 1, (int)( (xstart - xminimum)/cmgem.xdelta ) + 1 );
		ixstop = min( nxsize, (int)( (xstop - xminimum)/cmgem.xdelta ) + 2 );
		xstart = xminimum + cmgem.xdelta*(float)( ixstart - 1 );
		xstop = xminimum + cmgem.xdelta*(float)( ixstop - 1 );
	    }
	    else{
		ixstart = 1;
		ixstop = nxsize;
		xstart = xminimum;
		xstop = xmaximum;
	    }
	    getylm( &lylimits, &ystart, &ystop );
	    if( lylimits ){
		cmgem.ydelta = (ymaximum - yminimum)/(float)( nysize - 1 );
		iystart = max( 1, (int)( (ystart - yminimum)/cmgem.ydelta ) + 1 );
		iystop = min( nysize, (int)( (ystop - yminimum)/cmgem.ydelta ) + 2 );
		ystart = yminimum + cmgem.ydelta*(float)( iystart - 1 );
		ystop = yminimum + cmgem.ydelta*(float)( iystop - 1 );
	    }
	    else{
		iystart = 1;
		iystop = nysize;
		ystart = yminimum;
		ystop = ymaximum;
	    }

	    setcontdatalim( ixstart, ixstop, iystart, iystop );

	    /* -- Set PL/GEM world coordinates limits. */
	    cmgem.ximn = xstart;
	    cmgem.ximx = xstop;
	    cmgem.yimn = ystart;
	    cmgem.yimx = ystop;
	    cmgem.ximnz = xstart;
	    cmgem.ximxz = xstop;
	    cmgem.yimnz = ystart;
	    cmgem.yimxz = ystop;

	    /* -- Begin next plot frame if requested. */
	    if( lframesave ){
		beginframe( lprint , nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    /* -- Rectify old PL/GEM viewport values and current graphics library ones. */
	    getratio( &vspaceratio );
	    cmgem.xpmnu = cmgem.xpmn;
	    cmgem.xpmxu = cmgem.xpmx;
	    cmgem.ypmnu = cmgem.ypmn*vspaceratio;
	    cmgem.ypmxu = cmgem.ypmx*vspaceratio;
	    setvport( cmgem.xpmnu, cmgem.xpmxu, cmgem.ypmnu, cmgem.ypmxu );

	    /* -- Set viewport border and viewport ratio. */
	    if( cmxyz.laspect ){
		vportratio = fabs( (ystop - ystart)/(xstop - xstart) );
		setvportratio( vportratio );
		getvport( &cmgem.xpmnu, &cmgem.xpmxu, &cmgem.ypmnu, &cmgem.ypmxu );
	    }

	    /* -- Set the text font from gtext parameter */
	    settextfont( cmgem.igtfnt );

	    calcsize(&width_return, &height_return, &image_width, &image_height,
	      xmaximum, xminimum, xmaximum, xminimum, cmgem.xpmn, cmgem.xpmx,
	      cmgem.ypmn, cmgem.ypmx, 1.0, nerr);

	    calcloc(&xloc, &yloc, &cbarxoffset, &cbaryoffset, width_return,
	      height_return, image_width, image_height, cmgem.xpmn, cmgem.xpmx,
	      xminimum, xminimum, *ennd, 1.0-cmgem.ypmx, 0.0, vspaceratio, nerr);

	    yloc += 1; /* kludged because calcloc returns yloc as 1 less than it should for image */

	    jxstart = (cmcontouring.ixdatastart <= 0) ? 1      : cmcontouring.ixdatastart;
	    jxstop  = (cmcontouring.ixdatastop  <= 0) ? nxsize : cmcontouring.ixdatastop;
	    jystart = (cmcontouring.iydatastart <= 0) ? 1      : cmcontouring.iydatastart;
	    jystop  = (cmcontouring.iydatastop  <= 0) ? nysize : cmcontouring.iydatastop;

	    /* -- Create an image plot of the data. */
	    plotimage(cmmem.sacmem[ndxz], nxsize, xminimum, xmaximum, nysize,
	      yminimum, ymaximum, jxstart, jxstop, jystart, jystop, image_width,
	      image_height, width_return, height_return, xloc, yloc, cbarxoffset,
	      cbaryoffset, cmgem.ypmx, lbinary, lcbar, nerr);
	    if( *nerr != 0 ) 
		goto L_8888;

	    /* -- Plot the axes, labels andframe id; home cursor. */
	    plcalwvtrans();
	    plgrid( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    dispid( 0 , 0 );	/* added arguments. maf 970130 */

	    plhome();
	    flushbuffer( nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- End current plot frame if requested. */
	    if( lframesave )
		endframe( FALSE , nerr );

	    /* -- Wait for user prompt before plotting next frame if appropriate. */
	    if( jfile == nfiles && !cmgam.lwaite )
			lwait = FALSE;
	    if( lwait ){
		zgpmsg( kwait,9, kret,9 );
		ncret = indexb( kret,9 );
		upcase( kret, ncret, kret,9 );
		if( kret[0] == 'K' )
		    goto L_8888;
		if( kret[0] == 'G' )
		    lwait = FALSE;
	    }
	}

	/* - Restore GEM parameters before returning. */

L_8888:
	cmgem.lframe = lframesave;
	plrest();
	return;

} /* end of function */

