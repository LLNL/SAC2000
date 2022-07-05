#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
#include "../../inc/gdm.h"

void specplot(specdata,nx,ny,xmin,xmax,ymin,ymax,ywmin,ywmax,imagetype,lbinary,lcbar,lprint, nerr)
float *specdata;
int nx, ny;
float xmin, xmax, ymin, ymax, ywmin, ywmax;
char *imagetype;
int lbinary;
int lcbar;
int lprint ;
int *nerr;
{
	char kptext[MCMSG+1], kret[9];
	int l1dttm, lany, lbotaxsave, lbottcsave, lframesave, ltitlsave, 
	 ltoptcsave, lwait, lxgrdsave, lxlabsave, lxlims, lylabsave;
	int n1dttm[6], nlcx, nlcy, num;
        int nystart, nypoints;
        unsigned int width_return, height_return, image_width, image_height, xloc, yloc, cbarxoffset, cbaryoffset;
	float tmax, tmaxj, tmin, tminj, toff[MDFL], ypdel, ypmxsave, ypmnsave, xpmxsave, xpmnsave;
        float vspaceratio, vportratio;
	void zgetgd(), zgpmsg();
	static int lrel = FALSE;
	static int lperpl = FALSE;
	static int nperpl = 3;
	static char kzref[9] = "Z       ";
	static char kwait[9] = "Waiting$";

	int *const N1dttm = &n1dttm[0] - 1;
	float *const Toff = &toff[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To produce a spectrogram plot.  This routine plots the
	 *         input trace in a small window above a spectrogram, which
         *         is either a color image plot, a greyscale plot or a contour
         *         plot.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred.
	 *             Potential error numbers:  1001, 1504.
	 *=====================================================================
	 * MODULE/LEVEL:  gam/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ndfl, sacmem
	 *    hdr:     begin, ennd, delta
	 *    gem:     lbotax, lbottc, ltopax, ltoptc, lxlab, lylab, ltitl,
	 *             lxgrd, ypmn, ypmx, chht, tsdef
	 *    gam:     kgddef
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970130:  Added arguments to dispid() not to plot file number. maf
         * 940825:  Original version, adapted from xp1.
         *
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */
	/* PROCEDURE: */
	/* Errors before plsave have to avoid going to execute plrest. */
	*nerr = 0;

	/* - If no graphics device is open, try to open the default device. */

	getstatus( "ANY", &lany );
	if( !lany ){
		zgetgd( kmgam.kgddef,9 );
		begindevices( kmgam.kgddef,9, 1, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

        if( cmgam.cmap == MDEFAULT){
          setpsctable(nerr);
          if( *nerr != 0 ) goto L_8888;
	}

        if( (cmgam.cmap == MCOLOR) && (strcmp(imagetype,"grey") == 0) ){
          changectable(cmgdm.nctsize+1,MGREY);
          cmgam.cmap = MGREY;
        }
        else if((cmgam.cmap == MGREY) && (strcmp(imagetype,"color") == 0)){
          changectable(cmgdm.nctsize+1,MCOLOR);
          cmgam.cmap = MCOLOR;
        }

	/* EXECUTION PHASE: */

	/* - Save current plot and x limit attributes.
	 * - Error after plsave have to go to execute plrest. */

	plsave();


	/* - Set up specific options that apply only to this plot. */

	lbotaxsave = cmgem.lbotax;
	lbottcsave = cmgem.lbottc;
	ltoptcsave = cmgem.ltoptc;
	cmgem.lbottc = FALSE;
	cmgem.lbotax = FALSE;

	lxlabsave = cmgem.lxlab;
	lylabsave = cmgem.lylab;
	ltitlsave = cmgem.ltitl;
	lxgrdsave = cmgem.lxgrd;
	cmgem.lxlab = FALSE;
	cmgem.lylab = FALSE;
	cmgem.ltitl = FALSE;
	cmgem.lxgrd = FALSE;

	/* - Set up y window for seismogram. */
        /* - Use the upper 20% of the viewport. */
	ypdel = (cmgem.ypmx - cmgem.ypmn)/ 5.0;

	/* - Check WAIT option.  This is on when:
	 * -- A wait request has been made.
	 * -- An active device (normally the user's terminal) is on. */

	if( cmgam.lwaitr ){
		getstatus( "ACTIVE", &lwait );
		}
	else{
		lwait = FALSE;
		}

        /* plot the first input file */

	ypmxsave = cmgem.ypmx;
        ypmnsave = cmgem.ypmn;
        xpmxsave = cmgem.xpmx;
        xpmnsave = cmgem.xpmn;

	lframesave = cmgem.lframe;
	cmgem.lframe = FALSE;

		/* -- Determine time limits for x axis of this frame.
		 *    (Correct for any differences in GMT reference time.) */

	getfil( 1, TRUE, &num, &nlcy, &nlcx, nerr );
      	if( *nerr != 0 )goto L_7777;

	getxlm( &lxlims, &tmin, &tmax );
       	if( lrel ){
         	tmax = tmax - tmin;
	       	Toff[1] = -tmin;
	       	tmin = 0.;
      	}
	else{
	       	copyi( nzdttm, n1dttm, 6 );
	       	l1dttm = ldttm( n1dttm );
	       	Toff[1] = 0.;
       	}

	/* - Check range of time limits to avoid errors that could occur
	 *   later during plotting. */

	if( fabs( tmax - tmin ) > (float)( MLARGE ) ){
      		*nerr = 1504;
       		setmsg( "ERROR", *nerr );
       		goto L_7777;
	}

		/* - Set x axis plot limits. */

       	cmgem.lxlim = TRUE;
       	cmgem.ximn = tmin;
       	cmgem.ximx = tmax;

       	if( lframesave ){
       		beginframe( lprint , nerr );
       		if( *nerr != 0 )
       			goto L_7777;
       		getvspace( &cmgem.xvspmn, &cmgem.xvspmx, &cmgem.yvspmn, 
			 &cmgem.yvspmx );
	}


       	cmgem.tsdef = fmin( cmgem.tsdef, (cmgem.yvspmx - cmgem.yvspmn)/(8.0*5.0 ));
       	cmgam.tsfid = cmgem.tsdef;
       	cmgam.tspk = cmgem.tsdef;
       	cmgem.tsaxis = cmgem.tsdef;

     	cmgem.ypmn = cmgem.ypmx - ypdel;

        /* --- Get pointers to this file's location in memory. */

     	getfil( 1, TRUE, &num, &nlcy, &nlcx, nerr );
	if( *nerr != 0 )goto L_7777;

        /* --- Set up x axis data values. */

	if( *leven ){
		cmgem.lxgen = TRUE;
	      	cmgem.xdelta = *delta;
	       	cmgem.xfirst = *begin + Toff[1];
	}else{
	      	cmgem.lxgen = FALSE;
       	}

       	/* --- Set up y axis plot limits. */

       	getylm( &cmgem.lylim, &cmgem.yimn, &cmgem.yimx );

       	/* --- Plot this file. */

       	pl2d( cmmem.sacmem[nlcx], cmmem.sacmem[nlcy], num, 1, 1, nerr );
       	if( *nerr != 0 )goto L_7777;

       	/* --- Plot picks and fileid. */

       	disppk( Toff[1] );
       	dispid( 0 , 0 );	/* added arguments.  maf 970130 */

       	/* --- Add a label with offset time if this is a REL plot. */

       	if( lrel && cmgam.lfidrq ){
               sprintf(kptext,"OFFSET: %10.3e", -Toff[1] );
	       cmgem.chht = cmgem.tsdef;
	       cmgem.chwid = cmgem.txrat*cmgem.chht;
	       settextsize( cmgem.chwid, cmgem.chht );
	       settextangle( cmgem.horz );
	       pltext( kptext,MCMSG+1, cmgam.xfidlc, cmgam.yfidlc );
	       cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
	}

		/* -- Draw bottom x axis. */
       	cmgem.lbotax = lbotaxsave;
       	cmgem.lbottc = lbottcsave;
       	cmgem.ltoptc = ltoptcsave;
       	cmgem.lxgrd = lxgrdsave;
       	cmgem.ypmxu = ypmxsave*cmgem.yvspmx;
       	cmgem.chht = cmgem.tsaxis;
       	cmgem.chwid = cmgem.txrat*cmgem.chht;
       	settextsize( cmgem.chwid, cmgem.chht );

	xlinax();

       	/* -- Draw axes labels and title. */
       	if( lxlabsave )
       		centxt( kmgem.kxlab,145, cmgem.nxlab, cmgem.ixlabp, cmgem.tsxlab );
       	if( lylabsave )
       		centxt( kmgem.kylab,145, cmgem.nylab, cmgem.iylabp, cmgem.tsylab );
       	if( ltitlsave )
       		centxt( kmgem.ktitl,145, cmgem.ntitl, cmgem.ititlp, cmgem.tstitl );

        calcsize(&width_return, &height_return, &image_width, &image_height, xmax, xmin, *ennd, cmgem.xfirst,
                               cmgem.xpmn, cmgem.xpmx, ypmnsave, cmgem.ypmx, 0.7, nerr);

        getratio(&vspaceratio);
        calcloc(&xloc, &yloc,&cbarxoffset,&cbaryoffset,width_return,height_return,image_width,image_height,cmgem.xpmn,cmgem.xpmx,xmin,
                               cmgem.xfirst,*ennd,ypmnsave,ypdel,vspaceratio,nerr);
 
        nypoints = (ywmax/ymax)*(float)ny;
        nystart = ((ywmin/ymax)*(float)ny) + 1;

        plotimage(specdata,nx,xmin,xmax,ny,ymin,ymax,1,nx,nystart,nypoints,image_width,image_height,
                  width_return,height_return,xloc,yloc,cbarxoffset,cbaryoffset,(1.0 - (ypmnsave + ypdel)),lbinary,lcbar,nerr);
        if( *nerr != 0 )goto L_7777;

        cmgem.xpmn = (cmgem.xpmn + (((xmin-cmgem.xfirst)/(*ennd-cmgem.xfirst))*(cmgem.xpmx-cmgem.xpmn)));
        cmgem.xpmx = cmgem.xpmn+((float)image_width/(float)width_return);
        cmgem.ypmx = 1.0 - (ypmnsave + ypdel);
        cmgem.ypmn = cmgem.ypmx - (float)((float)image_height/(float)height_return)- RNDOFF;

        cmgem.ximn = xmin;
        cmgem.ximx = xmax;
        cmgem.yimn = ywmin;
        cmgem.yimx = ywmax;
        cmgem.ximnz = xmin;
        cmgem.ximxz = xmax;
        cmgem.yimnz = ywmin;
        cmgem.yimxz = ywmax;

        cmgem.xpmnu = cmgem.xpmn;
        cmgem.xpmxu = cmgem.xpmx;
        cmgem.ypmnu = cmgem.ypmn*vspaceratio;
        cmgem.ypmxu = cmgem.ypmx*vspaceratio;

        setvport(cmgem.xpmnu,cmgem.xpmxu,cmgem.ypmnu,cmgem.ypmxu);

        plcalwvtrans();
        
        plgrid(nerr);

       	/* -- Home cursor, advance frame. */

       	plhome();
        flushbuffer(nerr);

       	if( lframesave )
			endframe( FALSE , nerr );


L_7777:
	plrest();
	cmgam.tsfid = cmgem.tsdef;
	cmgam.tspk = cmgem.tsdef;
	cmgem.tsaxis = cmgem.tsdef;

	cmgem.ypmx = ypmxsave;
        cmgem.ypmn = ypmnsave;
        cmgem.xpmx = xpmxsave;
        cmgem.xpmn = xpmnsave;

	cmgem.lbotax = lbotaxsave;
	cmgem.lbottc = lbottcsave;
	cmgem.lframe = lframesave;

L_8888:
	return;
} /* end of function */

