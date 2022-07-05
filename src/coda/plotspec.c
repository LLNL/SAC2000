#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/gdm.h"
#include "../../inc/gam.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "coda.h"

void zgetgd(char* name, int name_len);


void plotspec(struct envelope *envelopes,int nbands,float C_begin,int* nerr)
{
	char kret[9];
	int lany, lframs, lwait, lxgens, lprint = FALSE ;
	int jdfl, ncret, nlcx, nlcy, nlen, notused;
	static char kwait[9] = "Waiting$";
        int ndx1, ndx2, ndxh, j, i, junk;
        float *Sacmem1, *Sacmem2;

	/* PROCEDURE: */
	*nerr = 0;

	/* - Save current values of several GEM parameters. */

	lxgens = cmgem.lxgen;
	lframs = cmgem.lframe;

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
	/* - Save plot environment. */
        jdfl = 1;
	plsave();

	cleardfl( nerr ); 

	nlen = nbands;
       	cmdfm.ndfl = 1;
	crsac( jdfl, 2, nlen, &ndxh, &ndx1, &junk, nerr );
        putfil( jdfl, nerr);
	getfil( jdfl, TRUE, &nlen, &ndx1, &ndxh, nerr );
	Sacmem1 = cmmem.sacmem[ndx1];
	Sacmem2 = cmmem.sacmem[ndxh];
	
        nlen = 0;
	for( j = 1; j <= nbands; j++ ){
          if(envelopes[j].Moment > 0.0) {
	    *(Sacmem1++) = envelopes[j].Moment;
	    *(Sacmem2++) = (envelopes[j].freq_high + envelopes[j].freq_low)/2;
	    nlen++;
	  }
	}
	*iftype = *ixy;
	*leven = FALSE;
	*npts = nlen;
	extrma( cmmem.sacmem[ndxh], 1, *npts, begin, ennd, &junk );
	extrma( cmmem.sacmem[ndx1], 1, *npts, depmin, depmax, depmen ); 
	putfil( jdfl, nerr );

	/* -- Get file from memory manager. */
	getfil( jdfl, TRUE, &nlen, &nlcy, &nlcx, nerr );
	if( *nerr != 0 )
	  goto L_8888;
	
	cmgem.lxgen = FALSE;
        cmgem.lxlim = TRUE;
        cmgem.lylim = TRUE;

	/* -- Determine x axis plot limits. */
	/*	getxlm( &cmgem.lxlim, &cmgem.ximn, &cmgem.ximx ); */
	cmgem.ximn = envelopes[1].freq_low;;
	cmgem.ximx = envelopes[nbands].freq_high;
	/* -- Determine y axis plot limits. */
	getylm( &cmgem.lylim, &cmgem.yimn, &cmgem.yimx );
        cmgem.lxlim = TRUE;
        cmgem.lylim = TRUE;
	nlen = *npts;
        cmgem.yimn = cmgem.yimn - 1.0;
	cmgem.yimx = cmgem.yimx + 1.0;
	cmgem.ximx = cmgem.ximx + 1.0;
        cmgem.ixint = cmgem.ilog; 
        cmgem.symsz = 0.015;
        cmgem.isymwidth = 1.0;
	cmgem.lwidth = TRUE;
        cmgem.isym = 14;
        setsymbolnum( cmgem.isym );
        cmgem.lsym = TRUE;
        cmgem.ltitl = TRUE;
	sprintf(kmgem.ktitl, "Moment Rate Spectrum");
        cmgem.ntitl = (strcspn(kmgem.ktitl," ") + 1);
        cmgem.lylab = TRUE;
	sprintf(kmgem.kylab, "Log Moment (dyne-cm)");
        cmgem.nylab = (strcspn(kmgem.kylab," ") + 1);
        cmgem.lxlab = TRUE;
	sprintf(kmgem.kxlab, "Frequency (Hz)");
        cmgem.nxlab = (strcspn(kmgem.kxlab," ") + 1);
        cmgem.igtfnt=7;
	setvspacetype( FALSE, 1.0 );
        setsymbolsize( cmgem.symsz);
	//	fprintf(stderr,"limits are: %f %f \n",cmgem.ximn,cmgem.ximx);	

	beginframe( lprint , nerr );
	if( *nerr != 0 )
	  goto L_8888;
	getvspace( &cmgem.xvspmn, &cmgem.xvspmx, &cmgem.yvspmn, 
		   &cmgem.yvspmx );

	/* -- Plot the data.  Do not allow PL2D to perform framing. */
	cmgem.lframe = FALSE;
       	pl2d( cmmem.sacmem[nlcx], cmmem.sacmem[nlcy], nlen, 1, 1, nerr );
	if( *nerr != 0 )
	  goto L_8888;

	flushbuffer( nerr );
	/* -- Plot the frame id, pick display and home cursor.
	dispid( cmgam.lfinorq , jdfl );	
	disppk( 0. );
	plhome();

	if( *nerr != 0 )
	  goto L_8888;
	*/
	
	/* - Restore GEM parameters before returning. */

L_8888:
	lwait = FALSE;
	cmgem.lxgen = lxgens;
	cmgem.lframe = lframs;
	plrest();
	return;

} /* end of function */

