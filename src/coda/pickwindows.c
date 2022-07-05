#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MWIN	5

#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gem.h"
#include "../../inc/eam.h"
#include "../../inc/gam.h"
#include "coda.h"

void pickwindows(struct envelope *envelopes,int nbands,float C_begin,int* nerr)
{
	char _c0[2], kmsg[MCMSG+1], knlocs[9], 
	 kptext[MCMSG+1], kundrt[9], kxloc[17], kyloc[17];
	char ktemp[MCMSG+7];	/* increased array size for jdfl.  maf 970130 */
	int lany, lempty, lhlwrt[MDFL], lppkab, lrdttm, ltitls, 
	 lwfok, lxlims, lzdttm[MDFL];
	byte kchar;
	int lfirst, lxlimj, lylimj,num, num1, num2, num2m1, jx, jy, ic1, ic2, nc;
	float atrwid, fjunk, slen, slenm, ximnj, ximxj, xjunk, 
	 xlinl1, xlinl2, xsymlc, yatrlc, yimnj, yimxj;
	int iwf[5], jdx, jdfl, jdfl1, jdfl2, ndfl,
	 jdfls, jfr, jhdr1, jhdr2, jhour, jjday, jmark, jmark1, jmark2, 
	 jmin, jmsec, jofset, jsec, jwin, jyear, ncerr, ndxpk, ndxx, ndxy, 
	 nexday, nfr, nlcx, nlcy, nlen, nln, nlncda, nperfr, npmark, npmsec, 
	 npsec, nrdttm[6], nsavelast, nst, unused ;
	float amplmn, amplmx, facc, fsecsi, prl, psecsi, seccur, 
	 secinc, ssecsi, time, tmax, tmaxj, tmin, tminew, tminj, to, toff[MDFL], 
	 tref1, twin[MWIN][2], xlf, xloc, xloc1, xloc2, xlocs1, xlocs2, 
	 xtpos, yimnzs[MDFL], yimxzs[MDFL], yloc, ypdel, ypdelv, ypmns, 
	 ypmnv, ypmxs, ypmxus, ypmxv, ytpos;
	void zgetgd();
	static char kndate[25] = "                        ";
	static char kntime[17] = "                ";
	static int lint = FALSE;
	static int lnewxw = FALSE;
	static byte kdir = 'U';
	static byte ktype = 'I';
	static byte kqual = '0';
	static int lhltrm = FALSE;
	static int lhlhyp = FALSE;
	static int nsavelocs = 0;
        char *cattemp, *cattemp1;


	int *const Iwf = &iwf[0] - 1;
	int *const Lhlwrt = &lhlwrt[0] - 1;
	int *const Lzdttm = &lzdttm[0] - 1;
	float *const Toff = &toff[0] - 1;
	float *const Yimnzs = &yimnzs[0] - 1;
	float *const Yimxzs = &yimxzs[0] - 1;

        extern int bellON;
        int ndx1, ndx2, ndxh, j, junk;
        float *Sacmem1, *Sacmem2;
        float *Sacmem;

	/* PROCEDURE: */
	
	*nerr = 0;
        bellON = TRUE;

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

	plsave();

	/* - Temporarily turn on cursor graphics device only. */

	cursoron();
	/* - Set up specific options for this plot. */
	cmgem.llefax = TRUE;
	cmgem.lrigax = FALSE;
	cmgem.ltopax = FALSE;
	cmgem.lleftc = TRUE;
	cmgem.lrigtc = TRUE;
	cmgem.ltoptc = TRUE;
	ltitls = cmgem.ltitl;
	cmgem.ypmx = 0.80;
	cmgem.ypmn = 0.10;
	cmgem.lnxdiv = TRUE;
	cmgem.nxdiv = 7;
	cmgem.lnydiv = TRUE;
	cmgem.nydiv = 5;
	cmgem.lxfudg = FALSE;
	kchar = 'U';

	/* - Set up y window for each subplot. */

        nfr = nbands;
        nperfr = 1;
	ypdel = (cmgem.ypmx - cmgem.ypmn)/(float)( nperfr );


	/* these need to be set to correct values eventually */
        for(j=0;j<nfr;j++) {
	  Toff[j] = 0. ;
	  Lhlwrt[j] = FALSE;
	}


	/* - Loop on number of frames. */

	ypmns = cmgem.ypmn;
	ypmxs = cmgem.ypmx;
	ypmxus = cmgem.ypmxu;
	jfr = 1;
L_1900:
	if( jfr > nfr )
	    goto L_7777;

        tmin = C_begin;
        tmax = envelopes[0].number_of_points*envelopes[0].GFdelta;
	jwin = 1;
	twin[jwin - 1][0] = tmin;
	twin[jwin - 1][1] = tmax;


	/* -- Begin new frame and set up some parameters. */
L_2000:

	/* put envelope and GF data into SAC memory */
	cleardfl( nerr ); 

        jdfl = 1; /* Envelope of data */
	nlen = envelopes[jfr-1].number_of_points - 5;
	cmdfm.ndfl = 3;
	crsac( jdfl, 1, nlen, &ndxh, &ndx1, &junk, nerr );
        putfil( jdfl, nerr);
	getfil( jdfl, TRUE, &nlen, &ndx1, &ndxh, nerr );
	Sacmem = cmmem.sacmem[ndx1];
	for( j = 0; j < nlen-1; j++ ){
	  *(Sacmem++) = *(envelopes[jfr-1].envelope_data+j);
	}
        nlen = nlen - 2;
	*iftype = *ixy;
	*leven = TRUE;
	*delta = envelopes[jfr-1].GFdelta;
	*begin = C_begin;
	*npts = nlen;
	*ennd = *begin + (float)( *npts - 1 )**delta;

	extrma( cmmem.sacmem[ndx1], 1, *npts, depmin, depmax, depmen );
	putfil( jdfl, nerr );

        jdfl = 2;  /* Entire GF envelope */
	nlen = envelopes[jfr-1].GFnpts - 50;
	crsac( jdfl, 1, nlen, &ndxh, &ndx2, &junk, nerr );
        putfil( jdfl, nerr);
	getfil( jdfl, TRUE, &nlen, &ndx2, &ndxh, nerr );
	Sacmem = cmmem.sacmem[ndx2];
	for( j = 0; j < nlen-1; j++ ){
	  *(Sacmem++) = (*(envelopes[jfr-1].GFenvelope+j))+envelopes[jfr-1].CodaAmp;
	}
        nlen = nlen - 2;
        *begin = envelopes[jfr-1].window_start_seconds;
	*iftype = *ixy;
	*leven = TRUE;
	*delta = envelopes[jfr-1].GFdelta;
	*npts = nlen;
	*ennd = *begin + (float)( *npts - 1 )**delta;

	extrma( cmmem.sacmem[ndx2], 1, *npts, depmin, depmax, depmen );
	putfil( jdfl, nerr );

        jdfl = 3; /* part of GF envelope used in fit */
	nlen = envelopes[jfr-1].fit_npoints;
	crsac( jdfl, 1, nlen, &ndxh, &ndx2, &junk, nerr );
        putfil( jdfl, nerr);
	getfil( jdfl, TRUE, &nlen, &ndx2, &ndxh, nerr );
	Sacmem = cmmem.sacmem[ndx2];
	for( j = 0; j < nlen-1; j++ ){
	  *(Sacmem++) = (*(envelopes[jfr-1].GFenvelope+j))+envelopes[jfr-1].CodaAmp;
	}
        nlen = nlen - 1;
        *begin = envelopes[jfr-1].window_start_seconds;
	*iftype = *ixy;
	*leven = TRUE;
	*delta = envelopes[jfr-1].GFdelta;
	*npts = nlen;
	*ennd = *begin + (float)( *npts - 1 )**delta;

	extrma( cmmem.sacmem[ndx2], 1, *npts, depmin, depmax, depmen );
	putfil( jdfl, nerr );

	cmgem.lxlim = TRUE;
	cmgem.lylim = TRUE;
	cmgem.ximn = VLARGE;
	cmgem.ximx = -VLARGE;
	cmgem.yimn = VLARGE;
	cmgem.yimx = -VLARGE;
        cmgem.iiwidth[1] = 1;
        cmgem.iiwidth[2] = 5;
        cmgem.niwidth = 3;
        cmgem.lwidth = 1;
        cmgem.liwidth = 1;
        cmgem.iicol[0] = 7;
        cmgem.iicol[1] = 3;
        cmgem.iicol[2] = 1;
        cmgem.nicol = 3;
        cmgem.lcol = 1;
        cmgem.licol = 1;
        cmgem.ltitl = TRUE;
	sprintf(kmgem.ktitl, "Frequency Band= %2.2f-%2.2f Hz",envelopes[jfr-1].freq_low,envelopes[jfr-1].freq_high);
        cmgem.ntitl = (strcspn(kmgem.ktitl," ") + 1);
        cmgem.lylab = TRUE;
	sprintf(kmgem.kylab, "Log Amplitude");
        cmgem.nylab = (strcspn(kmgem.kylab," ") + 1);
        cmgem.lxlab = TRUE;
	sprintf(kmgem.kxlab, "Lapse Time (seconds)");
        cmgem.nxlab = (strcspn(kmgem.kxlab," ") + 1);
        cmgem.igtfnt=7;

        ndfl = 3;
	for( jdfl = 1; jdfl <= ndfl; jdfl++ ){
		getfil( jdfl, TRUE, &nlen, &ndxy, &ndxx, nerr );
		nlen = nlen - 1;
		/* --- X limits first. */
		getxlm( &lxlimj, &ximnj, &ximxj );
		cmgem.ximn = fmin( cmgem.ximn, ximnj + Toff[jdfl] );
		cmgem.ximx = fmax( cmgem.ximx, ximxj + Toff[jdfl] );
		/* --- Y limits are more complicated if XLIM is already on. */
		getylm( &lylimj, &yimnj, &yimxj );
		if( lxlims && !lylimj ){
		    if( *leven ){
			num1 = (int)( (ximnj - *begin)/ *delta ) + 1;
			if( num1 < 1 )
			    num1 = 1;
			num2 = (int)( (ximxj - *begin)/ *delta ) + 1;
			if( num2 > nlen )
			    num2 = nlen;
			if( num1 <= nlen && num2 >= 1 ){
			    num2m1 = num2 - num1 + 1;
			    ndx1 = ndxy + num1 - 1;
			    extrma( cmmem.sacmem[ndxy]+num1-1, 1, num2m1,
				    &yimnj, &yimxj, &fjunk );
			}
			else{
			    yimnj = -1.;
			    yimxj = 1.;
			}
		    }
		    else{
			jx = ndxx;
			jy = ndxy;
			yimnj = VLARGE;
			yimxj = -VLARGE;
                        Sacmem1 = cmmem.sacmem[ndxx];
                        Sacmem2 = cmmem.sacmem[ndxy];
			for( jdx = 1; jdx <= nlen; jdx++ ){
			    if( *Sacmem1 >= ximnj && *Sacmem1 <= ximxj ){
				if( *Sacmem2 < yimnj )
				    yimnj = *Sacmem2;
				if( *Sacmem2 > yimxj )
				    yimxj = *Sacmem2;
			    } /* end if( *Sacmem1 >= ximnj && ... */
			    jx = jx + 1;
			    jy = jy + 1;
                            Sacmem1++; 
			    Sacmem2++;
			} /* end for( jdx = 1; jdx <= nlen; jdx++ ) */
		    } /* end else associated with if( *leven ) */
		} /* end if( lxlims && !lylimj ) */	
		cmgem.yimn = fmin( cmgem.yimn, yimnj );
		cmgem.yimx = fmax( cmgem.yimx, yimxj );
	} /* end for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */

        /* set for fixed plot window */
	if(cmgem.yimn < YVPMIN) 
	  cmgem.yimn = YVPMIN;
	if(cmgem.yimx > YVPMAX)
	  cmgem.yimx = YVPMAX;

	settexttype( kmgem.kgtqua );
	settextfont( cmgem.igtfnt );
	setlinestyle( cmgem.isolid );
	setcolor( cmgem.iskcol );
	setlinewidth( cmgem.ithin );

	/* - Begin new frame if requested. */

        cmgem.lframe = TRUE;
	if( cmgem.lframe ){
	    beginframe( FALSE , nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    getvspace( &cmgem.xvspmn, &cmgem.xvspmx,
		       &cmgem.yvspmn, &cmgem.yvspmx );
	}
	ypmxv = ypmxs*cmgem.yvspmx;
	ypmnv = ypmns*cmgem.yvspmx;
	ypdelv = ypdel*cmgem.yvspmx;
	cmgem.chht = cmgem.tsdef;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );
	xtpos = cmgem.xpmn;
	ytpos = cmgem.yvspmx - 1.1*cmgem.chht;
	cmgem.ltoptc = TRUE;
	cmgem.lxlim = TRUE;
        cmgem.lframe = FALSE;
	cmgem.ximn = tmin;
	cmgem.ximx = tmax;

	if( *nerr != 0 )
	    goto L_8888;
	if( cmgem.liline ){
	    xlinl2 = cmgam.xfidlc - 0.5*cmgem.chwid;
	    xlinl1 = xlinl2 - atrwid;
	}
	if( cmgem.lsym )
	    xsymlc = cmgam.xfidlc - 0.5*cmgem.chwid - 0.5*atrwid;


	ic1 = 0;
	for( jdfl = 1; jdfl <= ndfl; jdfl++ ){
	    getfil( jdfl, TRUE, &num, &nlcy, &nlcx, nerr );
            num = num - 1;
	    if( *nerr != 0 )
		goto L_8888;
	    if( *leven ){
		cmgem.lxgen = TRUE;
		cmgem.xdelta = *delta;
		cmgem.xfirst = *begin + Toff[jdfl];
	    }
	    else{
		cmgem.lxgen = FALSE;
	    }
	    if( cmgam.lfidrq ){
		settextjust( "LEFT", "BOTTOM" );
		if( cmgem.lcol )
		    setcolor( cmgem.icol );
		setlinewidth( cmgem.ithin );
		move( cmgam.xfidlc, cmgam.yfidlc );
		if( cmgam.ifidtp == 4 ){
		    formhv( (char*)kmgam.kfidnm[0],9, cmgam.ifidfm, ktemp
		     ,MCMSG+7, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    nc = indexb ( ktemp , MCMSG+7 ) ;
		    text( ktemp, nc + 1 , nc );
		}
		else{
		    if(lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 )){
		  	nc = ic2 - ic1 + 1;
                        strncpy ( ktemp , kmdfm.kdfl + ic1 - 1 , nc ) ;
                        ktemp[ nc ] = '\0' ;
			char* tmp = malloc((nc + 1) * sizeof(char));
			strncpy(tmp, ktemp, nc);
			if ( cmgam.lfinorq ) {
			    sprintf ( tmp , "%s - %d", ktemp , jdfl ) ;
		        }

		        nc = strlen ( tmp );
		    	strncpy(ktemp, tmp, nc+1);
			text( ktemp , nc + 1 , nc );
		        free(tmp);
		    }
		}
		yatrlc = cmgam.yfidlc + 0.5*cmgem.chht;
		if( cmgem.liline && cmgem.icline > 0 ){
		    setlinewidth( cmgem.iwidth );
		    setlinestyle( cmgem.icline );
		    move( xlinl1, yatrlc );
		    draw( xlinl2, yatrlc );
		    setlinestyle( cmgem.isolid );
		}
		if( cmgem.lsym && cmgem.isym > 0 ){
	            setlinewidth( cmgem.isymwidth );
		    symbol( (float*)&xsymlc, (float*)&yatrlc, 1, TRUE );
                    setlinewidth( cmgem.iwidth );
		}
		if( cmgem.lcol )
				setcolor( cmgem.iskcol );
		setlinewidth( cmgem.iwidth );
		cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
	    }
	    pl2d( cmmem.sacmem[nlcx], cmmem.sacmem[nlcy], num, 1, 1, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	} /* end for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */

	if( cmgam.lfidrq ){
	    cmgem.chht = cmgem.tsdef;
	    cmgem.chwid = cmgem.txrat*cmgem.chht;
	    settextsize( cmgem.chwid, cmgem.chht );
	}
	npmark = 0;
	/* -- If MARKALL option is on, set header and plot marker limits. */

	jhdr1 = 1;
	jhdr2 = 1;
	jmark1 = 1;
	jmark2 = 1;

	setlinewidth( cmgem.ithin );
	plgrid( nerr );
	settextjust( "LEFT", "BOTTOM" );

	/* -- Perform graphics input function. */

	xloc = cmgem.xpmn + 0.05*(cmgem.xpmx - cmgem.xpmn);
	yloc = ypmxv - 0.5*ypdelv;
	cmgem.chht = cmgem.tsdef;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );
	settextangle( cmgem.horz );

L_4000:
	cursor0( &xloc, &yloc, &kchar );

	upcase( &kchar, 1, &kchar, 1 );

	/* -- Go back to last x window. */
	if( kchar == 'O' ){
	    tmin = twin[jwin - 1][0];
	    tmax = twin[jwin - 1][1];
	    jwin = max( 1, jwin - 1 );
	    plhome();
	    endframe( FALSE , nerr );
	    npmark = 0;
	    goto L_2000;
	}

	/* -- Kill PPK; return immediately to command level. */
	else if( kchar == 'Q' || kchar == 'K' ){
	    plhome();
	    endframe( FALSE , nerr );
	    goto L_7777;
	}

	/* -- Go to next subplot. */
	else if( kchar == 'N' ){
	    plhome();
	    endframe( FALSE , nerr );
	    npmark = 0;
	    jfr = jfr + 1;
	    goto L_1900;
	}

	/* -- Go back to last subplot. */
	else if( kchar == 'B' ){
	    plhome();
	    endframe( FALSE , nerr );
	    npmark = 0;
	    jfr = jfr - 1;
	    goto L_1900;
	}

	/* - Rest of cursor responses need a valid cursor position. */

	if( ((xloc < cmgem.xpmn || xloc > cmgem.xpmx) || yloc < ypmnv) || 
	 yloc > ypmxv ){
	    setmsg( "OUTPUT", 1502 );
	    apfmsg( xloc );
	    apfmsg( yloc );
	    pltmsg( &xtpos, &ytpos );
	    ytpos = ytpos - cmgem.chht;
	    goto L_4000;
	}

	/* - Determine time at cursor location.
	 *   (Correct for any differences between the zero times.) */
	if( cmgem.ixint == cmgem.ilin ){
	    secinc = (xloc - cmgem.xmpip2)/cmgem.xmpip1 - Toff[jfr];
	}
	else{
	    secinc = pow(10.,(xloc - cmgem.xmpip2)/cmgem.xmpip1);
	}

	/* - Determine amplitude corresponding to cursor position. */
	amplmn = Yimnzs[jfr];
	amplmx = Yimxzs[jfr];
	cmeam.pkampl = amplmx - (ypmxv - jofset*ypdelv - yloc)*(amplmx - amplmn)/ypdelv;

	/* - Perform action corresponding to returned non-integer character. */

	/* -- Define end of new x window. */
	if( lnewxw ){
	    jwin = min( MWIN, jwin + 1 );
	    twin[jwin - 1][0] = tmin;
	    twin[jwin - 1][1] = tmax;
	    tmin = tminew;
	    tmax = secinc;
	    lnewxw = FALSE;
	    plhome();
	    endframe( FALSE , nerr );
	    goto L_2000;
	}
	/* -- Define start of new x window. */
	else if( kchar == 'X' ){
	    lnewxw = TRUE;
	    tminew = secinc;
	    markvert( jmark1, jmark2, &xloc, ypmxv, ypdelv, "X",2, 0 );
	}

	/* -- Define first arrival time. */
	else if( kchar == 'A' ){
	    markhdr( jdfl, jhdr1, jhdr2, "A", secinc, kmeam.kpkid );
	    markvert( jmark1, jmark2, &xloc, ypmxv, ypdelv, "A" ,2, npmark );
	    npmark = npmark + 1;
            fprintf(stderr, "Window Begin = %f \n",secinc);
	    envelopes[jfr-1].window_start_seconds = secinc;
	}

	else if( kchar == 'D' ){
            fprintf(stderr, "This band will not be used \n");
	    envelopes[jfr-1].fit_window_picked = WINDOW_NOPICK;
	}

	/* -- Define coda length (fini). */
	else if( kchar == 'F' ){
	    strcpy( kmeam.kpkid, "FINI    " );
	    markhdr( jdfl, jhdr1, jhdr2, kmeam.kpkid, secinc, kmhdr.kundef );
	    markvert( jmark1, jmark2, &xloc, ypmxv, ypdelv, kmeam.kpkid ,9, 0 );
            fprintf(stderr, "Window End = %f \n",secinc);
	    envelopes[jfr-1].fit_npoints = (int)((secinc-envelopes[jfr-1].window_start_seconds)/envelopes[jfr-1].GFdelta);
	    envelopes[jfr-1].fit_window_picked = WINDOW_FROM_INTERACTIVE;
	    if( cmeam.lhpfop && Lzdttm[jdfl] ){
		fsecsi = secinc;
		lempty = FALSE;
		cmeam.lfini = TRUE;
		lhltrm = TRUE;
	    }
	}

	/* -- Bad cursor response handled here. */
	else{
	    setmsg( "OUTPUT", 1503 );
	    apcmsg( &kchar,1 );
	    pltmsg( &xtpos, &ytpos );
	    ytpos = ytpos - cmgem.chht;
	}

	/* -- Loop back for another cursor response. */

	goto L_4000;

	/* - Restore plot environment. */

L_7777:
	plrest();

	/* - Return to normal graphics device mode. */

	cursoroff();

	/* - Delete any extra locations from blackboard if necessary. */


L_8888:

	return;

} /* end of function */

