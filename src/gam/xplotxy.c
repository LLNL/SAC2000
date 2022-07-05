#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
#include "../../inc/dfm.h"
#include "../../inc/mem.h"
#include "../../inc/hdr.h"
#include "../../inc/xyz.h"
void /*FUNCTION*/ xplotxy(nerr)
int *nerr;
{
	char kfile[MCPFN+1], ktemp[MCMSG+1];
	int lany, lchange, lfirst, lxlims, lylimj;
	int _l0, ic1, ic2, idflnumber[MDFL], jdfl, jdflnumber, jdflnumber_, 
	 nc, ncfile, ndflnumber, nlcx, nlcy, notused, num, numx, numy;
	float _f0, atrwid, slen, slenm, slenvs, unused, vportratio, xlinl1, 
	 xlinl2, xrange, xsymlc, yatrlc, yimnj, yimxj, yrange;
	void *_p0, zgetgd();
        char *s1;


	int *const Idflnumber = &idflnumber[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To execute the action command PLOTXY.
	 *           This command makes a multi-trace, single window plot.
	 *           The user specifies which data file contains the "x" data
	 *           and which data file(s) contain the "y" data.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:   gam/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG
	 *    gem:     lframe, xpmn, xpmx, ypmn, ypmx, lpen, ipen, lcol, icol,
	 *             lsym, isym, iskpen, iskcol, tsdef, chwid, chht, icline
	 *    gam:     kgddef, lfidrq, ifidtp, kfidnm, ifidlc, fidbdr, tsfid
	 *    dfm:     ndfl
	 *    hdr:     depmin, depmax
	 *    xyz:     laspect
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     lxlim, ximn, ximx, lylim, yimn, yimx
	 *    gam:     xfidlc, yfidlc
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, lcirc, cfmt, cresp, vflist, plrest, getstatus
	 *             begindevices, plsave, getfil, ldttm, setcolor,
	 *             getxlm, getylm, beginframe, plmap, settextsize,
	 *             pltext, symbol, pldta, plgrid, plhome, endframe
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    atrwid:  Width of linestyle/symbol attribute part of fileid. [f]
	 *    xlinl1:  X beginning of linestyle attribute display. [f]
	 *    xlinl2:  X ending of linestyle attribute display. [f]
	 *    xsymlc:  X location of symbol attribute display. [f]
	 *    yatrlc:  Y location of linestyle/symbol attribute display. [f]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    910608:  Added call to zgetgd when no graphics device specified.
	 *             Changed call to begindevice to begindevices. (wct)
	 *    910301:  Changed iline to icline.
	 *    901215:  Fixed xlim option, added 'lxlim = .true. if limits are set.
	 *    901121:  Added aspect option, like is used in contour command. wct
	 *    890420:  Original version based on xp2.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890420
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
        ndflnumber = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	jdflnumber = 0;
	lchange = FALSE;
L_1000:
	if( lcmore( nerr ) ){

		/* -- ASPECT ON|OFF:  maintain aspect ratio of data or not. */
		if( lklog( "ASPECT$",8, &cmxyz.laspect ) ){

			/* -- integer:  the index number of data file in data file list. */
			}
		else if( lcirc( 1, cmdfm.ndfl, &jdfl ) ){
			jdflnumber = jdflnumber + 1;
			Idflnumber[jdflnumber] = jdfl;
			lchange = TRUE;

			/* -- "filename":  the name of a data file in the data file list. */
			}
		else if( lcchar( MCPFN, kfile,MCPFN+1, &ncfile ) ){
			jdfl = nfndcl( kmdfm.kdfl,MAXCHARS, kfile,MCPFN+1, &ic1, &ic2 );
			if( jdfl > 0 ){
				jdflnumber = jdflnumber + 1;
				Idflnumber[jdflnumber] = jdfl;
				lchange = TRUE;
				}
			else{
				*nerr = 5106;
				setmsg( "ERROR", *nerr );
				apcmsg( kfile,MCPFN+1 );
				goto L_8888;
				}

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	if( lchange )
		ndflnumber = jdflnumber;

	/* CHECKING PHASE: */

	/* - Make sure there are at least two data files specified. */

	if( ndflnumber < 2 ){
		*nerr = 1505;
		setmsg( "ERROR", *nerr );
		goto L_8888;
		}

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

	/* - Save current plot environment. */

	plsave();

	/* - Set x axis limits based on data file unless limits are already set. */

	getxlm( &lxlims, &cmgem.ximn, &cmgem.ximx );
	if( !lxlims ){
		getfil( Idflnumber[1], FALSE, &notused, &notused, &notused, 
		 nerr );
		if( *nerr != 0 )
			goto L_8888;
		xrange = *depmax - *depmin;
		cmgem.lxlim = TRUE;
		cmgem.ximn = *depmin - cmgem.xfudg*xrange;
		cmgem.ximx = *depmax + cmgem.xfudg*xrange;
		}
	else{
		cmgem.lxlim = TRUE;
		}

	/* - Set y axis limits. */

	cmgem.lylim = TRUE;
	cmgem.yimn = VLARGE;
	cmgem.yimx = -VLARGE;
	lfirst = TRUE;
	for( jdflnumber = 2; jdflnumber <= ndflnumber; jdflnumber++ ){
		jdflnumber_ = jdflnumber - 1;
		jdfl = Idflnumber[jdflnumber];
		getfil( jdfl, FALSE, &notused, &notused, &notused, nerr );
		if( *nerr != 0 )
			goto L_8888;
		getylm( &lylimj, &yimnj, &yimxj );
		cmgem.yimn = fmin( cmgem.yimn, yimnj );
		cmgem.yimx = fmax( cmgem.yimx, yimxj );
		}
	yrange = cmgem.yimx - cmgem.yimn;
	cmgem.lylim = TRUE;
	cmgem.yimn = cmgem.yimn - cmgem.yfudg*yrange;
	cmgem.yimx = cmgem.yimx + cmgem.yfudg*yrange;

	/* - Set background and skeleton attributes. */

	settexttype( kmgem.kgtqua );
	settextfont( cmgem.igtfnt );
	setlinestyle( cmgem.isolid );
	setcolor( cmgem.iskcol );

	/* -- Set viewport using different aspect ratio if ASPECT ON. */
	if( cmxyz.laspect ){
		vportratio = fabs( (cmgem.yimx - cmgem.yimn)/(cmgem.ximx - 
		 cmgem.ximn) );
		setvspacetype( FALSE, vportratio );
		getvport( &cmgem.xpmnu, &cmgem.xpmxu, &cmgem.ypmnu, &cmgem.ypmxu );
		}
	else{
		setvspacetype( TRUE, unused );
		getvport( &cmgem.xpmnu, &cmgem.xpmxu, &cmgem.ypmnu, &cmgem.ypmxu );
		}

	/* - Begin new frame if requested. */

	if( cmgem.lframe ){
		beginframe( FALSE , nerr );
		if( *nerr != 0 )
			goto L_8888;
		getvspace( &cmgem.xvspmn, &cmgem.xvspmx, &cmgem.yvspmn, &cmgem.yvspmx );
		}

	/* - Calculate mapping transformation for these fixed limits.
	 *   (In this case, all passed variables but NERR are unused.) */

	plmap( cmmem.sacmem[1], cmmem.sacmem[1], 1, 1, 1, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Determine location for id. */

	ic1 = 0;
	if( cmgam.lfidrq ){
		cmgem.chht = cmgam.tsfid;
		cmgem.chwid = cmgem.txrat*cmgem.chht;
		settextsize( cmgem.chwid, cmgem.chht );
		getstringsize( "vs. ", 4, &slenvs );
		cmgam.fidbdr = cmgem.chht;
		slenm = 0.;
		for( jdflnumber = 1; jdflnumber <= ndflnumber; jdflnumber++ ){
			jdflnumber_ = jdflnumber - 1;
			jdfl = Idflnumber[jdflnumber];
			if( cmgam.ifidtp == 4 ){
				getfil( jdfl, FALSE, &notused, &notused, &notused, 
				 nerr );
				if( *nerr != 0 )
					goto L_8888;
				formhv( (char*)kmgam.kfidnm[0],9, cmgam.ifidfm, ktemp
				 ,MCMSG+1, nerr );
				if( *nerr != 0 )
					goto L_8888;
				nc = indexb( ktemp,MCMSG+1 );
				getstringsize( ktemp, nc, &slen );
				}
			else{
				lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
				nc = ic2 - ic1 + 1;
                                strncpy((s1=malloc(nc+1)),kmdfm.kdfl+ic1 - 1,nc);
                                s1[nc] = '\0';
				getstringsize( s1, nc, &slen );
                                free(s1);
				}
			slenm = fmax( slenm, slen );
			}
		/*        if(liline)then
		 *          atrwid=2.5*chwid
		 *        elseif(lsym)then
		 *          atrwid=chwid
		 *        else
		 *          atrwid=0.
		 *        endif */
		atrwid = slenvs;
		if( cmgam.ifidlc == cmgam.iur ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - slenm;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
			}
		else if( cmgam.ifidlc == cmgam.iul ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + atrwid;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
			}
		else if( cmgam.ifidlc == cmgam.ilr ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - slenm;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + (float)( ndflnumber - 
			 1 )*cmgem.chht;
			}
		else if( cmgam.ifidlc == cmgam.ill ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + atrwid;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + (float)( ndflnumber - 
			 1 )*cmgem.chht;
			}
		else{
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + atrwid;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
			}
		settextjust( "LEFT", "BOTTOM" );
		}
	if( cmgem.liline ){
		xlinl2 = cmgam.xfidlc - 0.5*cmgem.chwid;
		xlinl1 = xlinl2 - atrwid;
		}
	if( cmgem.lsym )
		xsymlc = cmgam.xfidlc - 0.5*cmgem.chwid - 0.5*atrwid;

	/* - Loop to plot each requested file vs designated x file. */

	getfil( Idflnumber[1], TRUE, &numx, &nlcx, &notused, nerr );
	if( *nerr != 0 )
		goto L_8888;
	cmgem.lxgen = FALSE;
	ic1 = 0;
	for( jdflnumber = 2; jdflnumber <= ndflnumber; jdflnumber++ ){
		jdflnumber_ = jdflnumber - 1;
		jdfl = Idflnumber[jdflnumber];
		getfil( jdfl, TRUE, &numy, &nlcy, &notused, nerr );
		if( *nerr != 0 )
			goto L_8888;
		num = min( numx, numy );
		if( cmgam.lfidrq ){
			if( cmgem.lcol )
				setcolor( cmgem.icol );
			move( cmgam.xfidlc, cmgam.yfidlc );
			if( cmgam.ifidtp == 4 ){
				formhv( (char*)kmgam.kfidnm[0],9, cmgam.ifidfm, ktemp
				 ,MCMSG+1, nerr );
				if( *nerr != 0 )
					goto L_8888;
				nc = indexb( ktemp,MCMSG+1 );
				text( ktemp,MCMSG+1, nc );
				}
			else{
				lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
				nc = ic2 - ic1 + 1;
                                strncpy((s1=malloc(nc+1)),kmdfm.kdfl+ic1 - 1,nc);
                                s1[nc] = '\0';
				text( s1, nc+1, nc );
				free(s1);
				}
			yatrlc = cmgam.yfidlc + 0.5*cmgem.chht;
			if( cmgem.liline && cmgem.icline > 0 ){
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
			cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
			}

		pldta( cmmem.sacmem[nlcx], cmmem.sacmem[nlcy], num, 1, 1, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

	/* - Add last line to fileid and reset text size to default value.
	 *   Last line specifies which data file was used for the x axis. */

	if( cmgam.lfidrq ){
		move( cmgam.xfidlc, cmgam.yfidlc );
		jdfl = Idflnumber[1];
		if( cmgam.ifidtp == 4 ){
			getfil( jdfl, FALSE, &notused, &notused, &notused, nerr );
			if( *nerr != 0 )
				goto L_8888;
			formhv( (char*)kmgam.kfidnm[0],9, cmgam.ifidfm, ktemp,MCMSG+1, 
			 nerr );
			if( *nerr != 0 )
				goto L_8888;
			nc = indexb( ktemp,MCMSG+1 );
			text( ktemp,MCMSG+1, nc );
			}
		else{
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
			nc = ic2 - ic1 + 1;
                        strncpy((s1=malloc(nc+1)),kmdfm.kdfl+ic1 - 1,nc);
                        s1[nc] = '\0';
			text( s1,nc+1 , nc );
			free(s1);
			}
		move( cmgam.xfidlc - slenvs, cmgam.yfidlc );
		text( "vs. ",5, 4 );
		cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
		cmgem.chht = cmgem.tsdef;
		cmgem.chwid = cmgem.txrat*cmgem.chht;
		settextsize( cmgem.chwid, cmgem.chht );
		}

	/* - Draw grid lines, axes and such. */

	plgrid( nerr );

	/* - Home cursor and end frame if requested. */

	plhome();
	if( cmgem.lframe )
		endframe( FALSE , nerr );

	/* - Restore plot environment and return. */

L_8888:
	plrest();
	settextjust( "LEFT", "BOTTOM" );

	return;

} /* end of function */

