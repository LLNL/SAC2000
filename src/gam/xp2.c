#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "gem.h"
#include "gdm.h"
#include "gam.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void /*FUNCTION*/ xp2(nerr)
int *nerr;
{
	char ktemp[MCMSG+7];	/* increased array size for jdfl.  maf 970130 */
	int lany, lfirst, lxlimj, lxlims, lylimj,
	     lprint = FALSE , ltry = FALSE ;
	int ic1, ic2, jdx, jdfl, jx, jy, nc, ndx1, ndx2, 
	 ndxx, ndxy, nlcx, nlcy, nlen, notused, nrdttm[6], num, num1, 
	 num2, num2m1;
	float atrwid, fjunk, slen, slenm, toff[MDFL], ximnj, ximxj, xjunk, 
	 xlinl1, xlinl2, xsymlc, yatrlc, yimnj, yimxj;
	void zgetgd();


        float *Sacmem1, *Sacmem2;

	float *const Toff = &toff[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To execute the action command PLOT2.
	 *           This command makes a multi-trace, single window plot.
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
	 *             lsym, isym, iskpen, iskcol, tsdef, chwid, chht, icline,
	 *             lwidth, iwidth, iskwidth
	 *    gam:     kgddef, lp2abs, lfidrq, ifidtp, kfidnm, ifidlc, fidbdr,
	 *             tsfid
	 *    dfm:     ndfl
	 *    hdr:     nzdttm, leven, delta, begin
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     lxlim, ximn, ximx, lylim, yimn, yimx
	 *    gam:     lp2abs, xfidlc, yfidlc
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, lclog2, cfmt, cresp, vflist, ddttm, plrest,
	 *             getstatus, begindevices, plsave, getfil, ldttm, setcolor,
	 *             getxlm, getylm, beginframe, plmap, settextsize, copyi
	 *             pltext, symbol, pldta, plgrid, plhome, endframe,
	 *             setlinewidth
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    nrdttm:  six element array containing "reference" date/time
	 *             used to calculate time offsets in absolute mode. [i]
	 *    toff(j): Contains the time offset for file "j". [f]
	 *             Used in both absolute and relative modes.* MODULE/LEVEL: 
	 *    atrwid:  Width of linestyle/symbol attribute part of fileid. [f]
	 *    xlinl1:  X beginning of linestyle attribute display. [f]
	 *    xlinl2:  X ending of linestyle attribute display. [f]
	 *    xsymlc:  X location of symbol attribute display. [f]
	 *    yatrlc:  Y location of linestyle/symbol attribute display. [f]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    980128:  Fixed bug plotting fileid. maf
	 *    970908:  Changed response to ddttm().  maf
	 *    970205:  Moved settextjust() to for loop to fix a bug.  maf
	 *    970130:  Allows printing of file numbers.
	 *    920528:  Added setlinewidth call, lwidth & iwidth variables.
	 *    910608:  Added call to zgetgd when no graphics device specified.
	 *             Changed begindevice to begindevices. (wct)
	 *    910301:  Changed iline to icline.
	 *    890227:  Fileid can now be the first header item from the LIST
	 *             option of the FILEID command.
	 *    861112:  Deleted calls to ZPEN.
	 *    860213:  Deleted LNICE logic.
	 *    860114:  Fixed bug in fileid involving linestyle in fileid.
	 *    850814:  Added linestyle attribute display to fileid.
	 *    850516:  Fixed bug in fileid placement when symbol plotting is on.
	 *    850506:  Text and display attributes now being set correctly.
	 *    831006:  Changed to FILEID options instead of local ones.
	 *             Added ABSOLUTE/RELATIVE option.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820317:  Fixed bug with setting pen numbers for table.
	 *    811228:  Added symbol display to legend.
	 *    811030:  Fixed bug when framing option was off.
	 *    810708:  Added SOLID option which plots first trace as a solid
	 *             line and then reverts to current line style.
	 *    810120:  Changed to output message retrieval from disk.
	 *    791206:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861112
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "RELATIVE/ABSOLUTE":  change method of displaying time
					on x axis. */
	    if( lclog2( "A$",3, "R$",3, &cmgam.lp2abs ) )
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

	/* - Save current plot environment. */

	plsave();

	/* - Calculate x and y axis limits. */

	getxlm( &lxlims, &xjunk, &xjunk );
	cmgem.lxlim = TRUE;
	cmgem.lylim = TRUE;
	cmgem.ximn = VLARGE;
	cmgem.ximx = -VLARGE;
	cmgem.yimn = VLARGE;
	cmgem.yimx = -VLARGE;

	/* -- Absolute mode. */
	if( cmgam.lp2abs ){
	    lfirst = TRUE;
	    for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		getfil( jdfl, TRUE, &nlen, &ndxy, &ndxx, nerr );
		if( *nerr != 0 )
		    goto L_8888;
		/* --- LDTTM function returns .TRUE. if 
			     date-time stamp is defined. */
		if( ldttm( nzdttm ) ){
		    if( lfirst ){
			lfirst = FALSE;
			copyi( nzdttm, nrdttm, 6 );
			Toff[jdfl] = 0.;
		    }
		    else if ( *iftype == IRLIM || *iftype == IAMPH ) {
			/* if it is frequency information, plot relative */
			Toff[jdfl] = 0. ;
		    }
		    else{
			/* --- DDTTM function computes difference in two
				     date-time stamps. */
			ddttm( nzdttm, nrdttm, &Toff[jdfl] );
			/* if difference is more than twodays, plot relative */
			if ( fabs ( Toff[jdfl] ) > TWODAYS )
			    Toff[jdfl] = 0. ;
		    }
		} /* end if( ldttm( nzdttm ) ) */
		else{
		    Toff[jdfl] = 0.;
		}
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
	} /* end if( cmgam.lp2abs ) */

	/* -- Relative mode. */
	else{
	    cmgem.ximn = 0.;
	    cmgem.ximx = -VLARGE;
	    for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
		    goto L_8888;
		getxlm( &lxlimj, &ximnj, &ximxj );
		cmgem.ximx = fmax( cmgem.ximx, ximxj - ximnj );
		Toff[jdfl] = -ximnj;
		getylm( &lylimj, &yimnj, &yimxj );
		cmgem.yimn = fmin( cmgem.yimn, yimnj );
		cmgem.yimx = fmax( cmgem.yimx, yimxj );
	    } /* end for ( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */
	} /* end else */

	/* - Set background and skeleton attributes. */

	settexttype( kmgem.kgtqua );
	settextfont( cmgem.igtfnt );
	setlinestyle( cmgem.isolid );
	setcolor( cmgem.iskcol );
	setlinewidth( cmgem.ithin );

	/* - Begin new frame if requested. */

	if( cmgem.lframe ){
	    beginframe( lprint , nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    getvspace( &cmgem.xvspmn, &cmgem.xvspmx,
		       &cmgem.yvspmn, &cmgem.yvspmx );
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
	    cmgam.fidbdr = cmgem.chht;
	    slenm = 0.;
	    for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		int numChars ;  /* to fix bug plotting fileid. maf 980128 */
		if( cmgam.ifidtp == 4 ){
		    getfil( jdfl, FALSE, &notused, &notused, &notused, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    formhv( (char*)kmgam.kfidnm[0],9, cmgam.ifidfm, ktemp
			 ,MCMSG+7, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    numChars = indexb ( ktemp , MCMSG + 7 ) ;
		    getstringsize( ktemp, numChars, &slen );
		} /* end if( cmgam.ifidtp == 4 ) */
		else{
		    if(lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 )){
			nc = ic2 - ic1 + 1;
                        strncpy ( ktemp , kmdfm.kdfl + ic1 - 1 , nc ) ;
                        ktemp[nc] = '\0';
			if ( cmgam.lfinorq )
			    sprintf ( ktemp , "%s - %d" , ktemp , jdfl ) ;
			numChars = indexb ( ktemp , MCMSG + 7 ) ;
			getstringsize ( ktemp , numChars , &slen ) ;
		     }
                     else slen = 0.;
		} /* end else associated with if( cmgam.lp2abs ) */
		slenm = fmax( slenm, slen );
	    } /* end for ( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */

	    if( cmgem.liline )
		atrwid = 2.5*cmgem.chwid;
	    else if( cmgem.lsym )
		atrwid = cmgem.chwid;
	    else
		atrwid = 0.;

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
		cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr +
			       (float)( cmdfm.ndfl - 1 )*cmgem.chht;
	    }
	    else if( cmgam.ifidlc == cmgam.ill ){
		cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + atrwid;
		cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr +
			       (float)( cmdfm.ndfl - 1 )*cmgem.chht;
	    }
	    else{
		cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + atrwid;
		cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
	    }
	/*    settextjust( "LEFT", "BOTTOM" );	*/
	} /* end if( cmgam.lfidrq ) */

	if( cmgem.liline ){
	    xlinl2 = cmgam.xfidlc - 0.5*cmgem.chwid;
	    xlinl1 = xlinl2 - atrwid;
	}
	if( cmgem.lsym )
	    xsymlc = cmgam.xfidlc - 0.5*cmgem.chwid - 0.5*atrwid;


	/* - Loop to plot each trace in DFL on same plot window. */

	ic1 = 0;
	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    getfil( jdfl, TRUE, &num, &nlcy, &nlcx, nerr );
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
			if ( cmgam.lfinorq ){
			    sprintf ( ktemp , "%s - %d", ktemp , jdfl ) ;
			}
			nc = strlen ( ktemp );
			text( ktemp , nc + 1 , nc );
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
	    pldta( cmmem.sacmem[nlcx], cmmem.sacmem[nlcy], num, 1, 1, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	} /* end for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */

	if( cmgam.lfidrq ){
	    cmgem.chht = cmgem.tsdef;
	    cmgem.chwid = cmgem.txrat*cmgem.chht;
	    settextsize( cmgem.chwid, cmgem.chht );
	}

	/* - Draw grid lines, axes and such. */

	setlinewidth( cmgem.ithin );
	if( !cmgam.lp2abs )
	    pltext( "RELATIVE MODE",14, cmgam.xfidlc, cmgam.yfidlc );
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

