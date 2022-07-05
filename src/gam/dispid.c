#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ dispid( ldfl, idfl )
int ldfl ; /* 1 to plot the file number, 0 to not. maf 970130 */
int  idfl ;  /* file number. maf 970130 */
{
	int jfidnm, jfidnm_, jfidtx, jfidtx_, nc, nc1, nc2, nerr;
	float slen, slen1, slen1m, slen2, slen2m, slenm;
	void *_p0;
        char *strtemp;


	/*=====================================================================
	 * PURPOSE: To put file id information on current plot.
	 *=====================================================================
	 * MODULE/LEVEL:  GAM/3
	 *=====================================================================
	 * INPUT:
	 *    LDFL:    1 means print file number.
	 *    IDFL:    File number.
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     KEVNM,
	 *    DFM:     KDFL, IDFLC
	 *    GEM:     XPMN, XPMX, YPMX, CHWID, CHHT, THWRAT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GAM:     XFIDLC, YFIDLC, FIDBDR
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - This routine is called by PL2D in GEM module.
	 *   This routine decouples DFM variables from GEM/GAM modules.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  SETTEXTSIZE, PLTEXT, INDEXB
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Change text size. */
	cmgem.chht = cmgam.tsfid;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );

	/* - Compute location for id, even if standard fileid is not requested.
	 *   This is done so that auxiliary information can be written anyway. */

	cmgam.fidbdr = cmgem.chht;
	if( !cmgam.lfidrq ){
		if( cmgam.ifidlc == cmgam.iur ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - 16.0*cmgem.chwid;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.iul ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.ilr ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - 16.0*cmgem.chwid;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + 4.0*cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.ill ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr;
			cmgam.yfidlc = cmgem.ypmxu + cmgam.fidbdr + 4.0*cmgem.chht;
		}
		settextjust( "LEFT", "BOTTOM" );
		goto L_8888;
	} /* end if( !cmgam.lfidrq ) */

	cmgam.nfidtx = 0;

	for( jfidnm = 1; jfidnm <= cmgam.nfidnm; jfidnm++ ){
		jfidnm_ = jfidnm - 1;
		cmgam.nfidtx = cmgam.nfidtx + 1;
		formhv( (char*)kmgam.kfidnm[jfidnm_],9, cmgam.ifidfm, 
		  (char*)kmgam.kfidtx[cmgam.nfidtx - 1] ,41, &nerr );
		if( nerr != 0 )
			cmgam.nfidtx = cmgam.nfidtx - 1;
	}

	if( cmgam.nfidtx <= 0 ){
		cmgam.nfidtx = 1;
		formhv( "FILENAME",9, cmgam.ifidfm, (char*)kmgam.kfidtx[cmgam.nfidtx - 1]
		 ,41, &nerr );
	}

	if( cmgam.ifidfm == 1 ){
		slen1m = 0.;
		slen2m = 0.;
		for( jfidtx = 1; jfidtx <= cmgam.nfidtx; jfidtx++ ){
			jfidtx_ = jfidtx - 1;
			nc1 = indexc( (char*)kmgam.kfidtx[jfidtx_],41, '=' ) + 1;
			getstringsize( (char*)kmgam.kfidtx[jfidtx_], nc1, &slen1 );
			slen1m = fmax( slen1m, slen1 );
			nc2 = indexb( (char*)kmgam.kfidtx[jfidtx_],41 );

                        strtemp = malloc(42-(nc1+1));
                        strncpy(strtemp,kmgam.kfidtx[jfidtx_]+nc1,-(nc1 + 1) + 41);
                        strtemp[41-(nc1+1)] = '\0';

			getstringsize( strtemp, nc2 - nc1 + 1, &slen2 );

                        free(strtemp);
			slen2m = fmax( slen2m, slen2 );
		}
		if( cmgam.ifidlc == cmgam.iur ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - slen2m;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.iul ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + slen1m;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.ilr ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - slen2m;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + 
			  (float)( cmgam.nfidtx - 1 )*cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.ill ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + slen1m;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + 
			  (float)( cmgam.nfidtx - 1 )*cmgem.chht;
		}
		else{
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr + slen1m;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		for( jfidtx = 1; jfidtx <= cmgam.nfidtx; jfidtx++ ){
			jfidtx_ = jfidtx - 1;
			settextjust( "RIGHT", "BOTTOM" );
			nc1 = indexc( (char*)kmgam.kfidtx[jfidtx_],41, '=' ) + 1;

                        strtemp = malloc(nc1+1);
                        strncpy(strtemp,kmgam.kfidtx[jfidtx_], nc1);
                        strtemp[nc1] = '\0';

			pltext( strtemp, nc1+1, cmgam.xfidlc, cmgam.yfidlc );

                        free(strtemp);

			settextjust( "LEFT", "BOTTOM" );
			nc2 = indexb( (char*)kmgam.kfidtx[jfidtx_],41 );

                        strtemp = malloc(nc2-(nc1+1)+2);
                        strncpy(strtemp,kmgam.kfidtx[jfidtx_]+nc1, nc2 - 
			 (nc1 + 1) + 1);
                        strtemp[nc2-(nc1+1)+1] = '\0';

			pltext( strtemp, nc2 - (nc1 + 1) + 2, cmgam.xfidlc, 
			 cmgam.yfidlc );

                        free(strtemp);
			cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
		}
	}
	else{
		slenm = 0.;
		for( jfidtx = 1; jfidtx <= cmgam.nfidtx; jfidtx++ ){
			jfidtx_ = jfidtx - 1;
			nc = indexb( (char*)kmgam.kfidtx[jfidtx_],41 );
			getstringsize( (char*)kmgam.kfidtx[jfidtx_], nc, &slen );
			slenm = fmax( slenm, slen );
		}
		if( cmgam.ifidlc == cmgam.iur ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - slenm;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.iul ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.ilr ){
			cmgam.xfidlc = cmgem.xpmxu - cmgam.fidbdr - slenm;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + 
			  (float)( cmgam.nfidtx - 1 )*cmgem.chht;
		}
		else if( cmgam.ifidlc == cmgam.ill ){
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr;
			cmgam.yfidlc = cmgem.ypmnu + cmgam.fidbdr + 
			  (float)( cmgam.nfidtx - 1 )*cmgem.chht;
		}
		else{
			cmgam.xfidlc = cmgem.xpmnu + cmgam.fidbdr;
			cmgam.yfidlc = cmgem.ypmxu - cmgam.fidbdr - cmgem.chht;
		}
		settextjust( "LEFT", "BOTTOM" );
		for( jfidtx = 1; jfidtx <= cmgam.nfidtx; jfidtx++ ){
			jfidtx_ = jfidtx - 1;
			pltext( (char*)kmgam.kfidtx[jfidtx_],41, cmgam.xfidlc, 
			 cmgam.yfidlc );
			cmgam.yfidlc = cmgam.yfidlc - cmgem.chht;
		}
	}

	/* Plot file number if appropriate.  maf 970130 */
	if ( ldfl ) {
		char kdfl[ 4 ] ;
		float xPosition, yPosition ;
		float minHeight = 0.0111866 , minWidth = 0.007458103 ;

		/* make sure characters are big enought to be read. */
		if ( cmgem.chht < minHeight || cmgem.chwid < minWidth )
		    settextsize( minWidth, minHeight );

		sprintf ( kdfl , "%d", idfl ) ;

		xPosition = ( cmgem.xvspmx - cmgem.xvspmn ) * 0.92 + cmgem.xvspmn ;
		yPosition = cmgam.yfidlc + cmgem.chht ;

		pltext ( kdfl , strlen ( kdfl ) + 1 , xPosition , yPosition ) ;
	}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    970130:  Added ldfl and idfl for plotting of file number.  maf
	 *    850515:  Outputs FILENAME if all header fields undefined.
	 *    820324:  Changed character size to medium.
	 *             Now computing number of characters in each string.
	 *    801018:  Changes to reflect header version 5.
	 *    800922:  Modified station id format.
	 *    800823:  Original version (factored out of P1 and PP).
	 *===================================================================== */

} /* end of function */

