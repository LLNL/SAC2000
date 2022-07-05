#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	FSECAX	8.
#define	FSECTC	4.
#define	MDECLB	4

#include "../../inc/mach.h"
#include "../../inc/gtm.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ ylogaxis(llefax, lrigax, lleftc, lrigtc, widlef, 
	 widrig)
int llefax, lrigax, lleftc, lrigtc;
float *widlef, *widrig;
{
	char _c0[2], kdec[9], khorz[9], kvert[9];
	int lloglb, lsecax, lsectc, lygrid;
	int _d_l, _d_m, _do0, _do1, _do2, _do3, _do4, _do5, _do6, 
	 _do7, _l0, idecin, idecmn, idecmx, iline, isecin, iygrid, jdec, 
	 jdec_, jfac, jfac_, nc, ndivu;
	float _f0, chht, chwid, decade, decmn, decmx, decsiz, slen, slen10, 
	 slenmx, xloc, ypmnf, ypmxf, yref, yrefs;


	/*=====================================================================
	 * *** INTERNAL SUBROUTINE:  NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To produce a logarithmically-scaled axis to the right
	 *           and/or left of the current plot window.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    llefax:  Set to .TRUE. for an annotated axis to the left of
	 *             the current viewport. [l]
	 *    lrigax:  Set to .TRUE. for an annotated axis to the right. [l]
	 *    lleftc:  Set to .TRUE. for tick marks to the left. [l]
	 *    lrigtc:  Set to .TRUE. for tick marks to the right. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    widlef:  Viewport width of annotated axes at left of viewport. [f]
	 *    widrig:  Viewport width of annotated axis at right of viewport. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gtm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gtm:     ywcmin, ywcmax, yvpmin, yvpmax, lloglb,
	 *             ympwv1, ympwv2, lygrd, fac
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  cnvita, ljust, line, text, setlinestyle
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    mdeclb:  Maximum number of labeled decades.
	 *    fsecax:  Factor used to determine secondary labels.
	 *    fsectc:  Factor used to determine secondary tick marks.
	 *    lsecax:  .TRUE. if there is enough room for secondary axis labels.
	 *    lsectc:  .TRUE. if there is enough room for secondary tick marks.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version based on YLOGAX.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861020
	 *===================================================================== */
	/* PROCEDURE: */

	/* - Get current graphics character size. */
	gettextsize( &chwid, &chht );

	/* - Label every decade if there are no more than MDECLB of them. */

	decmn = cmgtm.ywcmin;
	if( decmn <= 0. ){
		idecmn = decmn;
		}
	else{
		idecmn = decmn + 0.9999999;
		}
	decmx = cmgtm.ywcmax;
	if( decmx >= 0. ){
		idecmx = decmx;
		}
	else{
		idecmx = decmx - 0.9999999;
		}
	ndivu = idecmx - idecmn + 1;
	idecin = ndivu/MDECLB;
	if( idecin < 1 )
		idecin = 1;

	/* - Determine if there is room for secondary labels and/or tick marks. */

	decsiz = fabs( cmgtm.yvpmax - cmgtm.yvpmin )/(cmgtm.ywcmax - cmgtm.ywcmin);
	if( (decsiz > FSECAX*chht && idecin == 1) && lloglb ){
		lsecax = TRUE;
		isecin = 2;
		if( decsiz > 2.*FSECAX*chht )
			isecin = 1;
		}
	else{
		lsecax = FALSE;
		}
	if( decsiz > FSECTC*chht && idecin == 1 ){
		lsectc = TRUE;
		}
	else{
		lsectc = FALSE;
		}

	/* - Calculate width of exponent. */

	slenmx = 0.;
	for( jdec = idecmn; jdec <= idecmx; jdec += idecin){
		jdec_ = jdec - 1;
		cnvita( jdec, kdec,9 );
		ljust( kdec,9 );
		nc = indexb( kdec,9 );
		getstringsize( kdec, nc, &slen );
		slenmx = fmax( slenmx, slen );
		}
	getstringsize( "10", 2, &slen10 );

	/* - Save current linestyle and text justification. Set linestyle to solid. */

	getlinestyle( &iline );
	gettextjust( khorz,9, kvert );
	setlinestyle( 1 );

	/* - Draw the left axis. */

	if( llefax || lleftc ){

		/* -- Axes line. */
		line( cmgtm.xvpmin, cmgtm.yvpmin, cmgtm.xvpmin, cmgtm.yvpmax );

		/* -- Put secondary labels on decade before first full one, if necessary. */
		decade = (float)( idecmn - 1 );
		yref = decade*cmgtm.ympwv1 + cmgtm.ympwv2;
		ypmnf = cmgtm.yvpmin - 0.0002;
		ypmxf = cmgtm.yvpmax + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			yrefs = yref + cmgtm.fac[jfac_]*cmgtm.ympwv1;
			if( yrefs >= ypmnf && yrefs <= ypmxf ){
				if( (lsecax && llefax) && (jfac%isecin) == 0 ){
					xloc = cmgtm.xvpmin - 0.1*chwid;
					settextjust( "RIGHT", "CENTER" );
					move( xloc, yrefs );
					text( &kmgtm.kfac[jfac_],1, 1 );
					}
				if( lsectc ){
					line( cmgtm.xvpmin, yrefs, cmgtm.xvpmin + 
					 0.5*chwid, yrefs );
					}
				}
			}

		/* -- Put primary and secondary labels on remainder of axis. */
		strcpy( kdec, "        " );
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin){
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			cnvita( jdec, kdec,9 );
			ljust( kdec,9 );
			yref = decade*cmgtm.ympwv1 + cmgtm.ympwv2;
			if( llefax ){
				xloc = cmgtm.xvpmin - 0.2*chwid - slenmx;
				settextjust( "RIGHT", "CENTER" );
				move( xloc, yref );
				text( "10",3, 2 );
				settextjust( "LEFT", "BOTTOM" );
				nc = indexb( kdec,9 );
				text( kdec,9, nc );
				}
			line( cmgtm.xvpmin, yref, cmgtm.xvpmin + chwid, 
			 yref );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				yrefs = yref + cmgtm.fac[jfac_]*cmgtm.ympwv1;
				if( yrefs <= ypmxf ){
					if( (lsecax && llefax) && (jfac%isecin) == 0 ){
						xloc = cmgtm.xvpmin - chwid;
						settextjust( "LEFT", "CENTER" );
						move( xloc, yrefs );
						text( &kmgtm.kfac[jfac_],1, 1 );
						}
					if( lsectc ){
						line( cmgtm.xvpmin, yrefs, cmgtm.xvpmin + 
						 .5*chwid, yrefs );
						}
					}
				}
			}

		/* -- Save axes widths. */
		if( llefax ){
			*widlef = 0.2*chwid + slenmx + slen10;
			}
		else{
			*widlef = 0.;
			}

		}

	/* - Right axis. */

	if( lrigax || lrigtc ){

		/* -- Axes line. */
		line( cmgtm.xvpmax, cmgtm.yvpmin, cmgtm.xvpmax, cmgtm.yvpmax );

		/* -- Put secondary labels on decade before first full one, if necessary. */
		decade = (float)( idecmn - 1 );
		yref = decade*cmgtm.ympwv1 + cmgtm.ympwv2;
		ypmnf = cmgtm.yvpmin - 0.0002;
		ypmxf = cmgtm.yvpmax + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			yrefs = yref + cmgtm.fac[jfac_]*cmgtm.ympwv1;
			if( yrefs >= ypmnf && yrefs <= ypmxf ){
				if( (lsecax && lrigax) && (jfac%isecin) == 0 ){
					xloc = cmgtm.xvpmax + 0.1*chwid;
					settextjust( "LEFT", "CENTER" );
					move( xloc, yrefs );
					text( &kmgtm.kfac[jfac_],1, 1 );
					}
				if( lsectc ){
					line( cmgtm.xvpmax, yrefs, cmgtm.xvpmax - 
					 0.5*chwid, yrefs );
					}
				}
			}

		/* -- Put primary and secondary labels on remainder of axis. */
		strcpy( kdec, "        " );
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin){
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			cnvita( jdec, kdec,9 );
			ljust( kdec,9 );
			yref = decade*cmgtm.ympwv1 + cmgtm.ympwv2;
			if( lrigax ){
				xloc = cmgtm.xvpmax + 0.2*chwid + slen10;
				settextjust( "RIGHT", "CENTER" );
				move( xloc, yref );
				text( "10",3, 2 );
				settextjust( "LEFT", "BOTTOM" );
				nc = indexb( kdec,9 );
				text( kdec,9, nc );
				}
			line( cmgtm.xvpmax, yref, cmgtm.xvpmax - chwid, 
			 yref );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				yrefs = yref + cmgtm.fac[jfac_]*cmgtm.ympwv1;
				if( yrefs <= ypmxf ){
					if( (lsecax && lrigax) && (jfac%isecin) == 0 ){
						xloc = cmgtm.xvpmax + 0.1*chwid;
						settextjust( "LEFT", "CENTER" );
						move( xloc, yrefs );
						text( &kmgtm.kfac[jfac_],1, 1 );
						}
					if( lsectc ){
						line( cmgtm.xvpmax, yrefs, cmgtm.xvpmax - 
						 .5*chwid, yrefs );
						}
					if( cmgem.lygrd ){
						setlinestyle( cmgem.iygrd );
						line( cmgtm.xvpmin, yrefs, cmgtm.xvpmax, 
						 yrefs );
						setlinestyle( 1 );
						}
					}
				}
			}

		/* -- Save axes widths. */
		if( lrigax ){
			*widrig = 0.2*chwid + slen10 + slenmx;
			}
		else{
			*widrig = 0.;
			}

		}

	/* - Grid lines. */

	if( cmgem.lygrd ){
		setlinestyle( cmgem.iygrd );
		decade = (float)( idecmn - 1 );
		yref = decade*cmgtm.ympwv1 + cmgtm.ympwv2;
		ypmnf = cmgtm.yvpmin - 0.0002;
		ypmxf = cmgtm.yvpmax + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			yrefs = yref + cmgtm.fac[jfac_]*cmgtm.ympwv1;
			if( yrefs > ypmnf && yrefs <= ypmxf ){
				line( cmgtm.xvpmin, yrefs, cmgtm.xvpmax, yrefs );
				}
			}
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin){
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			yref = decade*cmgtm.ympwv1 + cmgtm.ympwv2;
			line( cmgtm.xvpmin, yref, cmgtm.xvpmax, yref );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				yrefs = yref + cmgtm.fac[jfac_]*cmgtm.ympwv1;
				if( yrefs <= ypmxf )
					line( cmgtm.xvpmin, yrefs, cmgtm.xvpmax, yrefs );
				}
			}
		}

	/* - Restore linestyle and text justification. */

	setlinestyle( 1 );
	settextjust( khorz, kvert );

L_8888:
	return;

} /* end of function */

