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
#include "../../inc/gem.h"
void /*FUNCTION*/ ylogax()
{
	char _c0[2], kdec[9];
	int lsecax, lsectc;
	int _d_l, _d_m, _do0, _do1, _do2, _do3, _do4, _do5, _do6, 
	 _do7, idecin, idecmn, idecmx, isecin, jdec, jdec_, jfac, jfac_, 
	 nc, ndivu, nerr;
	float _f0, _f1, decade, decmn, decmx, decsiz, skfudge, slen, slen10, 
	 slenmx, xloc, xvpmax, xvpmin, ypmnf, ypmxf, yref, yrefs, yvpmax, 
	 yvpmin;


	/*=====================================================================
	 * PURPOSE:  To produce a logarithmically-scaled axis to the right
	 *           and/or left of the current plot window.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     YIMNZ, YIMXZ, YPMNU, YPMXU, LLOGLB,
	 *             'LEFT', 'CENTER', 'RIGHT', 'BOTTOM', 'TOP',
	 *             LRIGAX, LRIGTC, LLEFAX, LLEFTC,
	 *             YMPIP1, YMPIP2, LYGRD, FAC(),
	 *             ITHIN, ISKWIDTH, IWIDTH, SKDEVFUDGE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     AXWRIG, AXWLEF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVITA, LJUST, LINE, PLTEXT, GETSTRINGSIZE
	 *             SETLINEWIDTH, GETVPORT
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    MDECLB:  Maximum number of labeled decades.
	 *    FSECAX:  Factor used to determine secondary labels.
	 *    FSECTC:  Factor used to determine secondary tick marks.
	 *    LSECAX:  .TRUE. if there is enough room for secondary axis labels.
	 *    LSECTC:  .TRUE. if there is enough room for secondary tick marks.
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Label every decade if there are no more than MDECLB of them. */
	decmn = cmgem.yimnz;
	if( decmn <= 0. ){
		idecmn = decmn;
		}
	else{
		idecmn = decmn + 0.9999999;
		}
	decmx = cmgem.yimxz;
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

	decsiz = fabs( cmgem.ypmxu - cmgem.ypmnu )/(cmgem.yimxz - cmgem.yimnz);
	isecin = 1;
	if( (decsiz > FSECAX*cmgem.chht && idecin == 1) && cmgem.lloglb ){
		lsecax = TRUE;
		isecin = 2;
		if( decsiz > 2.*FSECAX*cmgem.chht )
			isecin = 1;
		}
	else{
		lsecax = FALSE;
		}
	if( decsiz > FSECTC*cmgem.chht && idecin == 1 ){
		lsectc = TRUE;
		}
	else{
		lsectc = FALSE;
		}

	/* - Calculate width of exponent. */

	slenmx = 0.;
	for( jdec = idecmn; jdec <= idecmx; jdec += idecin ){
		jdec_ = jdec - 1;
		cnvita( jdec, kdec,9 );
		ljust( kdec,9 );
		nc = indexb( kdec,9 );
		getstringsize( kdec, nc, &slen );
		slenmx = fmax( slenmx, slen );
		}
	getstringsize( "10", 2, &slen10 );

	/* - Determine the axes fudge factor for thick axes lines. */
	getvport( &xvpmin, &xvpmax, &yvpmin, &yvpmax );
	skfudge = cmgem.skdevfudge*((yvpmin - yvpmax)/(xvpmin - xvpmax));

	/* - Draw the left axis. */

	setlinestyle( cmgem.isolid );
	setlinewidth( cmgem.iskwidth );

	if( cmgem.llefax || cmgem.lleftc ){

		/* -- Left Axes line. */
		if( cmgem.iskwidth > cmgem.ithin ){
			line( cmgem.xpmnu, cmgem.ypmnu - cmgem.iskwidth*
			 skfudge, cmgem.xpmnu, cmgem.ypmxu + cmgem.iskwidth*
			 skfudge );
			}
		else{
			line( cmgem.xpmnu, cmgem.ypmnu, cmgem.xpmnu, cmgem.ypmxu );
			}

		/* -- Put secondary labels on decade before first full one, if necessary. */
		decade = (float)( idecmn - 1 );
		yref = decade*cmgem.ympip1 + cmgem.ympip2;
		ypmnf = cmgem.ypmnu - 0.0002;
		ypmxf = cmgem.ypmxu + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			yrefs = yref + Fac[jfac]*cmgem.ympip1;
			if( yrefs >= ypmnf && yrefs <= ypmxf ){
				if( (lsecax && cmgem.llefax) && (jfac%isecin) == 0 ){
					xloc = cmgem.xpmnu - 0.1*cmgem.chwid;
					settextjust( "RIGHT", "CENTER" );
					pltext( &Kfac[jfac],1, xloc, yrefs );
					setlinewidth( cmgem.iskwidth );
					}
				if( lsectc ){
					line( cmgem.xpmnu, yrefs, cmgem.xpmnu + 
					 0.5*cmgem.chwid, yrefs );
					}
				}
			}

		/* -- Put primary and secondary labels on remainder of axis. */
		strcpy( kdec, "        " );
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin ){
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			cnvita( jdec, kdec,9 );
			ljust( kdec,9 );
			yref = decade*cmgem.ympip1 + cmgem.ympip2;
			if( cmgem.llefax ){
				xloc = cmgem.xpmnu - 0.2*cmgem.chwid - slenmx;
				settextjust( "RIGHT", "CENTER" );
				pltext( "10",3, xloc, yref );
				settextjust( "LEFT", "BOTTOM" );
				pltext( kdec,9, xloc, yref );
				setlinewidth( cmgem.iskwidth );
				}
			line( cmgem.xpmnu, yref, cmgem.xpmnu + cmgem.chwid, 
			 yref );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				yrefs = yref + Fac[jfac]*cmgem.ympip1;
				if( yrefs <= ypmxf ){
					if( (lsecax && cmgem.llefax) && (jfac%isecin) == 
					 0 ){
						xloc = cmgem.xpmnu - cmgem.chwid;
						settextjust( "LEFT", "CENTER" );
						pltext( &Kfac[jfac],1, xloc, yrefs );
						setlinewidth( cmgem.iskwidth );
						}
					if( lsectc ){
						line( cmgem.xpmnu, yrefs, cmgem.xpmnu + 
						 .5*cmgem.chwid, yrefs );
						}
					}
				}
			}

		/* -- Save axes widths. */
		if( cmgem.llefax ){
			cmgem.axwlef = 0.2*cmgem.chwid + slenmx + slen10;
			}
		else{
			cmgem.axwlef = 0.;
			}

		}

	/* - Right axis. */

	if( cmgem.lrigax || cmgem.lrigtc ){

		/* -- Right Axes line. */
		if( cmgem.iskwidth > cmgem.ithin ){
			line( cmgem.xpmxu, cmgem.ypmnu - cmgem.iskwidth*
			 skfudge, cmgem.xpmxu, cmgem.ypmxu + cmgem.iskwidth*
			 skfudge );
			}
		else{
			line( cmgem.xpmxu, cmgem.ypmnu, cmgem.xpmxu, cmgem.ypmxu );
			}

		/* -- Put secondary labels on decade before first full one, if necessary. */
		decade = (float)( idecmn - 1 );
		yref = decade*cmgem.ympip1 + cmgem.ympip2;
		ypmnf = cmgem.ypmnu - 0.0002;
		ypmxf = cmgem.ypmxu + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			yrefs = yref + Fac[jfac]*cmgem.ympip1;
			if( yrefs >= ypmnf && yrefs <= ypmxf ){
				if( (lsecax && cmgem.lrigax) && (jfac%isecin) == 0 ){
					xloc = cmgem.xpmxu + 0.1*cmgem.chwid;
					settextjust( "LEFT", "CENTER" );
					pltext( &Kfac[jfac],1, xloc, yrefs );
					}
				if( lsectc ){
					line( cmgem.xpmxu, yrefs, cmgem.xpmxu - 
					 0.5*cmgem.chwid, yrefs );
					}
				}
			}

		/* -- Put primary and secondary labels on remainder of axis. */
		strcpy( kdec, "        " );
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin ){
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			cnvita( jdec, kdec,9 );
			ljust( kdec,9 );
			yref = decade*cmgem.ympip1 + cmgem.ympip2;
			if( cmgem.lrigax ){
				xloc = cmgem.xpmxu + 0.2*cmgem.chwid + slen10;
				settextjust( "RIGHT", "CENTER" );
				pltext( "10",3, xloc, yref );
				settextjust( "LEFT", "BOTTOM" );
				pltext( kdec,9, xloc, yref );
				setlinewidth( cmgem.iskwidth );
				}
			line( cmgem.xpmxu, yref, cmgem.xpmxu - cmgem.chwid, 
			 yref );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				yrefs = yref + Fac[jfac]*cmgem.ympip1;
				if( yrefs <= ypmxf ){
					if( (lsecax && cmgem.lrigax) && (jfac%isecin) == 
					 0 ){
						xloc = cmgem.xpmxu + 0.1*cmgem.chwid;
						settextjust( "LEFT", "CENTER" );
						pltext( &Kfac[jfac],1, xloc, yrefs );
						setlinewidth( cmgem.iskwidth );
						}
					if( lsectc ){
						line( cmgem.xpmxu, yrefs, cmgem.xpmxu - 
						 .5*cmgem.chwid, yrefs );
						}
					if( cmgem.lygrd ){
						setlinestyle( cmgem.iygrd );
						setlinewidth( cmgem.ithin );
						line( cmgem.xpmnu, yrefs, cmgem.xpmxu, 
						 yrefs );
						setlinestyle( cmgem.isolid );
						setlinewidth( cmgem.iwidth );
						}
					}
				}
			}

		/* -- Save axes widths. */
		if( cmgem.lrigax ){
			cmgem.axwrig = 0.2*cmgem.chwid + slen10 + slenmx;
			}
		else{
			cmgem.axwrig = 0.;
			}

		}

	/* - Grid lines. */

	if( cmgem.lygrd ){
		setlinestyle( cmgem.iygrd );
		setlinewidth( cmgem.ithin );
		decade = (float)( idecmn - 1 );
		yref = decade*cmgem.ympip1 + cmgem.ympip2;
		ypmnf = cmgem.ypmnu - 0.0002;
		ypmxf = cmgem.ypmxu + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			yrefs = yref + Fac[jfac]*cmgem.ympip1;
			if( yrefs > ypmnf && yrefs <= ypmxf ){
				line( cmgem.xpmnu, yrefs, cmgem.xpmxu, yrefs );
				}
			}
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin ){ 
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			yref = decade*cmgem.ympip1 + cmgem.ympip2;
			line( cmgem.xpmnu, yref, cmgem.xpmxu, yref );

			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				yrefs = yref + Fac[jfac]*cmgem.ympip1;
				if( yrefs <= ypmxf )
					line( cmgem.xpmnu, yrefs, cmgem.xpmxu, yrefs );
				}
			}
		setlinestyle( cmgem.isolid );
		setlinewidth( cmgem.iwidth );
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    920530:  Added WDITH option. TEXT is always thin!
	 *    830927:  Moved grid drawing logic to its own do loop.
	 *    821001:  Cleaned up and documented.
	 *    810120:  Original version.
	 *===================================================================== */

} /* end of function */

