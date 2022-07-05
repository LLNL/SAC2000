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
void /*FUNCTION*/ xlogax()
{
	char _c0[2], kdec[9];
	int lsecax, lsectc;
	int _d_l, _d_m, _do0, _do1, _do2, _do3, _do4, _do5, _do6, 
	 _do7, idecin, idecmn, idecmx, isecin, jdec, jdec_, jfac, jfac_, 
	 nc, ndivu;
	float _f0, _f1, decade, decmn, decmx, decsiz, skfudge, slen, slen10, 
	 slenmx, xpmnf, xpmxf, xref, xrefs, xvpmax, xvpmin, yloc, yvpmax, 
	 yvpmin;


	/*=====================================================================
	 * PURPOSE:  To produce a logarithmically-scaled axis at the bottom
	 *           and/or top of the current plot window.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GEM:     XIMNZ, XIMXZ, XPMNU, XPMXU, LLOGLB,
	 *             'LEFT', 'CENTER', 'RIGHT', 'BOTTOM', 'TOP',
	 *             LBOTAX, LBOTTC, LTOPAX, LTOPTC,
	 *             XMPIP1, XMPIP2, LXGRD, ISOLID, IDOT, FAC(),
	 *             ISKWIDTH, ITHIN, IWIDTH, SKDEVFUDGE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GEM:     AXWBOT, AXWTOP
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVITA, LJUST, LINE, PLTEXT, SETLINESTYLE, SETLINEWIDTH
	 *             GETVPORT
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    MDECLB:  Maximum number of labeled decades.
	 *    FSECAX:  Factor used to determine secondary labels.
	 *    FSECTC:  Factor used to determine secondary tick marks.
	 *    LSECAX:  .TRUE. if there is enough room for secondary axis labels.
	 *    LSECTC:  .TRUE. if there is enough room for secondary tick marks.
	 *    xskfudge:Fudge factor when skeleton line-width gt 1. [f]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920526:  Added line-width. TEXT is always thin!
	 *    830927:  Moved grid drawing logic into its own do loop.
	 *    820930:  Cleanup up and documented.
	 *    810120:  Original PRIME version.
	 *===================================================================== */
	/* PROCEDURE: */

        settextangle(cmgem.horz);

	/* - Label every decade if there are no more than MDECLB of them. */
	decmn = cmgem.ximnz;
	if( decmn <= 0. ){
		idecmn = decmn;
		}
	else{
		idecmn = decmn + 0.999;
		}
	decmx = cmgem.ximxz;
	if( decmx >= 0. ){
		idecmx = decmx;
		}
	else{
		idecmx = decmx - 0.999;
		}
	ndivu = idecmx - idecmn + 1;
	idecin = ndivu/MDECLB;
	if( idecin < 1 )
		idecin = 1;

	/* - Determine if there is room for secondary labels and/or tick marks. */

	decsiz = fabs( cmgem.xpmxu - cmgem.xpmnu )/(cmgem.ximxz - cmgem.ximnz);
	isecin = 1;
	if( (decsiz > FSECAX*cmgem.chwid && idecin == 1) && cmgem.lloglb ){
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
	for( jdec = idecmn; jdec <= idecmx; jdec += idecin){
		jdec_ = jdec - 1;
		cnvita( jdec, kdec,9 );
		ljust( kdec,9 );
		nc = indexb( kdec,9 );
		getstringsize( kdec, nc, &slen );
		slenmx = fmax( slenmx, slen );
		}
	getstringsize( "10", 2, &slen10 );

	/* - Determine axes fudge factor for thick axes lines. */
	getvport( &xvpmin, &xvpmax, &yvpmin, &yvpmax );
	skfudge = cmgem.skdevfudge*((yvpmin - yvpmax)/(xvpmin - xvpmax));

	/* - Draw the bottom axes. */

	setlinestyle( cmgem.isolid );
	setlinewidth( cmgem.iskwidth );

	if( cmgem.lbotax || cmgem.lbottc ){

		/* -- Botton Axes line. */
		if( cmgem.iskwidth > cmgem.ithin ){
			line( cmgem.xpmnu - cmgem.iskwidth*skfudge, cmgem.ypmnu, 
			 cmgem.xpmxu + cmgem.iskwidth*skfudge, cmgem.ypmnu );
			}
		else{
			line( cmgem.xpmnu, cmgem.ypmnu, cmgem.xpmxu, cmgem.ypmnu );
			}

		/* -- Put secondary labels on decade before first full one, if necessary. */

		decade = (float)( idecmn - 1 );
		xref = decade*cmgem.xmpip1 + cmgem.xmpip2;
		xpmnf = cmgem.xpmnu - 0.0002;
		xpmxf = cmgem.xpmxu + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			xrefs = xref + Fac[jfac]*cmgem.xmpip1;
			if( xrefs >= xpmnf && xrefs <= xpmxf ){
				if( (lsecax && cmgem.lbotax) && (jfac%isecin) == 0 ){
					yloc = cmgem.ypmnu - 0.1*cmgem.chht;
					settextjust( "CENTER", "TOP" );
					pltext( &Kfac[jfac],1, xrefs, yloc );
					setlinewidth( cmgem.iskwidth );
					}
				if( lsectc ){
					line( xrefs, cmgem.ypmnu, xrefs, cmgem.ypmnu + 
					 0.5*cmgem.chwid );
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
			xref = decade*cmgem.xmpip1 + cmgem.xmpip2;
			if( cmgem.lbotax ){
				yloc = cmgem.ypmnu - 1.2*cmgem.chht;
				settextjust( "RIGHT", "TOP" );
				pltext( "10",3, xref, yloc );
				settextjust( "LEFT", "CENTER" );
				pltext( kdec,9, xref, yloc );
				setlinewidth( cmgem.iskwidth );
				}
			line( xref, cmgem.ypmnu, xref, cmgem.ypmnu + 
			 cmgem.chwid );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				xrefs = xref + Fac[jfac]*cmgem.xmpip1;
				if( xrefs <= xpmxf ){
					if( (lsecax && cmgem.lbotax) && (jfac%isecin) == 
					 0 ){
						yloc = cmgem.ypmnu - 0.1*cmgem.chht;
						settextjust( "CENTER", "TOP" );
						pltext( &Kfac[jfac],1, xrefs, yloc );
						setlinewidth( cmgem.iskwidth );
						}
					if( lsectc ){
						line( xrefs, cmgem.ypmnu, xrefs, cmgem.ypmnu + 
						 0.5*cmgem.chwid );
						}
					}
				}
			}

		/* -- Save axes widths. */
		if( cmgem.lbotax ){
			cmgem.axwbot = 2.2*cmgem.chht;
			}
		else{
			cmgem.axwbot = 0.;
			}

		}

	/* - Top axis. */

	if( cmgem.ltopax || cmgem.ltoptc ){

		/* -- Top Axes line. */
		if( cmgem.iskwidth > cmgem.ithin ){
			line( cmgem.xpmnu - cmgem.iskwidth*skfudge, cmgem.ypmxu, 
			 cmgem.xpmxu + cmgem.iskwidth*skfudge, cmgem.ypmxu );
			}
		else{
			line( cmgem.xpmnu, cmgem.ypmxu, cmgem.xpmxu, cmgem.ypmxu );
			}

		/* -- Put secondary labels on decade before first full one, if necessary. */
		decade = (float)( idecmn - 1 );
		xref = decade*cmgem.xmpip1 + cmgem.xmpip2;
		xpmnf = cmgem.xpmnu - 0.0002;
		xpmxf = cmgem.xpmxu + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			xrefs = xref + Fac[jfac]*cmgem.xmpip1;
			if( xrefs >= xpmnf && xrefs <= xpmxf ){
				if( (lsecax && cmgem.ltopax) && (jfac%isecin) == 0 ){
					yloc = cmgem.ypmxu + 0.1*cmgem.chht;
					settextjust( "CENTER", "BOTTOM" );
					pltext( &Kfac[jfac],1, xrefs, yloc );
					setlinewidth( cmgem.iskwidth );
					}
				if( lsectc ){
					line( xrefs, cmgem.ypmxu, xrefs, cmgem.ypmxu - 
					 0.5*cmgem.chwid );
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
			xref = decade*cmgem.xmpip1 + cmgem.xmpip2;
			if( cmgem.ltopax ){
				yloc = cmgem.ypmxu + 1.2*cmgem.chht;
				settextjust( "RIGHT", "CENTER" );
				pltext( "10",3, xref, yloc );
				settextjust( "LEFT", "BOTTOM" );
				pltext( kdec,9, xref, yloc );
				setlinewidth( cmgem.iskwidth );
				}
			line( xref, cmgem.ypmxu, xref, cmgem.ypmxu - 
			 cmgem.chwid );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				xrefs = xref + Fac[jfac]*cmgem.xmpip1;
				if( xrefs <= xpmxf ){
					if( (lsecax && cmgem.ltopax) && (jfac%isecin) == 
					 0 ){
						yloc = cmgem.ypmxu + 0.1*cmgem.chht;
						settextjust( "CENTER", "BOTTOM" );
						pltext( &Kfac[jfac],1, xrefs, yloc );
						setlinewidth( cmgem.iskwidth );
						}
					if( lsectc ){
						line( xrefs, cmgem.ypmxu, xrefs, cmgem.ypmxu - 
						 0.5*cmgem.chwid );
						}
					}
				}
			}

		/* -- Save axes widths. */
		if( cmgem.ltopax ){
			cmgem.axwtop = 2.2*cmgem.chht;
			}
		else{
			cmgem.axwtop = 0.;
			}

		}

	/* - Grid lines. */

	if( cmgem.lxgrd ){
		setlinestyle( cmgem.ixgrd );
		setlinewidth( cmgem.ithin );
		decade = (float)( idecmn - 1 );
		xref = decade*cmgem.xmpip1 + cmgem.xmpip2;
		xpmnf = cmgem.xpmnu - 0.0002;
		xpmxf = cmgem.xpmxu + 0.0002;
		for( jfac = 2; jfac <= 9; jfac++ ){
			jfac_ = jfac - 1;
			xrefs = xref + Fac[jfac]*cmgem.xmpip1;
			if( xrefs > xpmnf && xrefs <= xpmxf ){
				line( xrefs, cmgem.ypmnu, xrefs, cmgem.ypmxu );
				}
			}
		for( jdec = idecmn; jdec <= idecmx; jdec += idecin ){
			jdec_ = jdec - 1;
			decade = (float)( jdec );
			xref = decade*cmgem.xmpip1 + cmgem.xmpip2;
			line( xref, cmgem.ypmnu, xref, cmgem.ypmxu );
			for( jfac = 2; jfac <= 9; jfac++ ){
				jfac_ = jfac - 1;
				xrefs = xref + Fac[jfac]*cmgem.xmpip1;
				if( xrefs <= xpmxf )
					line( xrefs, cmgem.ypmnu, xrefs, cmgem.ypmxu );
				}
			}
		setlinestyle( cmgem.isolid );
		setlinewidth( cmgem.iwidth );
		}

L_8888:
	return;

} /* end of function */

