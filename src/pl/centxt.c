#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ centxt(ktext, ktext_s, ntext, itextp, tsize)
char *ktext;   int ktext_s;
int ntext, itextp;
double tsize;
{
	float anglsv, slen, tadef, textx, texty, ts, tssave, xpwid, ypwid;


	/* Ind
	 *=====================================================================
	 * PURPOSE: To center a text string relative to current plot.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KTEXT:   Text to be centered.
	 *    NTEXT:   Number of characters in KTEXT.
	 *=====================================================================
	 * MODULE/LEVEL:  GEM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Change to requested character size. */
	cmgem.chht = tsize;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );

	/* - Save character size in case we have to modify it. */

	ts = tsize;
	tssave = cmgem.tscur;
	anglsv = 0.0;

	/* - Compute location of text to be centered on top of plot. */

	if( itextp == cmgem.itop ){

		/* -- Change to horizontal text orientation. */
		settextangle( cmgem.horz );

		/* -- Locate x starting location for text.
		 *    See if text will fit in the available space.
		 *    If not, decrease character size so that text will just fit. */
		xpwid = cmgem.xpmxu - cmgem.xpmnu;
		getstringsize( ktext, ntext, &slen );
		if( slen > xpwid ){
			ts = ts*xpwid/slen;
			cmgem.chht = cmgem.tsdef;
			cmgem.chwid = cmgem.txrat*cmgem.chht;
			settextsize( cmgem.chwid, cmgem.chht );
			}
		textx = cmgem.xpmnu + 0.5*xpwid;

		/* -- Locate y starting location for text. */
		texty = fmin( cmgem.ypmxu + cmgem.axwtop + 0.75*cmgem.chht, 
		 cmgem.yvspmx - 0.6*cmgem.chht );
		cmgem.axwtop = texty + 0.5*cmgem.chht - cmgem.ypmxu;

		/* - Compute location of text to be centered at bottom of plot. */

		}
	else if( itextp == cmgem.ibot ){
		settextangle( cmgem.horz );
		xpwid = cmgem.xpmxu - cmgem.xpmnu;
		getstringsize( ktext, ntext, &slen );
		if( slen > xpwid ){
			ts = ts*xpwid/slen;
			cmgem.chht = cmgem.tsdef;
			cmgem.chwid = cmgem.txrat*cmgem.chht;
			settextsize( cmgem.chwid, cmgem.chht );
			}
		textx = cmgem.xpmnu + 0.5*xpwid;
		texty = fmax( cmgem.ypmnu - cmgem.axwbot - 0.75*cmgem.chht, 
		 cmgem.yvspmn + 0.6*cmgem.chht );
		cmgem.axwbot = cmgem.ypmnu - texty + 0.5*cmgem.chht;

		/* - Compute location of text to be centered to the left of plot. */

		}
	else if( itextp == cmgem.ileft ){
		settextangle( cmgem.vert );
		ypwid = cmgem.ypmxu - cmgem.ypmnu;
		getstringsize( ktext, ntext, &slen );
		if( slen > ypwid ){
			ts = ts*ypwid/slen;
			cmgem.chht = cmgem.tsdef;
			cmgem.chwid = cmgem.txrat*cmgem.chht;
			settextsize( cmgem.chwid, cmgem.chht );
			}
		textx = fmax( cmgem.xpmnu - cmgem.axwlef - 0.75*cmgem.chht, 
		 cmgem.xvspmn + 0.6*cmgem.chht );
		texty = cmgem.ypmnu + 0.5*ypwid;
		cmgem.axwlef = cmgem.xpmnu - textx + 0.5*cmgem.chht;

		/* - Compute location of text to be centered to the right of plot. */

		}
	else{
		settextangle( cmgem.vert );
		ypwid = cmgem.ypmxu - cmgem.ypmnu;
		getstringsize( ktext, ntext, &slen );
		if( slen > ypwid ){
			ts = ts*ypwid/slen;
			cmgem.chht = cmgem.tsdef;
			cmgem.chwid = cmgem.txrat*cmgem.chht;
			settextsize( cmgem.chwid, cmgem.chht );
			}
		textx = fmin( cmgem.xpmxu + cmgem.axwrig + 0.75*cmgem.chht, 
		 cmgem.xvspmx - 0.6*cmgem.chht );
		texty = cmgem.ypmnu + 0.5*ypwid;
		cmgem.axwrig = textx - cmgem.xpmxu + 0.5*cmgem.chht;
		}

	/* - Write centered text at computed location. */

	settextjust( "CENTER", "CENTER" );
	pltext( ktext,ktext_s, textx, texty );

	/* - Restore character size and orientation attributes. */

	cmgem.chht = tssave;
	cmgem.chwid = cmgem.txrat*cmgem.chht;
	settextsize( cmgem.chwid, cmgem.chht );
	settextangle( anglsv );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    841024:  Changed logic to use actual string length to center text.
	 *    831006:  Replaced calls to GETAXW/PUTAXW with inline coding.
	 *    821004:  Minor changes in text positions.
	 *    820331:  Major restructuring.
	 *===================================================================== */

} /* end of function */

