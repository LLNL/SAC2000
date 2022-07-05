#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ softwaretext(ktext, ntext)
char *ktext;
int ntext;
{
	int italic, rotate;
	int ich, index, j, j_, jopcode, jstroke, jstxmn, jstxmx, 
	 jxdel, jydel;
	float _f0, _f1, hfactr, rotrad, sqxy, theta, wfactr, xdel, xsav, 
	 ydel, ysav;


	/*=====================================================================
	 * *** INTERNAL SUBROUTINE:  NOT NORMALLY CALLED BY USER ***
	 *=====================================================================
	 * PURPOSE:  To display a text string using current software font.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktext:   Text string. [c]
	 *    nctext:  Length of KTEXT. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     twidth, thgt, iofset, iyhigh, tangle, nfont,
	 *             ascstr, stxmin, stxmax, stroke, xold, yold
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  move, draw
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    rotate:  .TRUE. if text is to be rotated from horizontal. [l]
	 *    italic:  .TRUE. if text is to be italicized. [l]
	 *    [xy]sav: Starting point for each character. [f]
	 *    jstroke: Current stroke from stroke table. [i]
	 *             Each stroke has a delta-location (x and y) and an opcode
	 *             encoded into it.
	 *    jopcode: Op code for current stroke. [i]
	 *             = 0 Move to location.
	 *             = 1 Draw to location.
	 *             = 2 Final move for this character.
	 *    j[xy]del: Delta location in normalized units. [i]
	 *    [xy]del:  Delta location scaled to current character size. [f]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861103:  Added check to make sure rotation is needed.
	 *    861017:  Major restructuring.
	 *    831026:  Original Version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861103
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Convert current values of width, height, and angle to the
	 *   units needed by this subroutine. */
	wfactr = cmgdm.twidth/cmgdm.iofset;
	hfactr = cmgdm.thgt/cmgdm.iyhigh;
	rotate = cmgdm.tangle != 0.0;
	if( rotate )
		rotrad = cmgdm.tangle*TORAD;
	italic = (cmgdm.nfont%2) == 0;

	/* - For each character in text string. */

	for( j = 1; j <= ntext; j++ ){
		j_ = j - 1;

		/* -- Convert ASCII value into its integer equivalent. */
		ich = ( ktext[j - 1] );
		if( ich > 128 )
			ich = ich - 128;

		/* -- Get index in stroke table for character and its width limits. */
		index = Ascstr[ich];
		jstxmn = Stxmin[ich];
		jstxmx = Stxmax[ich];

		/* -- Keep character starting point because all values in stroke
		 *    table use this reference point. */
		xsav = cmgdm.xold;
		ysav = cmgdm.yold;

		/* -- For each stroke in this character.
		 * --- Get opcode and x and y location from stroke table. */
L_40:
		jstroke = Stroke[index];
		jopcode = jstroke/4096;
		jxdel = jstroke/64 - jopcode*64;
		jydel = jstroke - 64*(jstroke/64);
		jxdel = jxdel - jstxmn;
		jydel = jydel - cmgdm.iyoff;
		/* --- Multiply by width and heigth factors. */
		xdel = jxdel*wfactr;
		ydel = jydel*hfactr;
		/* --- If "italic" font, slant the character. */
		if( italic )
			xdel = xdel + 0.414*ydel;
		/* --- Do rotation. */
		if( rotate ){
			sqxy = pow(powi(xdel,2) + powi(ydel,2),.5);
			theta = atan( ydel/xdel );
			xdel = sqxy*cos( theta + rotrad );
			ydel = sqxy*sin( theta + rotrad );
			}
		/* --- Depending on opcode do move or draw to delta location. */
		if( jopcode == 0 || jopcode == 2 ){
			move( xsav + xdel, ysav + ydel );
			}
		else{
			draw( xsav + xdel, ysav + ydel );
			}

		/* -- Do next stroke. */
		index = index + 1;
		if( jopcode <= 1 )
			goto L_40;

		/* -- Final stroke for this character has been reached.
		 *    Move to next character location. */
		jxdel = jstxmx - jstxmn;
		ydel = 0.;
		xdel = jxdel*wfactr;
		if( rotate ){
			ydel = xdel*sin( rotrad );
			xdel = xdel*cos( rotrad );
			}
		move( xsav + xdel, ysav + ydel );

		/* - Do next character. */

		}

L_8888:
	return;

} /* end of function */

