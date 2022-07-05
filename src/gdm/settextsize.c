#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ settextsize(width, height)
float width, height;
{
	void settextsize3(), settextsize4(), settextsize5();


	/*=====================================================================
	 * PURPOSE:  To change size of graphics text.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    width:   The width of a single character in viewport units. [f]
	 *             This width includes the inter character gap.
	 *             The smaller the value the smaller the characters.  For
	 *             example, a value of 0.02 would mean that 50 characters
	 *             of text would fit in a single line of text.
	 *             Note that when graphics quality text is used, the text
	 *             produced is proportionally spaced.  The width in this
	 *             case is that of the widest characters.  In practice you
	 *             will be able to get more that this number of characters
	 *             on a single line.
	 *    height:  The height of a single line in viewport units. [f]
	 *             This height includes the inter line gap.
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:    twidth, thgt
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   settextsize1, settextsize2, settextsize3, settextsize4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If size is different than current size: */
	if( width != cmgdm.twidth || height != cmgdm.thgt ){

		/* -- Save new size. */
		cmgdm.twidth = width;
		cmgdm.thgt = height;

		/* -- Send new text size to active graphics devices. */
		if( Lgdon[1] )
			settextsize1( width, height );
		if( Lgdon[2] )
			settextsize2( width, height );
		if( Lgdon[3] )
			settextsize3( width, height );
		if( Lgdon[4] )
			settextsize4( width, height );
		if( Lgdon[5] )
			settextsize5( width, height );

		}

L_8888:
	return;

} /* end of function */


