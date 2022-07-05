#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ getstringsize(ktext, ntext, width)
char *ktext;
int ntext;
float *width;
{
	int ich, j, j_;
	float wfactr;



	/*=====================================================================
	 * PURPOSE:  To determine the width of a text string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ktext:   Text string. [c]
	 *    ntext:   Number of characters in text string. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    width:   Width of text string in viewport coordinates. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:     ltsoft, twidth, iofset, stxmax, stxmin
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861017:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	/* - If software quality text is on, compute string width. */
	if( cmgdm.ltsoft ){

		/* -- Determine width factor and initialize string length. */
		wfactr = cmgdm.twidth/(float)( cmgdm.iofset );
		*width = 0.;

		/* -- For each character in string.
		 * --- Convert character to it's index number in font array.
		 * --- Sum up incremental character lengths. */
		for( j = 1; j <= ntext; j++ ){
			j_ = j - 1;
			ich = ( ktext[j - 1] );
			if( ich > 128 )
				ich = ich - 128;
			*width = *width + (Stxmax[ich] - Stxmin[ich])*wfactr;
			}

		/* - Otherwise, compute hardware (monospaced) text width. */

		}
	else{

		*width = (float)( ntext )*cmgdm.twidth;

		}

L_8888:
	return;

} /* end of function */

