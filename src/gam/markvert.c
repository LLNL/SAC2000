#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ markvert(jmark1, jmark2, xloc, ytop, ydel, klabel, 
	 klabel_s, nmarks)
int jmark1, jmark2;
float *xloc;
double ytop, ydel;
char *klabel;   int klabel_s;
int nmarks;
{
	int jmark, jmark_, nc;
	float _f0, _f1, height, width, ydiff, yloc1, yloc2, ymax;


	/*=====================================================================
	 * PURPOSE: Writes the vertical line and accompanying text on ppk plot.
	 *=====================================================================
	 * USAGE:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *  GEM:  LWIDTH, IWIDTH, ITHIN
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 * 920602: Added line-width to vertical marker line.
	 * 920602: Original Version Date Unknown.  Added documentation 6/92.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */
	/* - Set text justification and get current text size. */
	settextjust( "LEFT", "TOP" );
	gettextsize( &width, &height );

	/* - Draw and label marker line on requested subplots. */

	for( jmark = jmark1; jmark <= jmark2; jmark++ ){
		jmark_ = jmark - 1;
		yloc1 = ytop - ((float)( jmark - 1 ) + 0.05)*ydel;
		yloc2 = yloc1 - 0.90*ydel;
		setlinewidth( cmgem.iwidth );
		line( *xloc, yloc1, *xloc, yloc2 );
		setlinewidth( cmgem.ithin );
		nc = indexb( klabel,klabel_s );
		if( nc > 0 ){
			ydiff = (float)( nmarks )*height + 0.005;
			move( *xloc + 0.005, yloc1 - ydiff );
			text( klabel,klabel_s, nc );
			}
		}

	/* - Reset text justification. */

	settextjust( "LEFT", "BOTTOM" );

L_8888:
	return;

} /* end of function */

