#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void /*FUNCTION*/ polyline(xloc, yloc, number)
float xloc[], yloc[];
int *number;
{
	int j, j_;
	void drawpoly3(), drawpoly4(), drawpoly5();

	float *const Xloc = &xloc[0] - 1;
	float *const Yloc = &yloc[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To draw a line through a set of viewport locations.
	 *=====================================================================
	 * SPECIAL NOTE:  This polyline subroutine does NOT conform to the
	 *                SIGGRAPH standard in that it moves to the first
	 *                data point as opposed to drawing to it from the CP.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    xloc:    Array of x viewport coordinates. [fa]
	 *    yloc:    Array of y viewport coordinates. [fa]
	 *    number:  Length of xloc and yloc arrays. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  gtm/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:   move, draw
	 *=====================================================================
	 * MODIFICATION HISTORYLOC:
	 *    910826:  Move declarations to after the include statements to 
	 *             improve portability to DEC 5000 per Gyu-sang Jang @ UC Davis.
	 *    910212:  add differential processing based on lvsclip;
	 *             add calls to drawpoly3/4 for X & SunView      (jjy)
	 *    831026:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861017
	 *===================================================================== */
	/* PROCEDURE: */
	if( cmgdm.lvsclip ){

		move( Xloc[1], Yloc[1] );
		for( j = 2; j <= *number; j++ ){
			j_ = j - 1;
			/*         call draw(xloc(j),yloc(j),number) */
			draw( Xloc[j], Yloc[j] );
			}

		}
	else{

		if( Lgdon[1] ){
			move1( &Xloc[1], &Yloc[1] );
			for( j = 2; j <= *number; j++ ){
				j_ = j - 1;
				draw1( &Xloc[j], &Yloc[j] );
				}
			}

		if( Lgdon[2] ){
			move2( Xloc[1], Yloc[1] );
			for( j = 2; j <= *number; j++ ){
				j_ = j - 1;
				draw2( Xloc[j], Yloc[j] );
				}
			}

		if( Lgdon[3] )
			drawpoly3( xloc, yloc, number );

		if( Lgdon[4] )
			drawpoly4( xloc, yloc, number );

		if( Lgdon[5] )
			drawpoly5( xloc, yloc, number );

		}

L_8888:
	return;

} /* end of function */

