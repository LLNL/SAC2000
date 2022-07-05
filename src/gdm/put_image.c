#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "gdm.h"


void put_image5(char* data, unsigned int xloc, unsigned int yloc, unsigned int width, unsigned int height, int* nerr);
void put_image(data, xloc, yloc, width, height, nerr)
char **data;
unsigned int xloc, yloc;
unsigned int width, height;
int *nerr;
{
	int idx ;

	/*=====================================================================
	 * PURPOSE:  To do an image plot with limited options.
	 *           Need to add options to allow user to specify size of
         *           image.  Default is the size of the raw data.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *      array:  Two-dimensional array of data. [fa]  
	 *     nxsize:  Number of elements in the x (horizontal) direction. [i]
	 *     nysize:  Number of elements in the y (vertical) direction. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL: contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    gtm:         xvpmin, xvpmax, yvpmin, yvpmax
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *     saclib:  move, draw, setcolorname
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;


	if( Lgdon[1] )
            *nerr = 1;
	if( Lgdon[2] )
            put_image2(data[1], xloc, yloc, width, height, nerr);
	if( Lgdon[3] ) {
            put_image3(data[2], xloc, yloc, width, height, nerr);
	    data[ 2 ] = NULL ;
	}
	if( Lgdon[4] )
            *nerr = 1;
	if( Lgdon[5] )
            put_image5(data[4], xloc, yloc, width, height, nerr);

L_8888:

	if ( data ) {
	    for ( idx = 0 ; idx < MGD ; idx++ ) {
		if ( data[idx] )
		    free ( data[idx] ) ;
	    }

	    free ( data ) ;
	}

	return;

} /* end of function */

