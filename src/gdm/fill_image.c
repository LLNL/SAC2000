#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "gdm.h"


char **fill_image(height, width, data, dmin, range, npseudocolors,
		 nsaccolors, ndefcolors, nerr)

unsigned int height;
unsigned int width;
float data[];  /* data array  */
float dmin, range;
int npseudocolors;
int nsaccolors;
int ndefcolors;
int *nerr;

{
    char **array = NULL ;

    array = (char **) calloc ( MGD , sizeof ( char * ) ) ;
    if ( array == NULL ) {
	*nerr = 301 ;
	setmsg ( "ERROR" , *nerr ) ;
	outmsg () ;
	goto L_8888 ;
    }


    if( Lgdon[1] )
	*nerr = 1;
    if( Lgdon[2] )
	array[1] = fill_image2(height, width, data, dmin, range,
				npseudocolors, nsaccolors, ndefcolors, nerr);
    if( Lgdon[3] )
	array[2] = fill_image3(height, width, data, dmin, range,
				npseudocolors, nsaccolors, ndefcolors, nerr);
    if( Lgdon[4] )
	*nerr = 1;
    if( Lgdon[5] )
	array[4] = fill_image5(height, width, data, dmin, range,
				npseudocolors, nsaccolors, ndefcolors, nerr);

L_8888:
	return array;

} /* end of function */






