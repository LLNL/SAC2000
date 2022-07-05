#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "gdm.h"


char **fill_colorbar(npseudocolors, width, npricolors, ndefcolors, nerr)

int npseudocolors;   /* number of pseudocolors in the colortable */
int width;       /* number of elements in one scan line of the color bar */
int npricolors;  /* number of SAC primary colors */
int ndefcolors;  /* number of default colors in the colortable */
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
	array[1] = fill_clrbar2(npseudocolors,width,npricolors,ndefcolors,nerr);
    if( Lgdon[3] )
	array[2] = fill_clrbar3(npseudocolors,width,npricolors,ndefcolors,nerr);
    if( Lgdon[4] )
	*nerr = 1;
    if( Lgdon[5] )
	array[4] = fill_clrbar5(npseudocolors,width,npricolors,ndefcolors,nerr);

L_8888:
	return array;

} /* end of function */

