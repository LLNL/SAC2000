#define HT_ADJUST 32
#define WD_ADJUST 32

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gdm.h"


void adj_geometry(width, height, nerr)
unsigned int *width, *height;
int *nerr;
{


	if( Lgdon[1] )
            *nerr = 1;
	if( Lgdon[2] ){
            *width /= WD_ADJUST;
            *height /= HT_ADJUST;
            *nerr = 0;
	}
	if( Lgdon[3] )
            *nerr = 0;
	if( Lgdon[4] )
            *nerr = 1;
        if( Lgdon[5] )
            *nerr = 0;

L_8888:
	return;
} /* end of function */







