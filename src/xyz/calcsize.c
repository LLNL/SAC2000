#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
#include "../../inc/gdm.h"

void calcsize(w_width, w_height, width, height, xmax, xmin, last, first, 
              xpmn, xpmx, ypmn, ypmx, yfactor, nerr)
unsigned int *w_width, *w_height, *width, *height;
float xmax, xmin, last, first;
float xpmn, xpmx, ypmn, ypmx, yfactor;
int *nerr;
{

        *nerr = 0;

        get_geometry(w_width, w_height,  nerr);

        adj_geometry(w_width, w_height, nerr);

        *width = (float)(*w_width) * (xpmx - xpmn);
/*  adjust image width for the offset to the begin time of the spectrogram */
 
        *width = (float)(*width) * ((xmax-xmin)/(last - first));   

        *height = (float)(*w_height) * yfactor * (ypmx - ypmn);

L_8888:
	return;
} /* end of function */







