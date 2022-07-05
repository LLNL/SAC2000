#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
#include "../../inc/sgfcolor.h"

char *fill_image2(height, width, data, dmin, range, npseudocolors, nsaccolors, ndefcolors, nerr)

unsigned int height;   /*  */
unsigned int width;   /*  */
float data[];  /* data array  */
float dmin, range;
int npseudocolors;
int nsaccolors;
int ndefcolors;
int *nerr;
{

  char *array;
  int i,j,isave;
  float fsave;
  float wrange;

  wrange = range == 0.0 ? RNDOFF: range;
 
/* allocate the output array */


  if((array  = (char *)malloc(3*width*height*sizeof(char)))
                 == NULL){
    printf("error allocating image data byte array--fill_image2\n");
    *nerr = 0301;
    goto L_8888;
  };


/*  Standard algorithm to scale data to 0-(npseudocolors-1) range */

        *nerr = 0;

        isave = 0;
        for (i = height-1; i >= 0; i--){
          for (j = 0; j < width; j++)     {
          fsave = (((data[(i*width)+j] - dmin)/wrange)*(float)(npseudocolors));
          fsave = ( fsave < 0.0 ) ? 0.0 : fsave;
          fsave = ( fsave > (float)(npseudocolors-1)) ? (float)(npseudocolors-1) : fsave;
          array[isave++] = sred[(unsigned int)fsave];
          array[isave++] = sgreen[(unsigned int)fsave];
          array[isave++] = sblue[(unsigned int)fsave];
          }
        }


L_8888:
	return array;

} /* end of function */






