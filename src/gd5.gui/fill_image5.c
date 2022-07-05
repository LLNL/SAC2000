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
#include "../../inc/gd5.gui.h"


char *fill_image5(height, width, data, dmin, range, npseudocolors, nsaccolors, ndefcolors, nerr)

unsigned int height; 
unsigned int width;  
float data[]; 
float dmin, range;
int npseudocolors;
int nsaccolors;
int ndefcolors;
int *nerr;
{

  int i, j;
  float fsave,fmin,fmax;
  int isave;
  char *array;
  float wrange;

  wrange = range == 0.0 ? RNDOFF: range;

  if((array  = (char *)malloc(width*height*sizeof(char)))
                 == NULL){
    printf("error allocating image data byte array--fill_image5\n");
    *nerr = 0301;
    goto L_8888;
  };

/*  Standard algorithm to scale data to (cmgdm.nctsize-(cmgdm.nctsize+npscolors-1)) range */
  
        *nerr = 0;

        fmin = (float)nsaccolors+1.0+(float)ndefcolors;
        fmax = (float)(nsaccolors+npseudocolors+ndefcolors);
        isave = 0;
        for (i = height-1; i >= 0; i--){
          for (j = 0; j < width; j++)     {
          fsave = (((data[(i*width)+j] - dmin)/wrange)*(float)(npseudocolors))+(float)nsaccolors+1.0+(float)ndefcolors;
          fsave = fsave < fmin ? fmin : fsave;
          fsave = fsave > fmax ? fmax : fsave;
          array[isave++] = pixdef5[(unsigned int)fsave].pixel;
          }
        }


L_8888:
	return array;

} /* end of function */






