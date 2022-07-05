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
#include "../../inc/gdm.h"


char *fill_clrbar5(npseudocolors, width, npricolors, ndefcolors, nerr)

int npseudocolors;   /* number of pseudocolors in the colortable */
int width;       /* number of elements in one scan line of the color bar */
int npricolors;  /* number of SAC primary colors */
int ndefcolors;  /* number of default colors in the colortable */
int *nerr;
{

unsigned int k;
int ibyte, i, j;
char *array;

/*  Color bar  fill an entire scan line with one color */

  *nerr = 0;

/* allocate memory for colorbar byte array */
  if((array = (char *)malloc(width*npseudocolors*sizeof(char)))
                    == NULL){
    printf("error allocating colorbar byte array--fill_clrbar5\n");
    *nerr = 301;
    goto L_8888;
  };

  k = npricolors+npseudocolors+ndefcolors;
  ibyte = 0;
  for (i=0; i< npseudocolors; i++){
     for (j=0; j< width; j++){
       array[ibyte++ ] = pixdef5[k].pixel;  
     }
     k--;
  }

L_8888:
	return array;

} /* end of function */






