/*******************************************************************************
** PURPOSE:
*    To set the color table.
*
** INPUT ARGUMENTS:
*    win_num:      Window number to set table for. (Pointer)
*    ncolors:      Number of entries in color table. (Pointer)
*    red:          Array containing intensities of reds.  Range:  [0.0,1.0]
*    green:        Array containing intensities of greens.  Range: [0.0,1.0]
*    blue:         Array containing intensities of blues.  Range: [0.0,1.0]
*
** GLOBAL OUTPUT:
*    gd5.x11.h:  pixdef->(pixel, red, green, blue)
*
** SUBROUTINES CALLED:
*    XGetWindowAttributes,XAllocColorCells,XStoreColors
*
** LOCAL VARIABLES:
*    Status:  Status of color cells.
*    i:       Loop counter.
*    planes:  Plane mask.
*    pixels:  Pixel values.
*    full:    'Fullest color' -- Colors are in range [0, full].
*
** LIMITATIONS:
*    - Maximum of 256 colors.
*******************************************************************************
** MODIFICATION HISTORY:
*    910318:  Added check for black and white monitor (ammon)
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870316:  Changed call sequence and arrangement of color table.
*    870310:  Original Version
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/gd5.gui.h"
#include "../../inc/gdm.h"
#include "../../inc/color.h"

void setpsctable5(int* win_num, unsigned nentry, float* red, float* green, float* blue)
{ 
  Status status;
  int i, nent;
  int planes;
  int default_depth;
  unsigned long pixels[256];
  int full = 65535;
  unsigned long masks[256];
  XWindowAttributes xwinatt;
  XSetWindowAttributes winatt;
  unsigned int valuemask;
  Visual *v;
  XColor colorcell[1];
  XColor pixdeftest[256];
  unsigned int nalloc;
  int maxcolormaps,mincolormaps;
  Screen *screenptr;


/*  check for depth (black and white = 1; color != 1)  */
  default_depth = DefaultDepth(display5,DefaultScreen(display5));
  if(default_depth != 1)
  {

/* Scale red, green, blue to be in range [0, full]. */
/** Colors are passed in with the first color representing the background   **/
/** color and the last color representing the foreground color.  These      **/
/** need to be switched around to conform to X standards -- pixel value     **/
/** 0 representing foreground (black) and pixel value of 1 representing     **/
/** background (white).                                                     **/

  npscolors = cmgdm.npscimage;

  nalloc = nentry + npscolors + 7;

  v = DefaultVisual(display5,DefaultScreen(display5));

  colormap = XCreateColormap(display5,plotw5[*win_num].win,v,AllocNone);

  status = XAllocColorCells(display5, colormap,True,masks,0, pixels,nalloc);
  if (status != 0) {
    colorcell->flags = DoRed | DoGreen | DoBlue;
    for (i=0; i<7; i++){
      colorcell->pixel = i;
      XQueryColor(display5,DefaultColormap(display5,DefaultScreen(display5)),colorcell);
      pixdef5[i].pixel = pixels[i+2];
      pixdef5[i].red = colorcell->red;
      pixdef5[i].green = colorcell->green;
      pixdef5[i].blue = colorcell->blue;
      pixdef5[i].flags = DoRed | DoGreen | DoBlue;
      
    }

    pixdef5[nentry - 1 + 7].pixel = pixels[1];
    pixdef5[nentry - 1 + 7].red = full * red[nentry-1];
    pixdef5[nentry - 1 + 7].green = full * green[nentry-1];
    pixdef5[nentry - 1 + 7].blue = full * blue[nentry-1];
    pixdef5[nentry - 1 + 7].flags = DoRed | DoGreen | DoBlue;

    pixdef5[7].pixel = pixels[0];
    pixdef5[7].red = full * red[0];
    pixdef5[7].green = full * green[0];
    pixdef5[7].blue = full * blue[0];
    pixdef5[7].flags = DoRed | DoGreen | DoBlue;

    for (i = 8; i< nentry-1+7; i++) {
      pixdef5[i].pixel = pixels[i+1];
      pixdef5[i].red = full * red[i-7];
      pixdef5[i].green = full * green[i-7];
      pixdef5[i].blue = full * blue[i-7];
      pixdef5[i].flags = DoRed | DoGreen | DoBlue;
    }

    for (i = nentry+7; i < nalloc; i++){
      pixdef5[i].pixel = pixels[i];
      pixdef5[i].red = psred[i-(nentry+7)];
      pixdef5[i].green = psgreen[i-(nentry+7)];
      pixdef5[i].blue = psblue[i-(nentry+7)];
      pixdef5[i].flags = DoRed | DoGreen | DoBlue;
    }

/* Store color table */

    XStoreColors(display5,colormap,pixdef5,nalloc);
    
    XSetWindowColormap(display5,plotw5[*win_num].win,colormap); 

    XFlush(display5);

  } /* if(status != 0) */
  }
  else
  {
  /*  black and white */
    pixdef5[0].pixel = WhitePixel(display5,DefaultScreen(display5));
    nent = nentry - 1;
    for (i = 1; i< nent; i++) 
      pixdef5[i].pixel = BlackPixel(display5,DefaultScreen(display5));
  }
}

