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
*    gd5.gui.h:  pixdef->(pixel, red, green, blue)
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
*    110195:  Original Version
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void setctable5(win_num, nentry, red, green, blue)
  int *win_num;
  unsigned int nentry;
  float red[], green[], blue[];
{ 
  Status status;
  int i, nent;
  int planes;
  int default_depth;
  unsigned long pixels[256];
  int full = 65535.0;
  unsigned long masks[256];
  XWindowAttributes xwinatt;
  Colormap colormap;

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

  colormap = DefaultColormap(display5,DefaultScreen(display5));
  status = XAllocColorCells(display5, colormap,True, masks,0, pixels,nentry-2);
  if (status != 0) {
    pixdef5[nentry - 1].pixel = BlackPixel(display5,DefaultScreen(display5));
    pixdef5[nentry - 1].red = full * red[nentry - 1];
    pixdef5[nentry - 1].green = full * green[nentry - 1];
    pixdef5[nentry - 1].blue = full * blue[nentry - 1];
    pixdef5[nentry - 1].flags = DoRed | DoGreen | DoBlue;

    pixdef5[0].pixel = WhitePixel(display5,DefaultScreen(display5));
    pixdef5[0].red = full * red[0];
    pixdef5[0].green = full * green[0];
    pixdef5[0].blue = full * blue[0];
    pixdef5[0].flags = DoRed | DoGreen | DoBlue;

    for (i = 1; i< nentry-1; i++) {
      pixdef5[i].pixel = pixels[i - 1];
      pixdef5[i].red = full * red[i];
      pixdef5[i].green = full * green[i];
      pixdef5[i].blue = full * blue[i];
      pixdef5[i].flags = DoRed | DoGreen | DoBlue;
    }

/* Store color table */

    XStoreColors(display5,colormap,&pixdef5[1],nentry-2);
    XFlush(display5);
  }
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

