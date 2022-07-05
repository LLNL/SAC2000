/*******************************************************************************
** PURPOSE:
*    To change the auxiliary colors in the color table.
*
** INPUT ARGUMENTS:
*    icolortable: Requested auxiliary color table.  
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
*    940921:  Original Version
*******************************************************************************/

#define MGREY 1
#define MCOLOR 2

#include <X11/Xlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/gd5.gui.h"
#include "../../inc/color.h"
#include "../../inc/gdm.h"

void changectable5(nentry,icolortable)
  int nentry, icolortable;
{ 
  Status status;
  int i, nent;
  int planes;
  int default_depth;
  unsigned long  pixels[256];
  int full = 65535;
  unsigned long masks[256];
  XWindowAttributes xwinatt;
  XSetWindowAttributes winatt;
  unsigned int valuemask;
  Visual *v;
  XColor colorcell[1];
  XColor pixdeftest[256];
  unsigned int nalloc;

/*  check for depth (black and white = 1; color != 1)  */
  default_depth = DefaultDepth(display5,DefaultScreen(display5));
  if(default_depth != 1)
  {

  npscolors = cmgdm.npscimage;

  nalloc = nentry + npscolors + 7;

  for (i = nentry+7; i < nalloc; i++){
    pixels[i-(nentry+7)] = pixdef5[i].pixel; 
  }

  XFreeColors(display5,colormap, pixels, npscolors, 0 );

  status = XAllocColorCells(display5, colormap,True, masks, 0, pixels, npscolors);
  if (status != 0) {

    for (i = nentry+7; i < nalloc; i++){
      pixdef5[i].pixel = pixels[i-(nentry+7)];

      if(icolortable == MCOLOR){
      pixdef5[i].red = psred[i-(nentry+7)];
      pixdef5[i].green = psgreen[i-(nentry+7)];
      pixdef5[i].blue = psblue[i-(nentry+7)];
      } else {
      pixdef5[i].red = 256*(255-(i+1))-1;
      pixdef5[i].green = 256*(255-(i+1))-1;
      pixdef5[i].blue = 256*(255-(i+1))-1;
      }
      pixdef5[i].flags = DoRed | DoGreen | DoBlue;

    }

/* Store color table */

    XStoreColors(display5,colormap,&pixdef5[nentry+7],npscolors);
    
    XFlush(display5);

  } /* if(status != 0) */
  }
  else
  {
/* should return an error here */
  }
}

