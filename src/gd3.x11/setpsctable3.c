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
*    gd3.x11.h:  pixdef->(pixel, red, green, blue)
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
#include "../../inc/gd3.x11.h"
#include "../../inc/gdm.h"
#define DOINITS
#include "../../inc/color.h"
#undef DOINITS

void setpsctable3(int* win_num, unsigned nentry, float* red, float* green, float* blue)
{ 
  Status status;
  int i, nent;
  int planes;
  int default_depth;
  unsigned int pixels[256];
  int full = 65535;
  unsigned int masks[256];
  XWindowAttributes xwinatt;
  XSetWindowAttributes winatt;
  unsigned int valuemask;
  Visual *v;
  XColor colorcell[1];
  XColor pixdeftest[256];
  unsigned int nalloc;
  int maxcolormaps,mincolormaps;
  Screen *screenptr;

}

