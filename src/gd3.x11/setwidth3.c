/*******************************************************************************
** PURPOSE:
*    To set the graphics line-width.
*
** INPUT ARGUMENTS:
*    index:  Index of width entry in width list, starting at 0. (Pointer)
*
*  SUBROUTINES CALLED:
*    DisplayCells,DefaultScreen,BlackPixel
*
** GLOBAL INPUT:
*    pixdef3->pixel
*
** GLOBAL OUTPUT:
*    pixdef3->pixel
*******************************************************************************
** MODIFICATION HISTORY:
*    920526:  Original version.
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void setwidth3(int index)

{

  XGCValues gcv;

/* Set line-width */
  gcv.line_width=index;
  XChangeGC(display3,plotw3[c_win3].gc,GCLineWidth,&gcv);
}
