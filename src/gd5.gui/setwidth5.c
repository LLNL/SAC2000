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
*    pixdef5->pixel
*
** GLOBAL OUTPUT:
*    pixdef5->pixel
*******************************************************************************
** MODIFICATION HISTORY:
*    920526:  Original version.
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void setwidth5(index)
  int index;

{

  XGCValues gcv;

/* Set line-width */
  gcv.line_width=index;
  XChangeGC(display5,plotw5[c_win5].gc,GCLineWidth,&gcv);
}
