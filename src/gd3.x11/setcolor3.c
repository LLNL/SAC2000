/*******************************************************************************
** PURPOSE:
*    To set the graphics color.
*
** INPUT ARGUMENTS:
*    index:  Index of color entry in color table, starting at 0. (Pointer)
*
*  SUBROUTINES CALLED:
*    DisplayCells,DefaultScreen,BlackPixel
*
** GLOBAL INPUT:
*    pixdef3->pixel
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  color3
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"

void setcolor3(int* index)
{
  int index2;
  /* Set the color */
  
  if (XDisplayCells(display3,DefaultScreen(display3)) > *index){
    index2 = *index;
   if(cmgam.cmap != MDEFAULT) index2 += 7; /* if MGREY or MCOLOR skip over */
                                            /* the first seven entries. */
    color3 = pixdef3[index2].pixel;
  }
  else
    color3 = BlackPixel(display3,DefaultScreen(display3));

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870316:  Modification due to change of arrangement of color table.
*    870310:  Original Version
*******************************************************************************/
