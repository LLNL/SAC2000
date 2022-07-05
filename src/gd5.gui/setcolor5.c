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
*    pixdef5->pixel
*
** GLOBAL OUTPUT:
*    gd5.x11.h:  color5
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"

void setcolor5(index)
  int *index;
{
  int index2;
/* Set the color */
  
  if (XDisplayCells(display5,DefaultScreen(display5)) > *index){
    index2 = *index;
    if(cmgam.cmap != MDEFAULT) index2 += 7; /* if MGREY or MCOLOR need to skip over */
                                            /* the first seven entries. */
    color5 = pixdef5[index2].pixel;
  }
  else
    color5 = BlackPixel(display5,DefaultScreen(display5));

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    110195:  Original Version
*******************************************************************************/
