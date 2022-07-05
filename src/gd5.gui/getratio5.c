/*******************************************************************************
** PURPOSE:
*    To get the aspect ratio of the window.
*
** OUTPUT ARGUMENTS:
*    ratio:  Aspect ratio of the window. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:  c_win5, plotw5->(width_p, height_p)
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void getratio5(ratio)
  float *ratio;
{

/* Set the aspect ratio */

  *ratio = (float) plotw5[c_win5].height_p/ (float) plotw5[c_win5].width_p;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102795:  Original version
*******************************************************************************/
