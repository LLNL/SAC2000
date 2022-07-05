/*******************************************************************************
** PURPOSE:
*    To get the aspect ratio of the window.
*
** OUTPUT ARGUMENTS:
*    ratio:  Aspect ratio of the window. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  c_win3, plotw3->(width_p, height_p)
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void getratio3(float* ratio)
{

/* Set the aspect ratio */

  *ratio = (float) plotw3[c_win3].height_p/ (float) plotw3[c_win3].width_p;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Used as-is from X10 version.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870223:  Allowed for multiple windows.
*******************************************************************************/
