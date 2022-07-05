/*******************************************************************************
** PURPOSE:
*    To move to a given viewport location.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x location in viewport coordinates to move to. (Pointer)
*    yloc_vp:  y location in viewport coordinates to move to. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  plotw3->width_p, c_win3
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  current_pt_p3
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void move3(float* xloc_vp, float* yloc_vp)
{

/* Update current point */

  current_pt_p3.x = *xloc_vp * (plotw3[c_win3].width_p - 1);
  current_pt_p3.y = *yloc_vp * (plotw3[c_win3].width_p - 1);

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Used as-is from X10 version.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870223:  Original Version
*******************************************************************************/
