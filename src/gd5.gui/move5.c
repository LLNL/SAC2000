/*******************************************************************************
** PURPOSE:
*    To move to a given viewport location.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x location in viewport coordinates to move to. (Pointer)
*    yloc_vp:  y location in viewport coordinates to move to. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:  plotw5->width_p, c_win5
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  current_pt_p5
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void move5(xloc_vp, yloc_vp)
  float *xloc_vp, *yloc_vp;
{

/* Update current point */

  current_pt_p5.x = *xloc_vp * (plotw5[c_win5].width_p - 1);
  current_pt_p5.y = *yloc_vp * (plotw5[c_win5].width_p - 1);

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    110195:  Original Version
*******************************************************************************/
