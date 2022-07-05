/*******************************************************************************
** PURPOSE:
*    To draw to a given viewport location.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x location in viewport coordinates to draw to. (Pointer)
*    yloc_vp:  y location in viewport coordinates to draw to. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  current_point_p3, plotw3->(width_p, height, win), color3,
*                c_win3
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  current_point_p3
*
** SUBROUTINES CALLED:
*    XDrawLine
*
** LOCAL VARIABLES:
*    new_pt_p:  Point to draw to in pixels.
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void draw3(xloc_vp, yloc_vp)
  float *xloc_vp, *yloc_vp;
{
  point new_pt_p;

/* Convert point from viewport coords to pixels */

  new_pt_p.x = *xloc_vp * (plotw3[c_win3].width_p - 1);
  new_pt_p.y = *yloc_vp * (plotw3[c_win3].width_p - 1);

/* Draw line */

  XSetForeground(display3,plotw3[c_win3].gc,color3);
  XDrawLine(display3,plotw3[c_win3].win,plotw3[c_win3].gc, 
	    current_pt_p3.x, plotw3[c_win3].height_p - 1 - current_pt_p3.y,
	    new_pt_p.x, plotw3[c_win3].height_p - 1 - new_pt_p.y);

/* Update current point */

  current_pt_p3 = new_pt_p;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870223:  Original Version
*******************************************************************************/
