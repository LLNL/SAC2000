/*******************************************************************************
** PURPOSE:
*    To draw to a given viewport location.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x location in viewport coordinates to draw to. (Pointer)
*    yloc_vp:  y location in viewport coordinates to draw to. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:  current_point_p5, plotw5->(width_p, height, win), color5,
*                c_win5
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  current_point_p5
*
** SUBROUTINES CALLED:
*    XDrawLine
*
** LOCAL VARIABLES:
*    new_pt_p:  Point to draw to in pixels.
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void draw5(xloc_vp, yloc_vp)
  float *xloc_vp, *yloc_vp;
{
  point new_pt_p;

/* Convert point from viewport coords to pixels */

  new_pt_p.x = *xloc_vp * (plotw5[c_win5].width_p - 1);
  new_pt_p.y = *yloc_vp * (plotw5[c_win5].width_p - 1);

/* Draw line */

  XSetForeground(display5,plotw5[c_win5].gc,color5);
  XDrawLine(display5,plotw5[c_win5].win,plotw5[c_win5].gc, 
	    current_pt_p5.x, plotw5[c_win5].height_p - 1 - current_pt_p5.y,
	    new_pt_p.x, plotw5[c_win5].height_p - 1 - new_pt_p.y);

/* Update current point */

  current_pt_p5 = new_pt_p;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102595:  Original Version
*******************************************************************************/
