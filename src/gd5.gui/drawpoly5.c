/*****************************************************************************
** PURPOSE:
*    To draw (npts-1) poly-segment lines through npts points.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x locations in viewport coordinates to draw to. (Pointer)
*    yloc_vp:  y locations in viewport coordinates to draw to. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:  current_point_p5, plotw5->(width_p, height, win), color5,
*                c_win5
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  current_point_p5
*
** SUBROUTINES CALLED:
*    XDrawLines
*
** LOCAL VARIABLES:
*    i:       integer.
*    ptlist:  Points to draw through in pixels.
*****************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version 
*****************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void drawpoly5(float* xloc_vp, float* yloc_vp, int* npts)
{
  static int i;
  static XPoint ptlist[102];

/* Convert points from viewport coords to pixels */
  for (i = 0; i<*npts; i++) {
  ptlist[i].x = xloc_vp[i] * (plotw5[c_win5].width_p - 1) ;
  ptlist[i].y = plotw5[c_win5].height_p - 1
                - yloc_vp[i] * (plotw5[c_win5].width_p - 1);
  }

/* Draw line */

  XSetForeground(display5,plotw5[c_win5].gc,color5);
  XDrawLines(display5,plotw5[c_win5].win,plotw5[c_win5].gc, 
	   ptlist,*npts,CoordModeOrigin);
}

