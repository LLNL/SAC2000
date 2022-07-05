/*****************************************************************************
** PURPOSE:
*    To draw (npts-1) poly-segment lines through npts points.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x locations in viewport coordinates to draw to. (Pointer)
*    yloc_vp:  y locations in viewport coordinates to draw to. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  current_point_p3, plotw3->(width_p, height, win), color3,
*                c_win3
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  current_point_p3
*
** SUBROUTINES CALLED:
*    XDrawLines
*
** LOCAL VARIABLES:
*    i:       integer.
*    ptlist:  Points to draw through in pixels.
*****************************************************************************
** MODIFICATION HISTORY:
*    920317:  Changed size of ptlist from 100 to 102. - wct.
*    910212:  Original Version  (jjy)
*****************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void drawpoly3(float* xloc_vp, float* yloc_vp, int* npts)
{
  static int i;
  static XPoint ptlist[102];

/* Convert points from viewport coords to pixels */
  for (i = 0; i<*npts; i++) {
  ptlist[i].x = xloc_vp[i] * (plotw3[c_win3].width_p - 1) ;
  ptlist[i].y = plotw3[c_win3].height_p - 1
                - yloc_vp[i] * (plotw3[c_win3].width_p - 1);
  }

/* Draw line */

  XSetForeground(display3,plotw3[c_win3].gc,color3);
  XDrawLines(display3,plotw3[c_win3].win,plotw3[c_win3].gc, 
	   ptlist,*npts,CoordModeOrigin);
}

