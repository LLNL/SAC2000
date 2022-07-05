/*******************************************************************************
** PURPOSE:
*    To draw poly-segment line
*
** INPUT ARGUMENTS:
*    xloc_vp:  x locations in viewport coordinates to draw to. (Pointer)
*    yloc_vp:  y locations in viewport coordinates to draw to. (Pointer)
*
** GLOBAL INPUT:
*    gd4com:  current_point_p4, win_attr4.(win_width_p, win_height, pw),
*             c_win4, color4, linestyle4, texture4, brush4, newline4,
*             lineseg_p4, linestylelen_p4
*
** GLOBAL OUTPUT:
*    (none)
*
** SUBROUTINES CALLED:
*    pw_polyline
*
** LOCAL VARIABLES:
*    ptlist:  Points to draw to in pixels.
*******************************************************************************/

void drawpoly4(float* xloc_vp, float* yloc_vp, int* npts)
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    870410:  Added linestyle.
*    870120:  Allowed for multiple windows.
*    861205:  Original Version
*******************************************************************************/
