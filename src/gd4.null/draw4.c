/*******************************************************************************
** PURPOSE:
*    To draw to a given viewport location.
*
** INPUT ARGUMENTS:
*    xloc_vp:  x location in viewport coordinates to draw to. (Pointer)
*    yloc_vp:  y location in viewport coordinates to draw to. (Pointer)
*
** GLOBAL INPUT:
*    gd4com:  current_point_p4, win_attr4.(win_width_p, win_height, pw),
*             c_win4, color4, linestyle4, texture4, brush4, newline4,
*             lineseg_p4, linestylelen_p4
*
** GLOBAL OUTPUT:
*    gd4com:  current_point_p4, newline4, texture4.offset
*
** SUBROUTINES CALLED:
*    seglen4, pw_line
*
** LOCAL VARIABLES:
*    new_pt_p:  Point to draw to in pixels.
*******************************************************************************/

void draw4(xloc_vp, yloc_vp)
  float *xloc_vp, *yloc_vp;
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    870410:  Added linestyle.
*    870120:  Allowed for multiple windows.
*    861205:  Original Version
*******************************************************************************/
