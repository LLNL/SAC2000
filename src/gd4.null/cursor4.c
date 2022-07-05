/*******************************************************************************
** PURPOSE:
*    To return the location of the cursor and the character struck.
*
** INPUT ARGUMENTS:
*    cchar:  Character returned if no character was struck (mouse button
*            was pressed).
*
** OUTPUT ARGUMENTS:
*    xloc_vp:       x location of cursor in viewport coordinates. (Pointer)
*    yloc_vp:       y location of cursor in viewport coordinates. (Pointer)
*    cchar:         Character struck.  If no character was struck (mouse
*                   button was pressed) the value of cchar at input time will
*                   be returned.
*    cchar_length:  Length of cchar. (Pointer)
*
** GLOBAL INPUT:
*    gd4com:  win_attr.(canvas, frame, win_width_p, win_height_p), c_win4,
*             xcursor_p4, ycursor_p4, cursor_on4, char_cursor4
*
** GLOBAL OUTPUT:
*    gd4com:  cursor_on4, char_cursor4
*
** SUBROUTINES CALLED:
*    window_get, window_set, cursor_set, window_bell, dispatchevent4
*
** LOCAL VARIABLES:
*    cursor:  Handle of cursor for window.
*    nerr:    Error flag.
*******************************************************************************/

void cursor4(xloc_vp, yloc_vp, cchar, cchar_length)
  float *xloc_vp, *yloc_vp;
  char cchar[];
  int cchar_length;
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    901005:  Attempted to change call to window_bell to win_bell, 
*             fixing bug in SunOS 4.1, BUT under 4.0.3 the bell
*             does not sound, and the message about inappropriate
*             ioctl for device pops up.
*    880208:  Modified so that cchar is an input and an output.
*    870608:  Added state of mouse and keyboard events.
*    870120:  Allowed for multiple windows.
*    870422:  Set crosshair color.
*    861205:  Original Version
*******************************************************************************/
