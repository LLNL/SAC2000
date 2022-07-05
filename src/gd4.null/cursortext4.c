/*******************************************************************************
** PURPOSE:
*    To return the location of the cursor and the line of text input.
*
** OUTPUT ARGUMENTS:
*    xloc_vp:       x location of cursor in viewport coordinates. (Pointer)
*    yloc_vp:       y location of cursor in viewport coordinates. (Pointer)
*    ktext:         Line of text input.
*    ktext_length:  Length of ktext. (Pointer)
*
** GLOBAL INPUT:
*    gd4com:  win_attr4.(canvas, frame, win_width_p, win_height_p), c_win4,
*             xcursor_p4, ycursor_p4, cursortext_on4, text_cursor4
*
** GLOBAL OUTPUT:
*    gd4com:  cursortext_on4
*
** SUBROUTINES CALLED:
*    window_get, window_set, cursor_set, window_bell, dispatchevent4
*
** LOCAL VARIABLES:
*    cursor:  Handle of cursor for window.
*    nerr:    Error flag.
*******************************************************************************/

void cursortext4(xloc_vp, yloc_vp, ktext, ktext_length)
  float *xloc_vp, *yloc_vp;
  char ktext[];
  int ktext_length;
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    901202:  Added precompiler flags for SUNOS4.1 call to window_bell bug. wct
*    870608:  Added state of mouse and keyboard events.
*    870422:  Set crosshair color.
*    870323:  Original Version
*******************************************************************************/
