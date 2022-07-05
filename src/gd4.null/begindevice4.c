/*******************************************************************************
** PURPOSE:
*    To begin graphics to the SunWindow device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL OUTPUT:
*    gd4com:  scr_width_p4, scr_height_p4
*
** SUBROUTINES CALLED:
*    open, we_getparentwindow, win_getrect, close
*
** LOCAL VARIABLES:
*    fd:    File descriptor.
*    name:  Name of parent window.
*    rect:  Rect containing information about the parent window. (Root)
*******************************************************************************/


void begindevice4(int* nerr)
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    880210:  Changed from getting dimesions of "/dev/fb" to the parent window.
*    861205:  Original Version
*******************************************************************************/
