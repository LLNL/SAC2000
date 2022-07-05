/*******************************************************************************
** PURPOSE:
*    To get the aspect ratio of the device.
*
** OUTPUT ARGUMENTS:
*    ratio:  Aspect ratio of device. (Pointer)
*
** SUBROUTINES CALLED:
*    we_getparentwindow, open, win_getrect, close
*
** LOCAL VARIABLES:
*    fd:    File descriptor.
*    name:  Name of parent window.
*    rect:  Rect containing information about the parent window. (Root)
*******************************************************************************/

void getdevicerat4(float* ratio)
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    880322:  Changed from getting dimesions of "/dev/fb" to the parent window.
*    861205:  Original Version
*******************************************************************************/
