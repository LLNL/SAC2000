/*******************************************************************************
** PURPOSE:
*    To intialize the common block for the SunWindow device.
*
** GLOBAL OUTPUT:
*    gd4com:  device_name4, win_attr4.status, linestyle4, brush4.width,
*             texture4.offset,
*             texture.options.(givenpattern, startpoint, endpoint, balanced),
*             device_type4, cursor_on4, cursortext_on4, char_cursor4,
*             text_cursor4, num_wins4
*
** SUBROUTINES CALLED:
*    notify_do_dispatch, signal
*
** LOCAL VARIABLES:
*    i:  Loop variable.
*******************************************************************************/

void initdevice4(void )
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    880325:  Added call to signal.
*    880318:  Added call to notify_do_dispatch.
*    870410:  Added linestyle and brush initializations.
*    870323:  Added initializations for cursortext4_ routine.
*    870127:  Allowed for multiple windows.
*    861205:  Original Version
*******************************************************************************/
