/*******************************************************************************
** PURPOSE:
*    To create a window.
*
** INPUT ARGUMENTS:
*    win_num:  Window number. (Pointer)
*    xmin_vp:  Minimum x placement of window in device viewport dimensions.  
*              Range:  [0.0,1.0] (Pointer)
*    xmax_vp:  Maximum x placement of window in device viewport dimensions.  
*              Range:  [0.0,1.0] (Pointer)
*    ymin_vp:  Minimum y placement of window in device viewport dimensions.
*              Range:  [0.0,device aspect ratio] (Pointer)
*    ymax_vp:  Maximum y placement of window in device viewport dimensions.
*              Range:  [0.0,device aspect ratio] (Pointer)
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    gd4com:  scr_width_p4, scr_height_p4
*
** GLOBAL OUTPUT:
*    gd4com:  win_attr.(frame, canvas, win_width_p, win_height_p, pw, status),
*             num_wins4
*
** SUBROUTINES CALLED:
*    mpr_static, cursor_create, icon_create, window_create, window_set,
*    window_get, canvas_pixwin, event_proc4, resize_proc4, sprintf, strcpy,
*    strcat
*
** LOCAL VARIABLES:
*    cursor_image:  Pixrect containing cursor image.
*    icon_image:    Pixrect containing icon image.
*    cursor:        Handle of cursor.
*    icon:          Handle of icon.
*    frame_rect:    Placement of window in pixels.
*    frame_label:   Frame label.
*    icon_label:    Icon label.
*    kwin_num:      Character equivalent of win_num.
*******************************************************************************/

void createwindow4(int* win_num, float *xmin_vp, float *xmax_vp, float *ymin_vp, float *ymax_vp, int* nerr)
{

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    880322:  Put modified destroy handler back in.
*    880318:  Took out call to notify_dispatch.
*    880216:  Changed call to dispatchevents to notify_dispatch.
*    880212:  Commented out destroy handler.
*    870121:  Added icon.
*    870121:  Added destroy_proc4.
*    870120:  Fixed bug in frame_rect.
*    870120:  Allowed for multiple windows.
*    861205:  Original Version
*******************************************************************************/
