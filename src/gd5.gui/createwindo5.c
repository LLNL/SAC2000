/*******************************************************************************
** PURPOSE:
*    To create a window.  (A no-op for the GUI device until additional
*                          windows are supported.)
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
*
*
** GLOBAL OUTPUT:
*
** SUBROUTINES CALLED:
*
** LOCAL VARIABLES:
*
*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version.
*******************************************************************************/
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "../../inc/gd5.gui.h"

void createwindow5(int* win_num, float *xmin_vp, float *xmax_vp, float *ymin_vp, float *ymax_vp, int* nerr)

{
  *nerr = 0;

  return;

}

