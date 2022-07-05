/*******************************************************************************
** PURPOSE:
*    To end graphics to the XWindow device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  display3, basew3->(win, status), num_wins3, title_font3
*
** GLOABL OUTPUT:
*    gd3.x11.h:  c_win3
*
** SUBROUTINES CALLED:
*    XCloseDisplay
*
** LOCAL VARIABLES:
*    i:  Loop counter.
*
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void enddevice3(int* nerr)
{
  int i;

  *nerr = 0;

/* Destroy window activity and set status flag */

  c_win3 = 0;
  for (i=1; i <= num_wins3; i++) {
/*  XDestroyWindow(display3,basew3[i].win);*/
    basew3[i].status = UNAVAILABLE;
  }

/*  Free title font */

/* XUnloadFont(display3,title_font3); */

/*  Close window display */

  XCloseDisplay(display3);
  
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    911031:  Marked the window status UNAVAILABLE. (wct)
*    910819:  Changed XDestroyWindow to XCloseDisplay and commented
*             call to XUnloadFont (wct).
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870317:  Added call to free font.
*    870223:  Original Version
*******************************************************************************/
