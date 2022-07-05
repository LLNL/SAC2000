/*******************************************************************************
** PURPOSE:
*    To erase the window.
*
** GLOBAL INPUT:
*    gd3.x11.h:  plotw3->win
*
** SUBROUTINES CALLED:
*    XClearWindow, XFlush
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void erase3(void)
{

/* Erase window */

  XClearWindow(display3,plotw3[c_win3].win);
  XFlush(display3);
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870223:  Original Version
*******************************************************************************/
