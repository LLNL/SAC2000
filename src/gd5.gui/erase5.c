/*******************************************************************************
** PURPOSE:
*    To erase the window.
*
** GLOBAL INPUT:
*    gd5.gui.h:  plotw5->win
*
** SUBROUTINES CALLED:
*    XClearWindow, XFlush
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void erase5(void)
{

/* Erase window */

  XClearWindow(display5,plotw5[c_win5].win);
  XFlush(display5);
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102795:  Original Version
*******************************************************************************/
