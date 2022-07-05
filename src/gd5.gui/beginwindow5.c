/*******************************************************************************
** PURPOSE:
*    To begin plotting to a specified window.
*    Only one window is supported at this time.
*
** INPUT ARGUMENTS:
*    win_num:  Window number to begin plotting to. (Pointer)
*    (ignored)
** OUTPUT ARGUMENTS:
*    nerr:    Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  c_win5;
*******************************************************************************/
#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void beginwindow5(int* win_num, int* nerr)
{
  int window = 1;

  *nerr = 0;

  plotw5[window].status = AVAILABLE;
  c_win5                = window;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version
*******************************************************************************/
