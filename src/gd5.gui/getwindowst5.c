/*******************************************************************************
** PURPOSE:
*    To get the status of a window.
*
** INPUT ARGUMENTS:
*    win_num:  Window number to get status of. (Pointer)
*
** OUTPUT ARGUMENTS:
*    exists:  Status of window.  Set to 1 if window is available for plotting.
*
** GLOBAL INPUT:
*    gd5.gui.h:  plotw5->status.
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void getwindowstat5(win_num, exists)
  int *win_num, *exists;
{

/* Get status of window */

  if (plotw5[*win_num].status == AVAILABLE)
    *exists = 1;
  else
    *exists = 0;
 
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102795:  Original Version
*******************************************************************************/
