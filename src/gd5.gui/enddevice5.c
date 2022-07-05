/*******************************************************************************
** PURPOSE:
*    To end graphics to the GUI device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:  display5, basew5->(win, status), num_wins5, title_font5
*
** GLOABL OUTPUT:
*    gd5.gui.h:  c_win5
*
** SUBROUTINES CALLED:
*
** LOCAL VARIABLES:
*    i:  Loop counter.
*
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void enddevice5(int* nerr)
{
  int i;

  *nerr = 0;

/* Destroy window activity and set status flag */

  c_win5 = 0;
  for (i=1; i <= num_wins5; i++) {
    plotw5[i].status = UNAVAILABLE;
  }

  return;  
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version
*******************************************************************************/
