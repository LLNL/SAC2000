/*******************************************************************************
** PURPOSE:
*    To begin plotting to a specified window.
*
** INPUT ARGUMENTS:
*    win_num:  Window number to begin plotting to. (Pointer)
*
** OUTPUT ARGUMENTS:
*    nerr:    Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  basew3->status
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  c_win3;
*******************************************************************************/
#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void beginwindow3(int* win_num, int* nerr)
{

  *nerr = 0;

/* Check if requested window is AVAILABLE */

  if (basew3[*win_num].status == AVAILABLE) 
    c_win3 = *win_num;
  else
    *nerr = 1;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890606:  Used as-is for X11 driver.  (kjm)
*    870318:  Name changes due to gd3.x11.h structure change.
*    870227:  Original Version
*******************************************************************************/
