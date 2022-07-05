/*******************************************************************************
** PURPOSE:
*    To intialize the common blockfor the XWindow device.
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  device_name3, basew3->status, titlew3->status, plotw->status,
*                iconw3->status, borderwidth3, device_type3, cursor_on3,
*                cursortext_on3, char_cursor3, text_cursor3, num_wins3
*
** LOCAL VARIABLES:
*    i:  Loop variable.
*******************************************************************************/

#include <X11/Xlib.h>
#include <strings.h>
#include <string.h>
#include "../../inc/gd3.x11.h"


void initdevice3(void)
{

  int i;

/* Set the device name */

  device_name3[0] = 'X';
  device_name3[1] = 'W';
  device_name3[2] = 'I';
  device_name3[3] = 'N';
  device_name3[4] = 'D';
  device_name3[5] = 'O';
  device_name3[6] = 'W';
  device_name3[7] = 'S';
  device_name3[8] = ' ';
  device_name3[9] = ' ';
  device_name3[10] = ' ';
  device_name3[11] = ' ';
  device_name3[12] = '\0';

/* Set the initial state of the windows */

  for (i=0; i<=MAX_WINS; i++) {
    basew3[i].status = UNAVAILABLE;
    titlew3[i].status = UNAVAILABLE;
    plotw3[i].status = UNAVAILABLE;
  }

/* Initialize some variables */

  borderwidth3 = 4;
  device_type3 = 3;
  cursor_on3 = 0;
  cursortext_on3 = 0;
  char_cursor3[0] = '0';
  strcpy(text_cursor3, " ");
  num_wins3 = 0;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870323:  Added initializations for cursortext3_ routine.
*    870318:  Changes due to gd3.x10.h structure change.
*    870227:  Original Version
*******************************************************************************/
