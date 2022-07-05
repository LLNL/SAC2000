/*******************************************************************************
** PURPOSE:
*    To get the aspect ratio of the device.
*
** OUTPUT ARGUMENTS:
*    ratio:  Aspect ratio of device. (Pointer)
*
** SUBROUTINES CALLED:
*    DisplayHeight, DefaultScreen, DisplayWidth
*
** LOCAL VARIABLES:
*    root_info:    Information about the root window.
*    root_status:  Status of root window.
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"


void getdevicerat3(float* ratio)
{
/* Get dimensions of root window and set the aspect ratio */

    *ratio = ((float) scr_height_p3) / ((float) scr_width_p3);
						     }

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870227:  Original Version
*******************************************************************************/
