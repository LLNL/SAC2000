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
#include "../../inc/gd5.gui.h"


void getdevicerat5(ratio)
  float *ratio;
{
/* Get dimensions of root window and set the aspect ratio */

    *ratio = ((float) scr_height_p5) / ((float) scr_width_p5);
						     }

/*******************************************************************************
** MODIFICATION HISTORY:
*    102795:  Original Version
*******************************************************************************/
