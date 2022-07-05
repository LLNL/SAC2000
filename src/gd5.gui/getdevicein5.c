/*******************************************************************************
** PURPOSE:
*    To get the device attributes.
*
** OUTPUT ARGUMENTS:
*    dev_name:         Name of the device.
*    dev_type:         Type of device. (Pointer)
*    dev_name_length:  Length of dev_name. (Pointer)
*
** GLOBAL INPUT:
*    gd5.gui.h:  device_name5, device_type5
*
** SUBROUTINES CALLED:
*    strcpy
*******************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void getdeviceinfo5(dev_name, dev_name_length, dev_type)
  char dev_name[];
  int dev_name_length;
  int *dev_type;
{

/* Set attributes */

  strcpy(dev_name, device_name5);
  *dev_type = device_type5;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102795:  Original Version
*******************************************************************************/
