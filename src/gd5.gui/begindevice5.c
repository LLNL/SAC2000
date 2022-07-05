/*******************************************************************************
** PURPOSE:
*    To begin graphics to the GUI device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  scr_width_p5, scr_height_p5, title_font5
*
** SUBROUTINES CALLED:
*    DisplayWidth, DisplayHeight, XLoadFont
*
** LOCAL VARIABLES:
*    fontname:     Name of font for title label.
*******************************************************************************/

#include <stdio.h>
#ifdef STELLAR
#include "/usr/include/X11/Xlib.h"
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#endif
#include "../../inc/gd5.gui.h"



void begindevice5(int* nerr)
{

  *nerr = 0;
  
  return;
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version
*******************************************************************************/
