/*******************************************************************************
** PURPOSE:
*    To change the auxiliary colors in the color table.
*
** INPUT ARGUMENTS:
*    icolortable: Requested auxiliary color table.  
** GLOBAL OUTPUT:
*    gd3.x11.h:  pixdef->(pixel, red, green, blue)
*
** SUBROUTINES CALLED:
*    XGetWindowAttributes,XAllocColorCells,XStoreColors
*
** LOCAL VARIABLES:
*    Status:  Status of color cells.
*    i:       Loop counter.
*    planes:  Plane mask.
*    pixels:  Pixel values.
*    full:    'Fullest color' -- Colors are in range [0, full].
*
** LIMITATIONS:
*    - Maximum of 256 colors.
*******************************************************************************
** MODIFICATION HISTORY:
*    940921:  Original Version
*******************************************************************************/

#define MGREY 1
#define MCOLOR 2

#include <X11/Xlib.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/gd3.x11.h"
#include "../../inc/color.h"
#include "../../inc/gdm.h"

void changectable3(nentry,icolortable)
  int nentry, icolortable;
{ 
}

