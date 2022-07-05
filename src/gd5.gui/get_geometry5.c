/*******************************************************************************
** PURPOSE:
*    To get the geometry of the active window.
*
** INPUT ARGUMENTS:
*  
*  
*
** GLOBAL INPUT:
*    gd5.gui.h:  current_point_p5, plotw5->(width_p, height, win), color5,
*                c_win5
*
** GLOBAL OUTPUT:
*  
*
** SUBROUTINES CALLED:
*  
*
** LOCAL VARIABLES:
*  
*******************************************************************************/

#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"

void get_geometry5( int window, unsigned int* width_return, unsigned int* height_return, int* nerr)
{

    *nerr = 0;
    *width_return = plotw5[window].width_p;
    *height_return = plotw5[window].height_p;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*
*
*    102795:  Original Version
*******************************************************************************/
