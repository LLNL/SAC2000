/*******************************************************************************
** PURPOSE:
*    To get the geometry of the active window.
*
** INPUT ARGUMENTS:
*  
*  
*
** GLOBAL INPUT:
*    gd3.x11.h:  current_point_p3, plotw3->(width_p, height, win), color3,
*                c_win3
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
#include "../../inc/gd3.x11.h"

void get_geometry3( int window, unsigned int* width_return, unsigned int* height_return, int* nerr)
{

    *nerr = 0;
    *width_return = plotw3[window].width_p;
    *height_return = plotw3[window].height_p;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*
*
*    940823:  Original Version
*******************************************************************************/
