/*******************************************************************************
** PURPOSE:
*    To flush the graphics buffer.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** SUBROUTINES CALLED:
*    XFlush
*******************************************************************************/
#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"
void flushbuffer3(int* nerr)
{
  XFlush(display3);
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870223:  Original Version
*******************************************************************************/
