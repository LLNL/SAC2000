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
#include "../../inc/gd5.gui.h"
void flushbuffer5(nerr)
  int *nerr;
{
  XFlush(display5);
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102795:  Original Version
*******************************************************************************/
