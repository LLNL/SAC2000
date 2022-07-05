/*******************************************************************************
** PURPOSE:
*    To return the location of the cursor and the character struck.
*
** INPUT:
*
*    cchar:  Character returned if no character was struck (mouse button
*           was pressed).
*
** OUTPUT ARGUMENTS:
*    xloc_vp:       x location of cursor in viewport coordinates. (Pointer)
*    yloc_vp:       y location of cursor in viewport coordinates. (Pointer)
*    cchar:         Character struck.  If no character was struck (mouse
*                   button was pressed) the value of cchar at input time will
*                   be returned.
*    cchar_length:  Length of cchar. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  plotw3->(win, width_p, height_p), c_win3, xcursor_p3, ycursor_p3,
*                cursor_on3, char_cursor3
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  cursor_on3, current_mouse_x_p3, current_mouse_y_p3, char_cursor3
*
** SUBROUTINES CALLED:
*    XQueryPointer, XDrawLine, XFlush, XBell, XSelectInput, dispatchevent3_,
*    XSetForeground
*
** LOCAL VARIABLES:
*    subw:          Subwindow, if any, which mouse is in.
*    mouse_status:  Status of mouse.
*    nerr:          Error flag.
*    mouse_x_p:     X position of mouse in pixels.
*    mouse_y_p:     Y position of mouse in pixels.
*    pvendor:       Pointer to vendor string of display structure.
*    owoffset:      Set to zeor if MIT X11R4, otherwise set to the offset required
*                   to fix the cursor hotspot problem in OpenWindows
*******************************************************************************
** MODIFICATION HISTORY:
*    920609:  Changed hardcoded valie for OW_OFFSET in ../../inc/gd3.x11.h .
*             XDrawLine call-back problem fixed in OpenWindows version 3.
*    910506:  Added kludge to fix cursor location problem in OpenWindows
*             versions 2 and 3. Not sure when Sun will fix the problem.
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    880208:  Modified so that cchar is an input and an output.
*    870318:  Changes due to gd3.x11.h structure change.
*    870303:  Original Version
*******************************************************************************/
#include <stdio.h>
#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"
extern int bellON;
void dispatchevent3(int* nerr);

void cursor3(xloc_vp, yloc_vp, cchar, cchar_length)
  float *xloc_vp, *yloc_vp;
  char cchar[];
  int cchar_length;
{
  Window root, child;
  Bool mouse_status;
  int nerr;
  int rootx, rooty;
  int mouse_x_p, mouse_y_p;
  unsigned int mask;


/* Use an offset of OW_OFFSET for calls to XDrawLine. OpenWindows has a problem
   that does not occur in generic X11R4. */
  int owoffset;
  char *pvendor;

  owoffset=ZERO;

/* Turn on crosshairs if mouse is in window and ring bell */

  if ((mouse_status = XQueryPointer(display3,plotw3[c_win3].win,
				    &root,&child,&rootx,&rooty,
				    &mouse_x_p, &mouse_y_p, 
				    &mask)) == False) {
    nerr = 1;
    return;
  }
  else {
    cursor_on3 = 1;
    current_mouse_x_p3 = mouse_x_p;
    current_mouse_y_p3 = mouse_y_p;
    if ((current_mouse_x_p3 >= 0) &&
        (current_mouse_x_p3 < plotw3[c_win3].width_p) &&
        (current_mouse_y_p3 >= 0) &&
        (current_mouse_y_p3 < plotw3[c_win3].height_p)) {
      XSetForeground(display3,cursorgc3[c_win3],color3);
      XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
	    0, current_mouse_y_p3 + owoffset,
	    plotw3[c_win3].width_p-1, current_mouse_y_p3 + owoffset);
      XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
  	    current_mouse_x_p3 + owoffset, 0,
	    current_mouse_x_p3 + owoffset, plotw3[c_win3].height_p-1);
      XFlush(display3);
    }
  }
  if(bellON)
    XBell(display3,25); 
    
/* Select which events the plot window needs to accept for cursor input */

  XSelectInput(display3,plotw3[c_win3].win,
	       KeyPressMask |
	       ButtonPressMask |
	       EnterWindowMask |
	       PointerMotionMask |
	       LeaveWindowMask |
	       StructureNotifyMask |
	       ExposureMask);

/* Initialize character struck */

  char_cursor3[0] = cchar[0];

/* While waiting for cursor event, handle other events */

  while (cursor_on3 != 0)
  {
    dispatchevent3(&nerr);
  }

/* Don't accept events anymore for cursor input */

  XSelectInput(display3,plotw3[c_win3].win,
	       (StructureNotifyMask | ExposureMask));

/* Turn off crosshairs */

  if ((current_mouse_x_p3 >= 0) &&
      (current_mouse_x_p3 < plotw3[c_win3].width_p) &&
      (current_mouse_y_p3 >= 0) &&
      (current_mouse_y_p3 < plotw3[c_win3].height_p)) {
    XSetForeground(display3,cursorgc3[c_win3],color3);
    XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
  	  0, current_mouse_y_p3 + owoffset,
	  plotw3[c_win3].width_p-1, current_mouse_y_p3 + owoffset);
    XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
          current_mouse_x_p3 + owoffset, 0,
	  current_mouse_x_p3 + owoffset, plotw3[c_win3].width_p-1);
    XFlush(display3);
  }
  
/* Set location of cursor and character struck */

  *xloc_vp = (float) xcursor_p3 / (float) (plotw3[c_win3].width_p - 1);
  *yloc_vp = (float) (plotw3[c_win3].height_p - 1 - ycursor_p3) / 
	     (float) (plotw3[c_win3].width_p - 1);
  cchar[0] = char_cursor3[0];

}

