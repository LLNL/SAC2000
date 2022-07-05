/*******************************************************************************
** PURPOSE:
*    To return the location of the cursor and the line of text input.
*
** OUTPUT ARGUMENTS:
*    xloc_vp:       x location of cursor in viewport coordinates. (Pointer)
*    yloc_vp:       y location of cursor in viewport coordinates. (Pointer)
*    ktext:         Line of text input.
*    ktext_length:  Length of cchar. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  plotw3->(win, width_p, height_p), c_win3, xcursor_p3, ycursor_p3,
*                cursortext_on3, text_cursor3
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  cursortext_on3, current_mouse_x_p3, current_mouse_y_p3
*
** SUBROUTINES CALLED:
*    XQueryPointer, XDrawLine, XFlush, XBell, XSelectInput, dispatchevent3_
*
** LOCAL VARIABLES:
*    child:         Child window, if any, which mouse is in.
*    root:          Root window of window mouse is in.
*    rootx,y:       Mouse location relative to root window origin.
*    mask:          Mask_value from XQueryPointer.
*    mouse_status:  Status of mouse.
*    nerr:          Error flag.
*    mouse_x_p:     X position of mouse in pixels.
*    mouse_y_p:     Y position of mouse in pixels.
*******************************************************************************
** MODIFICATION HISTORY:
*    910506:  Added kludge to fix cursor location problem in OpenWindows
*             versions 2 and 3. Not sure when Sun will fix the problem.
*    890608:  Modified to run under X11 rather than X10.  (kjm)
*    870318:  Changes due to gd3.x10.h structure change.
*    870303:  Original Version
*******************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <X11/Xlib.h>
#include "../../inc/gd3.x11.h"

void dispatchevent3(int* nerr);
void cursortext3(xloc_vp, yloc_vp, ktext, ktext_length)
  float *xloc_vp, *yloc_vp;
  char ktext[];
  int ktext_length;
{
  Window root,child;
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
			&root, &child, &rootx, &rooty,
                        &mouse_x_p, &mouse_y_p, &mask)) == False) {
    nerr = 1;
    return;
  }
  else {
    cursortext_on3 = 1;
    current_mouse_x_p3 = mouse_x_p;
    current_mouse_y_p3 = mouse_y_p;
    if ((current_mouse_x_p3 >= 0) &&
        (current_mouse_x_p3 < plotw3[c_win3].width_p) &&
        (current_mouse_y_p3 >= 0) &&
        (current_mouse_y_p3 < plotw3[c_win3].height_p)) {
      XSetForeground(display3,cursorgc3[c_win3],color3);
      XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
	    0, current_mouse_y_p3 + owoffset,
	    plotw3[c_win3].width_p - 1, current_mouse_y_p3 + owoffset);
      XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
  	    current_mouse_x_p3 + owoffset, 0,
	    current_mouse_x_p3 + owoffset, plotw3[c_win3].height_p - 1);
      XFlush(display3);
    }
  }
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

/* While waiting for cursor text event, handle other events */

  while (cursortext_on3 != 0)
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
	  plotw3[c_win3].width_p - 1, current_mouse_y_p3 + owoffset);
    XDrawLine(display3,plotw3[c_win3].win,cursorgc3[c_win3],
          current_mouse_x_p3 + owoffset, 0,
	  current_mouse_x_p3 + owoffset, plotw3[c_win3].width_p - 1);
    XFlush(display3);
  }
  
/* Set location of cursor and character struck */

  *xloc_vp = (float) xcursor_p3 / (float) (plotw3[c_win3].width_p - 1);
  *yloc_vp = (float) (plotw3[c_win3].height_p - 1 - ycursor_p3) / 
	     (float) (plotw3[c_win3].width_p - 1);
  strcpy(ktext, text_cursor3);

}

