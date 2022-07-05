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
*    gd5.gui.h:  plotw5->(win, width_p, height_p), c_win5, xcursor_p5, ycursor_p5,
*                cursortext_on5, text_cursor5
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  cursortext_on5, current_mouse_x_p5, current_mouse_y_p5
*
** SUBROUTINES CALLED:
*    XQueryPointer, XDrawLine, XFlush, XBell, XSelectInput, dispatchevent5_
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
*    110195:  Original Version
*******************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"

void dispatchevent5(int* nerr);

void cursortext5(xloc_vp, yloc_vp, ktext, ktext_length)
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

  if ((mouse_status = XQueryPointer(display5,plotw5[c_win5].win,
			&root, &child, &rootx, &rooty,
                        &mouse_x_p, &mouse_y_p, &mask)) == False) {
    nerr = 1;
    return;
  }
  else {
    cursortext_on5 = 1;
    current_mouse_x_p5 = mouse_x_p;
    current_mouse_y_p5 = mouse_y_p;
    if ((current_mouse_x_p5 >= 0) &&
        (current_mouse_x_p5 < plotw5[c_win5].width_p) &&
        (current_mouse_y_p5 >= 0) &&
        (current_mouse_y_p5 < plotw5[c_win5].height_p)) {
      XSetForeground(display5,cursorgc5[c_win5],color5);
      XDrawLine(display5,plotw5[c_win5].win,cursorgc5[c_win5],
	    0, current_mouse_y_p5 + owoffset,
	    plotw5[c_win5].width_p - 1, current_mouse_y_p5 + owoffset);
      XDrawLine(display5,plotw5[c_win5].win,cursorgc5[c_win5],
  	    current_mouse_x_p5 + owoffset, 0,
	    current_mouse_x_p5 + owoffset, plotw5[c_win5].height_p - 1);
      XFlush(display5);
    }
  }
  XBell(display5,25);
    
/* Select which events the plot window needs to accept for cursor input */

  XSelectInput(display5,plotw5[c_win5].win,
	       KeyPressMask |
	       ButtonPressMask |
	       EnterWindowMask |
	       PointerMotionMask |
	       LeaveWindowMask |
	       StructureNotifyMask |
	       ExposureMask);

/* While waiting for cursor text event, handle other events */

  while (cursortext_on5 != 0)
  {
    dispatchevent5(&nerr);
  }

/* Don't accept events anymore for cursor input */

  XSelectInput(display5,plotw5[c_win5].win,
               (StructureNotifyMask | ExposureMask));

/* Turn off crosshairs */

  if ((current_mouse_x_p5 >= 0) &&
      (current_mouse_x_p5 < plotw5[c_win5].width_p) &&
      (current_mouse_y_p5 >= 0) &&
      (current_mouse_y_p5 < plotw5[c_win5].height_p)) {
    XSetForeground(display5,cursorgc5[c_win5],color5);
    XDrawLine(display5,plotw5[c_win5].win,cursorgc5[c_win5],
  	  0, current_mouse_y_p5 + owoffset,
	  plotw5[c_win5].width_p - 1, current_mouse_y_p5 + owoffset);
    XDrawLine(display5,plotw5[c_win5].win,cursorgc5[c_win5],
          current_mouse_x_p5 + owoffset, 0,
	  current_mouse_x_p5 + owoffset, plotw5[c_win5].width_p - 1);
    XFlush(display5);
  }
  
/* Set location of cursor and character struck */

  *xloc_vp = (float) xcursor_p5 / (float) (plotw5[c_win5].width_p - 1);
  *yloc_vp = (float) (plotw5[c_win5].height_p - 1 - ycursor_p5) / 
	     (float) (plotw5[c_win5].width_p - 1);
  strcpy(ktext, text_cursor5);

}

