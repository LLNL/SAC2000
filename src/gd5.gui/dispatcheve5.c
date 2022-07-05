/*******************************************************************************
** PURPOSE:
*    To handle window events.  Processes all window events in queue.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    plotw5->(win, width_p, height_p)
*    num_wins5
*    cursor_on5
*
** GLOBAL OUTPUT:
*    plotw5->(width_p, height_p)
*    cursor_on5, xcursor_p5, ycursor_p5, char_cursor5,
*    current_mouse_x_p5, current_mouse_y_p5
*
** SUBROUTINES CALLED:
*    XPending, XGetWindowAttributes, XNextEvent, XMoveResizeWindow,
*    XDrawString, XQueryPointer, XSetForeground,
*    XStoreBitmap, XBitmapBitsPut, XFreeBitmap, XLookupString,
*    XDrawLine, XFlush, make_label5, strncpy
*
** LOCAL VARIABLES:
*    mask:          Masking bitmap for putting icon bitmap into icon window.
*    pevent:        Pointer to event. (Pointer)
*    event:         Event.
*    mouse_status:  Status of mouse.
*    basew_status:  Status of base window.
*    root:          root window for window mouse is in
*    child:         Child window, if any, that mouse is in.
*    rootx:         X coord. relative to root.
*    rooty:         Y coord. relative to root.
*    mask:          Mask return from XQueryPointer.
*    mouse_x_p:     X position of mouse in pixels.
*    mouse_y_p:     Y position of mouse in pixels.
*    index:         Window number which event occurred in.
*    i, m, n:       Loop counters.
*    nevents:       Number of events in queue to process.
*    nbytes:        Equals 0 of no character was struck at cursor.
*    is_plotw:      Set to 1 if event is in plot window.
*    done_text:     Set to 1 as soon as a <cr> is struck.
*    first_char:    Set to 1 if reading first character of text.
*    kchar:         Character string struck at cursor. (Pointer)
*    temp_char:     Character constant struck at cursor.
*******************************************************************************
** MODIFICATION HISTORY:
*    102595:  Original Version
*******************************************************************************/

#include <stdio.h>
#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"


void dispatchevent5(int* nerr)
{
  XEvent event, *pevent = &event;
  Bool mouse_status;
  Status plotw_status;
  XWindowAttributes plotw_info;
  Window child, root;
  int mouse_x_p, mouse_y_p;
  int rootx,rooty;
  int index, i, m, n, nevents, nbytes;
  int is_plotw = 0;
  int done_text, first_char;
  char title_label[32], *ptitle_label;
  char kchar;
  char temp_char[2];
  unsigned int mask;

/* Use an offset of OW_OFFSET for calls to XDrawLine. OpenWindows has a problem
   that does not occur in generic X11R4. */
  int owoffset;
  char *pvendor;

/* temporarily disable this routine */

  return;
/*
  owoffset=ZERO;

  *nerr = 0;
  ptitle_label = title_label;

Comment Find out which window event occurred in and set index  

  while ((nevents = XPending(display5)) > 0) {  Comment While there is an event 
    for (n=1; n<=nevents; n++) { 
      XNextEvent(display5,&event);                 Comment Get next event 
      index = 0;
      i = 1;
      do {
        if (((XAnyEvent *) pevent)->window == plotw5[i].win) {
          index = i;
	  is_plotw = 1;
          break;          Comment Plot window 
        }
      }
      while (i <= num_wins5);

Comment  Process each event 

      switch (event.type) {

      case ConfigureNotify:

        plotw_status = XGetWindowAttributes(display5,
                                      plotw5[1].win,
                                      &plotw_info);
        plotw5[1].width_p  = plotw_info.width;
        plotw5[1].height_p = plotw_info.height;
	
	break;

      case Expose:

        plotw_status = XGetWindowAttributes(display5,
                                      plotw5[1].win,
                                      &plotw_info);
        plotw5[1].width_p  = plotw_info.width;
        plotw5[1].height_p = plotw_info.height;

	break;

      case KeyPress:

	  if ((cursor_on5) && (is_plotw)) {
	    Comment Get cursor position and if applicable, the character struck  
	    Comment Also, set cursor_on5 flag to indicate cursor is off 
	    xcursor_p5 = ((XKeyEvent *) pevent)->x;
	    ycursor_p5 = ((XKeyEvent *) pevent)->y;
	    nbytes = XLookupString(&event,&kchar,1,NULL,NULL);
	    if (nbytes > 0)
	      char_cursor5[0] = kchar;
            cursor_on5 = 0;
	  }

	  else if ((cursortext_on5) && (is_plotw)) {
	    Comment Get cursor position and a line of text  
	    m = 0;
	    done_text = 0;
	    first_char = 1;
	    while (!done_text) {
	      nbytes = XLookupString(&event,&kchar,1,NULL,NULL);
	      if (nbytes > 0) {
	        if (first_char) {  Comment Save position of first character  
		  xcursor_p5 = ((XKeyEvent *) pevent)->x;
		  ycursor_p5 = ((XKeyEvent *) pevent)->y;
		  first_char = 0;
		}
		if (kchar == 13) {
		  done_text = 1;
		  text_cursor5[m] = '\0';
		  cursortext_on5 = 0;
		}
		else {
		  text_cursor5[m] = kchar;
		  m++;
		  XNextEvent(display5,&event);
		}
              }
	      else
	        XNextEvent(display5,&event);
	    }
	  } 

	  break;

	case ButtonPress:

	  if ((cursor_on5) && (is_plotw)) {
	    Comment Get cursor position and if applicable, the character struck  
	    Comment Also, set cursor_on5 flag to indicate cursor is off  
	    xcursor_p5 = ((XButtonEvent *) pevent)->x;
	    ycursor_p5 = ((XButtonEvent *) pevent)->y;
	      nbytes = XLookupString(&event,&kchar,1,NULL,NULL);
	    if (nbytes > 0)
	      char_cursor5[0] = kchar;
            cursor_on5 = 0;
	  }

	  else if ((cursortext_on5) && (is_plotw)) {
	    Comment Get cursor position and a line of text  
	    m = 0;
	    done_text = 0;
	    first_char = 1;
	    while (!done_text) {
	      nbytes = XLookupString(&event,&kchar,1,NULL,NULL);
	      if (nbytes > 0) {
	        if (first_char) {  Comment Save position of first character  
		  xcursor_p5 = ((XButtonEvent *) pevent)->x;
		  ycursor_p5 = ((XButtonEvent *) pevent)->y;
		  first_char = 0;
		}
		if (kchar == 13) {
		  done_text = 1;
		  text_cursor5[m] = '\0';
		  cursortext_on5 = 0;
		}
		else {
		  text_cursor5[m] = kchar;
		  m++;
		  XNextEvent(display5,&event);
		}
              }
	      else
	        XNextEvent(display5,&event);
	    }
	  } 

	  break;

	case EnterNotify:
        
	     if ((cursor_on5 || cursortext_on5) && (is_plotw)) {
	       Comment Get mouse position and draw crosshair cursor 
	       if ((mouse_status = XQueryPointer(display5,plotw5[index].win,
                    &root,&child,&rootx,&rooty,
		    &mouse_x_p, &mouse_y_p, &mask)) == False) {
                 *nerr = 1;
		 return;
	       }
               else {
		 current_mouse_x_p5 = mouse_x_p;
		 current_mouse_y_p5 = mouse_y_p;
		 XSetForeground(display5,cursorgc5[index],color5);
		 XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			   0, current_mouse_y_p5 + owoffset,
			   plotw5[index].width_p - 1, current_mouse_y_p5 + owoffset);
		 XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			   current_mouse_x_p5 + owoffset, 0,
			   current_mouse_x_p5 + owoffset, plotw5[index].height_p - 1);
                 XFlush(display5);
	       }
	     }

	     break;

	case MotionNotify:

	       if ((cursor_on5 || cursortext_on5) && (is_plotw)) {
	         Comment  Get mouse position and update crosshair cursor 
	         if ((mouse_status = XQueryPointer(display5,plotw5[index].win,
		      &root,&child,&rootx,&rooty,
                      &mouse_x_p, &mouse_y_p, &mask)) == False) {
                   *nerr = 1;
		   return;
		 }
                 else {
		   XSetForeground(display5,cursorgc5[index],color5);
		   XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			     0, current_mouse_y_p5 + owoffset,
			     plotw5[index].width_p - 1, current_mouse_y_p5 + owoffset);
		   XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			     current_mouse_x_p5 + owoffset, 0,
			     current_mouse_x_p5 + owoffset, plotw5[index].height_p - 1);
                   current_mouse_x_p5 = mouse_x_p;
		   current_mouse_y_p5 = mouse_y_p;
		   XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			     0, current_mouse_y_p5 + owoffset,
			     plotw5[index].width_p - 1, current_mouse_y_p5 + owoffset);
		   XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			     current_mouse_x_p5 + owoffset, 0,
			     current_mouse_x_p5 + owoffset, plotw5[index].height_p - 1);
                   XFlush(display5);
                 }
               }

	       break;

        case LeaveNotify:
	       if ((cursor_on5 || cursortext_on5) && (is_plotw)) {
	         Comment Erase crosshair cursor 
		 XSetForeground(display5,cursorgc5[index],color5);
		 XDrawLine(display5,plotw5[index].win, cursorgc5[index],
			   0, current_mouse_y_p5 + owoffset,
			   plotw5[index].width_p - 1, current_mouse_y_p5 + owoffset);
		 XDrawLine(display5,plotw5[index].win,cursorgc5[index], 
			   current_mouse_x_p5 + owoffset, 0,
			   current_mouse_x_p5 + owoffset, plotw5[index].height_p - 1);
                 XFlush(display5);
	       }

	       break;

      }

Comment Reset window flags 

      is_plotw = 0;

    }

  }
  */

}

