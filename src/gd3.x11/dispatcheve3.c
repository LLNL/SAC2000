/*******************************************************************************
** PURPOSE:
*    To handle window events.  Processes all window events in queue.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    plotw3->(win, width_p, height_p), basew3->(win, width_p, height_p),
*    titlew3->(win, height_p), iconw3->win, num_wins3, borderwidth3,
*    title_font3, cursor_on3
*
** GLOBAL OUTPUT:
*    plotw3->(width_p, height_p), titlew3->width_p, basew3->(width_p, height_p),
*    cursor_on3, xcursor_p3, ycursor_p3, char_cursor3,
*    current_mouse_x_p3, current_mouse_y_p3
*
** SUBROUTINES CALLED:
*    XPending, XGetWindowAttributes, XNextEvent, XMoveResizeWindow,
*    XDrawString, XQueryPointer, XSetForeground,
*    XStoreBitmap, XBitmapBitsPut, XFreeBitmap, XLookupString,
*    XDrawLine, XFlush, make_label3, strncpy
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
*    is_basew:      Set to 1 if event is in base window.
*    is_titlew:     Set to 1 if event is in title window.
*    is_plotw:      Set to 1 if event is in plot window.
*    is_iconw:      Set to 1 if event is in icon window.
*    done_text:     Set to 1 as soon as a <cr> is struck.
*    first_char:    Set to 1 if reading first character of text.
*    title_label:   Title label.
*    ptitle_label:  Pointer to title_label. (Pointer)
*    kchar:         Character string struck at cursor. (Pointer)
*    temp_char:     Character constant struck at cursor.
*******************************************************************************
** MODIFICATION HISTORY:
*    910506:  Added kludge to fix cursor location problem in OpenWindows
*             versions 2 and 3. Not sure when Sun will fix the problem.
*    890607:  Modified to run under X11 rather than X10.    (kjm)
*    870323:  Added handling of line of cursor text events.
*    870318:  Changes due to gd3.x10.h structure change.
*    870310:  Added icon.
*    870309:  Added title window.
*    870302:  Added handling of resize events.
*    870227:  Original Version
*******************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include "../../inc/gd3.x11.h"

void make_label3(char* string, int* num, char* label);


void dispatchevent3(int* nerr)
{
  XEvent event, *pevent = &event;
  Bool mouse_status;
  Status basew_status;
  XWindowAttributes basew_info;
  Window child, root;
  int mouse_x_p, mouse_y_p;
  int rootx,rooty;
  int index, i, m, n, nevents, nbytes;
  int is_basew = 0;
  int is_titlew = 0;
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

  owoffset=ZERO;

  *nerr = 0;
  ptitle_label = title_label;

/* Find out which window event occurred in and set index */

  while ((nevents = XPending(display3)) > 0) {  /* While there is an event */
    for (n=1; n<=nevents; n++) { 
      XNextEvent(display3,&event);                 /* Get next event */
      index = 0;
      i = 1;
      do {
        if (((XAnyEvent *) pevent)->window == plotw3[i].win) {
          index = i;
	  is_plotw = 1;
          break;          /* Plot window */
        }
        else if (((XAnyEvent *) pevent)->window == basew3[i].win) {
	  index = i;
	  is_basew = 1;
	  break;          /* Base window */
	}
	else if (((XAnyEvent *) pevent)->window == titlew3[i].win) {
          index = i;
	  is_titlew = 1;
	  break;          /* Title window */
	}
	else
	  i++;
      }
      while (i <= num_wins3);

/*  Process each event */

      switch (event.type) {

      case ConfigureNotify:
	if (is_basew) {
	  /* Reconfigure title and plot windows if base window has been */
	  /* resized and set new dimensions of the windows */
	  basew_status = XGetWindowAttributes(display3,
					      basew3[index].win, 
					      &basew_info);
	  if ((basew_info.width != basew3[index].width_p) ||
	      (basew_info.height != basew3[index].height_p)) {
	    XMoveResizeWindow(display3,titlew3[index].win,
			      -borderwidth3,-borderwidth3,
			      basew_info.width, titlew3[index].height_p);
	    XMoveResizeWindow(display3,plotw3[index].win,
			      -borderwidth3,titlew3[index].height_p,
			      basew_info.width, 
			      basew_info.height-
			      titlew3[index].height_p-
			      borderwidth3);
	    plotw3[index].width_p = basew_info.width;
	    plotw3[index].height_p = basew_info.height - 
	      titlew3[index].height_p-borderwidth3;
	    titlew3[index].width_p = basew_info.width;
	    basew3[index].width_p = basew_info.width;
	    basew3[index].height_p = basew_info.height;
	  }
	}
	else if (is_titlew) {
	  /* Refresh title label */
	  make_label3("Graphics Window:  ", &index, title_label);
	  XDrawString(display3,titlew3[index].win,
		      titlew3[index].gc, 6, 17,
		      ptitle_label, strlen(ptitle_label));
	}
	
	break;

      case Expose:

	  if (is_basew) {
	    /* Reconfigure title and plot windows if base window has been */
	    /* resized and set new dimensions of the windows */
	    basew_status = XGetWindowAttributes(display3,
						basew3[index].win, 
						&basew_info);
	    if ((basew_info.width != basew3[index].width_p) ||
		(basew_info.height != basew3[index].height_p)) {
	      XMoveResizeWindow(display3,titlew3[index].win,
	        -borderwidth3, -borderwidth3,
                basew_info.width, titlew3[index].height_p);
	      XMoveResizeWindow(display3,plotw3[index].win,
	        -borderwidth3, titlew3[index].height_p,
	        basew_info.width, 
		  basew_info.height-titlew3[index].height_p-borderwidth3);
              plotw3[index].width_p = basew_info.width;
  	      plotw3[index].height_p = basew_info.height - 
				       titlew3[index].height_p-borderwidth3;
	      titlew3[index].width_p = basew_info.width;
	      basew3[index].width_p = basew_info.width;
	      basew3[index].height_p = basew_info.height;
	    }
	  }

	  else if (is_titlew) {
	    /* Refresh title label */
	    make_label3("Graphics Window:  ", &index, title_label);
 	    XDrawString(display3,titlew3[index].win,
			titlew3[index].gc, 6, 17,
			ptitle_label, strlen(ptitle_label));
	  }

	break;

      case KeyPress:

	  if ((cursor_on3) && (is_plotw)) {
	    /* Get cursor position and if applicable, the character struck */
	    /* Also, set cursor_on3 flag to indicate cursor is off */
	    xcursor_p3 = ((XKeyEvent *) pevent)->x;
	    ycursor_p3 = ((XKeyEvent *) pevent)->y;
	    nbytes = XLookupString(((XKeyEvent *) pevent),&kchar,1,NULL,NULL);
	    if (nbytes > 0)
	      char_cursor3[0] = kchar;
            cursor_on3 = 0;
	  }

	  else if ((cursortext_on3) && (is_plotw)) {
	    /* Get cursor position and a line of text */
	    m = 0;
	    done_text = 0;
	    first_char = 1;
	    while (!done_text) {
	      nbytes = XLookupString(((XKeyEvent *) pevent),&kchar,1,NULL,NULL);
	      if (nbytes > 0) {
	        if (first_char) {  /* Save position of first character */
		  xcursor_p3 = ((XKeyEvent *) pevent)->x;
		  ycursor_p3 = ((XKeyEvent *) pevent)->y;
		  first_char = 0;
		}
		if (kchar == 13) {
		  done_text = 1;
		  text_cursor3[m] = '\0';
		  cursortext_on3 = 0;
		}
		else {
		  text_cursor3[m] = kchar;
		  m++;
		  XNextEvent(display3,&event);
		}
              }
	      else
	        XNextEvent(display3,&event);
	    }
	  } 

	  break;

	case ButtonPress:

	  if ((cursor_on3) && (is_plotw)) {
	    /* Get cursor position and if applicable, the character struck */
	    /* Also, set cursor_on3 flag to indicate cursor is off */
	    xcursor_p3 = ((XButtonEvent *) pevent)->x;
	    ycursor_p3 = ((XButtonEvent *) pevent)->y;
	      nbytes = XLookupString(((XKeyEvent *) pevent),&kchar,1,NULL,NULL);
	    if (nbytes > 0)
	      char_cursor3[0] = kchar;
            cursor_on3 = 0;
	  }

	  else if ((cursortext_on3) && (is_plotw)) {
	    /* Get cursor position and a line of text */
	    m = 0;
	    done_text = 0;
	    first_char = 1;
	    while (!done_text) {
	      nbytes = XLookupString(((XKeyEvent *) pevent),&kchar,1,NULL,NULL);
	      if (nbytes > 0) {
	        if (first_char) {  /* Save position of first character */
		  xcursor_p3 = ((XButtonEvent *) pevent)->x;
		  ycursor_p3 = ((XButtonEvent *) pevent)->y;
		  first_char = 0;
		}
		if (kchar == 13) {
		  done_text = 1;
		  text_cursor3[m] = '\0';
		  cursortext_on3 = 0;
		}
		else {
		  text_cursor3[m] = kchar;
		  m++;
		  XNextEvent(display3,&event);
		}
              }
	      else
	        XNextEvent(display3,&event);
	    }
	  } 

	  break;

	case EnterNotify:
        
	     if ((cursor_on3 || cursortext_on3) && (is_plotw)) {
	       /* Get mouse position and draw crosshair cursor */
	       if ((mouse_status = XQueryPointer(display3,plotw3[index].win,
                    &root,&child,&rootx,&rooty,
		    &mouse_x_p, &mouse_y_p, &mask)) == False) {
                 *nerr = 1;
		 return;
	       }
               else {
		 current_mouse_x_p3 = mouse_x_p;
		 current_mouse_y_p3 = mouse_y_p;
		 XSetForeground(display3,cursorgc3[index],color3);
		 XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			   0, current_mouse_y_p3 + owoffset,
			   plotw3[index].width_p - 1, current_mouse_y_p3 + owoffset);
		 XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			   current_mouse_x_p3 + owoffset, 0,
			   current_mouse_x_p3 + owoffset, plotw3[index].height_p - 1);
                 XFlush(display3);
	       }
	     }

	     break;

	case MotionNotify:

	       if ((cursor_on3 || cursortext_on3) && (is_plotw)) {
	         /* Get mouse position and update crosshair cursor */
	         if ((mouse_status = XQueryPointer(display3,plotw3[index].win,
		      &root,&child,&rootx,&rooty,
                      &mouse_x_p, &mouse_y_p, &mask)) == False) {
                   *nerr = 1;
		   return;
		 }
                 else {
		   XSetForeground(display3,cursorgc3[index],color3);
		   XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			     0, current_mouse_y_p3 + owoffset,
			     plotw3[index].width_p - 1, current_mouse_y_p3 + owoffset);
		   XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			     current_mouse_x_p3 + owoffset, 0,
			     current_mouse_x_p3 + owoffset, plotw3[index].height_p - 1);
                   current_mouse_x_p3 = mouse_x_p;
		   current_mouse_y_p3 = mouse_y_p;
		   XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			     0, current_mouse_y_p3 + owoffset,
			     plotw3[index].width_p - 1, current_mouse_y_p3 + owoffset);
		   XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			     current_mouse_x_p3 + owoffset, 0,
			     current_mouse_x_p3 + owoffset, plotw3[index].height_p - 1);
                   XFlush(display3);
                 }
               }

	       break;

        case LeaveNotify:
	       if ((cursor_on3 || cursortext_on3) && (is_plotw)) {
	         /* Erase crosshair cursor */
		 XSetForeground(display3,cursorgc3[index],color3);
		 XDrawLine(display3,plotw3[index].win, cursorgc3[index],
			   0, current_mouse_y_p3 + owoffset,
			   plotw3[index].width_p - 1, current_mouse_y_p3 + owoffset);
		 XDrawLine(display3,plotw3[index].win,cursorgc3[index], 
			   current_mouse_x_p3 + owoffset, 0,
			   current_mouse_x_p3 + owoffset, plotw3[index].height_p - 1);
                 XFlush(display3);
	       }

	       break;

      }

/* Reset window flags */

      is_basew = 0;
      is_plotw = 0;
      is_titlew = 0;

    }

  }

}

