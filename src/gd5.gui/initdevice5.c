/*******************************************************************************
** PURPOSE:
*    To intialize the common blockfor the XWindow device.
*
** GLOBAL OUTPUT:
*    gd5.gui.h:  device_name5, plotw5->status,
*                iconw5->status, borderwidth5, device_type5, cursor_on5,
*                cursortext_on5, char_cursor5, text_cursor5, num_wins5
*
** LOCAL VARIABLES:
*    i:  Loop variable.
*******************************************************************************/
#include <stdio.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "../../inc/gd5.gui.h"

void dispatchevent3(int* nerr);
void dispatchevent5(int* nerr);

void initdevice5(display,draw_area)
Display *display;
Widget draw_area;
{

  int i;
  Status plotw_status;
  XWindowAttributes plotw_info;
  XGCValues xgccurs, xgcdef;
  Cursor cursor;
  int nerr;
  
/* Set the device name */

  device_name5[0] = 'G';
  device_name5[1] = 'U';
  device_name5[2] = 'I';
  device_name5[3] = ' ';
  device_name5[4] = ' ';
  device_name5[5] = ' ';
  device_name5[6] = ' ';
  device_name5[7] = ' ';
  device_name5[8] = ' ';
  device_name5[9] = ' ';
  device_name5[10] = ' ';
  device_name5[11] = ' ';
  device_name5[12] = '\0';

/* Set the display global variable. */
  display5 = display;


/* Set the initial state of the windows */

  for (i=0; i<=MAX_WINS; i++) {
    plotw5[i].status = UNAVAILABLE;
  }

/* GUI main window is window 1. */
  plotw5[1].win = XtWindow(draw_area);

  plotw_status = XGetWindowAttributes(display,
                                      plotw5[1].win,
                                      &plotw_info);
  plotw5[1].width_p  = plotw_info.width;
  plotw5[1].height_p = plotw_info.height;

/* Create a Graphics Context */
  xgccurs.function = GXxor;
  xgcdef.foreground = BlackPixel(display,DefaultScreen(display));
  xgcdef.background = WhitePixel(display,DefaultScreen(display));
  
  plotw5[1].gc = XCreateGC(display,plotw5[1].win,
                           (GCBackground | GCForeground),&xgcdef);

  cursorgc5[1] = XCreateGC(display,plotw5[1].win,
                           GCFunction,&xgccurs);

  XSelectInput(display,plotw5[1].win,
               (ExposureMask | StructureNotifyMask));


/* set the cursor to be the crosshair */
  cursor = XCreateFontCursor(display,XC_crosshair);
  XDefineCursor(display,plotw5[1].win,cursor);
  
/* Initialize some variables */

  display5 = display;
  device_type5 = 3;
  cursor_on5 = 0;
  cursortext_on5 = 0;
  char_cursor5[0] = '0';
  strcpy(text_cursor5, " ");
  num_wins5 = 1;

  scr_width_p5 = DisplayWidth(display5,DefaultScreen(display5));
  scr_height_p5 = DisplayHeight(display5,DefaultScreen(display5));

  XFlush(display);

/* Process events */
  dispatchevent5(&nerr);

  return;

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    103095:  Original Version
*******************************************************************************/
