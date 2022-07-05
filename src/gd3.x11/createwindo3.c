/*******************************************************************************
** PURPOSE:
*    To create a window.
*
** INPUT ARGUMENTS:
*    win_num:  Window number. (Pointer)
*    xmin_vp:  Minimum x placement of window in device viewport dimensions.  
*              Range:  [0.0,1.0] (Pointer)
*    xmax_vp:  Maximum x placement of window in device viewport dimensions.  
*              Range:  [0.0,1.0] (Pointer)
*    ymin_vp:  Minimum y placement of window in device viewport dimensions.
*              Range:  [0.0,device aspect ratio] (Pointer)
*    ymax_vp:  Maximum y placement of window in device viewport dimensions.
*              Range:  [0.0,device aspect ratio] (Pointer)
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** GLOBAL INPUT:
*    gd3.x11.h:  scr_width_p3, scr_height_p3, borderwidth3, titlefont3
*
** GLOBAL OUTPUT:
*    gd3.x11.h:  basew3->(win, width_p, height_p, status),
*             titlew3->(win, width_p, height_p, status),
*             plotw3->(win, width_p, height_p, status),
*             iconw3->(win, width_p, height_p, status), num_wins3
*
** SUBROUTINES CALLED:
*    XCreateWindow, XCreateGC, XSetStandardProperties, 
*    XSelectInput, XMapWindow, XDrawString,
*    XFlush, XSync, dispatchevent3_, make_label3
*
** LOCAL VARIABLES:
*    kwin_num:      Character equivalent of win_num.
*    xul_p:         Upper left x coordinate of plot window in pixels.
*    yul_p:         Upper left y coordinate of plot window in pixels.
*    width_p:       Width of plot window in pixels.
*    height_p:      Height of plot window in pixels.
*    title_label:   Title label.
*    ptitle_label:  Pointer to title label. (Pointer)
*******************************************************************************
** MODIFICATION HISTORY:
*    910426:  Added call XDefineCursor to set the crosshair in graphics window. (wct)
*    890830:  Used same window mask for title window as for plot window. (jet)
*    890606:  Modified to be X11 compatible.  (kjm)
*    870318:  Changes due to gd3.x11.h structure change.
*    870310:  Added icon.
*    870309:  Added title window.
*    870227:  Original Version.
*******************************************************************************/
#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "../../inc/gd3.x11.h"
#include "sac.bitmap"

Cursor cursor;

void make_label3(char* string, int* num, char* label);
void dispatchevent3(int* nerr);
void createwindow3(int *win_num, float *xmin_vp, float *xmax_vp, float *ymin_vp, float *ymax_vp, int* nerr)
{
  char kwin_num[8];
  char name[8];
  int xul_p, yul_p;
  unsigned int width_p, height_p;
  char title_label[32], *ptitle_label;
  XSetWindowAttributes winatt;
  XGCValues xgcval, xgccurs, xgcdef,xgcicon;
  unsigned int valuemask;
  XWMHints wmhints;
  XSizeHints sizehints;
  Pixmap SacPixmap;
  GC icongc;
  int stringwid;
  Font icon_font;
  XFontStruct *font_info;
  char *iconfont = "fixed";
  *nerr = 0;
  ptitle_label = title_label;
  XEvent event;

/* Set up boundaries of window */

  xul_p = (int) ((float) (scr_width_p3-1) * *xmin_vp);
  yul_p = (int) ((float) (scr_height_p3-1) * (1.0 - *ymax_vp));
  width_p = (unsigned int) ((float) (scr_width_p3-1) * (*xmax_vp - *xmin_vp));
  height_p = (unsigned int) ((float) (scr_height_p3-1) * (*ymax_vp - *ymin_vp));


/* Create title label */

  make_label3("Graphics Window:  ", win_num, title_label);

/* Create base window and set attributes */

  titlew3[*win_num].height_p = 25;
  winatt.backing_store = Always;
  winatt.background_pixel = WhitePixel(display3,DefaultScreen(display3));
  valuemask = CWBackPixel | CWBackingStore;


  if ((basew3[*win_num].win =
       XCreateWindow(display3,DefaultRootWindow(display3),
         xul_p, yul_p,
         width_p, (unsigned int)(height_p+titlew3[*win_num].height_p+borderwidth3),
         borderwidth3,CopyFromParent,InputOutput,CopyFromParent,
	 valuemask,&winatt)) == 0) {
    *nerr = 1;
    return;
  }






  basew3[*win_num].width_p = width_p;
  basew3[*win_num].height_p = height_p+titlew3[*win_num].height_p+borderwidth3;
  basew3[*win_num].status = AVAILABLE;
  basew3[*win_num].gc = XCreateGC(display3,basew3[*win_num].win,0,&xgcdef);
  make_label3("SAC ",win_num,name);
  wmhints.flags = InputHint | StateHint | IconPixmapHint;
  wmhints.input = True;
  wmhints.initial_state = NormalState;
  SacPixmap = XCreateBitmapFromData (display3,basew3[*win_num].win,
				     (const char*) sac_bits, sac_width,
				     sac_height);
  icon_font = XLoadFont(display3,iconfont);
  xgcicon.font = icon_font;
  xgcicon.foreground =  BlackPixel(display3,DefaultScreen(display3));
  xgcicon.background =  WhitePixel(display3,DefaultScreen(display3));
  icongc = XCreateGC(display3,SacPixmap,
		     (GCBackground | GCForeground | GCFont),
		     &xgcicon);
  font_info = XQueryFont(display3,XGContextFromGC(icongc));
  stringwid = XTextWidth(font_info,name,strlen(name));
  XDrawString(display3,SacPixmap,icongc,32-(stringwid>>1),
	      59,name,strlen(name));
  wmhints.icon_pixmap = SacPixmap;
  XSetWMHints(display3,basew3[*win_num].win, &wmhints);
  
  sizehints.flags = 0;
  sizehints.flags = PPosition | PSize;
  sizehints.x = xul_p;
  sizehints.y = yul_p;
 /* XSetWMNormalHints(display3,basew3[*win_num].win, &sizehints); */

/* Create title window and set attributes */

  if ((titlew3[*win_num].win =
       XCreateWindow(display3,basew3[*win_num].win,
         -borderwidth3, -borderwidth3,
         width_p, titlew3[*win_num].height_p,
         borderwidth3,CopyFromParent,InputOutput,CopyFromParent,
         valuemask,&winatt)) == 0) {
    *nerr = 1;
    return;
  }
		
  xgcval.font = title_font3;
  xgcval.foreground =  BlackPixel(display3,DefaultScreen(display3));
  xgcval.background =  WhitePixel(display3,DefaultScreen(display3));
  titlew3[*win_num].width_p = width_p;
  titlew3[*win_num].status = AVAILABLE;
  titlew3[*win_num].gc = XCreateGC(display3,titlew3[*win_num].win,
                                   (GCBackground | GCForeground | GCFont),
				   &xgcval);

/* Create plot window and set attributes */

  if ((plotw3[*win_num].win =
       XCreateWindow(display3,basew3[*win_num].win,
         -borderwidth3, titlew3[*win_num].height_p,
         width_p, height_p,
         borderwidth3,CopyFromParent,InputOutput,CopyFromParent,
         valuemask,&winatt)) == 0) {
    *nerr = 1;
    return;
  }

  xgccurs.function = GXxor;
  xgcdef.foreground =  BlackPixel(display3,DefaultScreen(display3));
  xgcdef.background =  WhitePixel(display3,DefaultScreen(display3));
  plotw3[*win_num].width_p = width_p;
  plotw3[*win_num].height_p = height_p;
  plotw3[*win_num].status = AVAILABLE;
  plotw3[*win_num].gc = XCreateGC(display3,plotw3[*win_num].win,
				  (GCBackground | GCForeground),&xgcdef);
  cursorgc3[*win_num] = XCreateGC(display3,plotw3[*win_num].win,
				  GCFunction,&xgccurs);

/* Select events will be accepted by each window */

  XSelectInput(display3,basew3[*win_num].win,
	       (ExposureMask | StructureNotifyMask));

/* Map the windows */

  XMapWindow(display3,basew3[*win_num].win);
  XMapWindow(display3,titlew3[*win_num].win);
  XMapWindow(display3,plotw3[*win_num].win);

  //Wait for the display event
  XNextEvent(display3, &event);

  XFlush(display3);
  XSync(display3,False);

/* Write label to title window */

  XDrawString(display3,titlew3[*win_num].win,titlew3[*win_num].gc, 6, 17,
		   ptitle_label, strlen(ptitle_label));

/* Set the cursor to be the crosshair */
 
  cursor = XCreateFontCursor(display3,XC_crosshair);
  XDefineCursor(display3,basew3[*win_num].win,cursor);

  XFlush(display3);

  num_wins3++;

/* Process events */

  dispatchevent3(nerr);

}

