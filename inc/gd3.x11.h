/*******************************************************************************
** COMMON for XWindow graphics device.
*
** DESCRIPTION:
*    current_pt_p3:       Pixel location of current point in XWindow
*                         coordinates.
*    basew3:              Base window structure attributes.
*    titlew3:             Title window structure attributes.
*    plotw3:              Plot window structure attributes.
*    iconw3:              Icon window structure attributes.
*    win_attr:            Window structure attributes.
*      win:               Handle of window.
*      width_p:           Width of window in pixels.
*      height_p:          Height of window in pixels.
*      status:            Status of window.
*    title_font3:         Font id of title label.
*    pixdef3:             Pixel color definitions.
*    borderwidth3:        Width of window borders in pixels.
*    current_mouse_x_p3:  Current x pixel location of mouse in XWindow
*                         coordinates.
*    current_mouse_y_p3:  Current y pixel location of mouse in XWindow
*                         coordinates.
*    num_wins3:           Number of windows which have been created.
*    c_win3:              Current window.  Numbered 1 to n
*    device_type:         Device type.
*    cursor_on:           Set to 1 if waiting for cursor input, otherwise 0.
*    cursortext_on4:      Set to 1 if waiting for cursor text input, otherwise 0.
*    color:               Index into color table to current color entry.
*    xcursor_p3:          x position of cursor in pixels.
*    ycursor_p3:          y position of cursor in pixels.
*    scr_width_p3:         Width of screen in pixels.
*    scr_height_p3:        Height of screen in pixels.
*    char_cursor:         Character struck at cursor location.
*    text_cursor4:        Line of text struck at cursor location.
*    device_name:         Device name.
************************************************************************
** MODIFICATION HISTORY:
*    920609:  Changed OW_OFFSET from hardcoded -8 to 0. Problem with
*             XdrawLine callback was fixed in OpenWindows version 3.
*    920318:  Protability to IBM required changing "struct{int x,y} point"
*             to "struct{int x,y;}
*    910506:  Added defines OW_OFFSET and ZERO for kludge to openwindows
*             cursor problem. XdrawLine (callback to server) retruns
*             eight pixels more than the values sent to it. Only happens
*             immediately after the XQueryPointer call. The offset is
*             applied in cursor3.c cursortext3.c and dispatcheve3.c (wct).
*    890606:  Added GC and display3 functionality.
*    870323:  Added variables for cursortext3_ routine.
*    870318:  Changed win_status3 structure.
*    870317:  Added title font.
*    870310:  Added icon.
*    870309:  Added title window.
*    870223:  Original Version.
*******************************************************************************/

#define ZERO 0
#ifndef OW_OFFSET
#define OW_OFFSET ZERO
#endif

#define MAX_WINS     10
#define AVAILABLE    0
#define UNAVAILABLE  1

typedef struct {int x, y;} point;

struct win_status3 {
  Window win;
  int width_p;
  int height_p;
  int status;
  GC  gc;
};

point current_pt_p3;
struct win_status3 basew3[MAX_WINS+1];
struct win_status3 titlew3[MAX_WINS+1];
struct win_status3 plotw3[MAX_WINS+1];
GC cursorgc3[MAX_WINS+1];
unsigned int borderwidth3;
int current_mouse_x_p3, current_mouse_y_p3;
int num_wins3;
int c_win3;
int device_type3;
int cursor_on3;
int cursortext_on3;
int linestyle3;
int xcursor_p3, ycursor_p3;
int scr_width_p3;
int scr_height_p3;
char char_cursor3[1];
char text_cursor3[132];
char device_name3[13];
Font title_font3;
Display *display3;
XColor pixdef3[256];
unsigned int color3;
Colormap colormap;
int npscolors;
unsigned int blackpixel;
unsigned int whitepixel;

Pixmap plotw3_pixmap;



