/*******************************************************************************
** PURPOSE:
*    To set the linestyle type.
*
** SUBROUTINES CALLED:
*    XChangeGC, XSetDashes
*
** INPUT ARGUMENTS:
*    linestyle:  Linestyle type.
*
******************************************************************************/
#include <stdio.h>
#include <X11/Xlib.h>
#include "../../inc/gd5.gui.h"

#define SOLID_LIST_LENGTH 1
#define DOTTED_LIST_LENGTH 2
#define DASHDOT_LIST_LENGTH 4
#define DASHED_LIST_LENGTH 2
#define DASHDOTDOTTED_LIST_LENGTH 6
#define LONG_DASHED_LIST_LENGTH 2


void setlinestyle5(linestyle)
  int *linestyle;
{
XGCValues gcv;
static int dash_list_length[] = {
  SOLID_LIST_LENGTH,
  DOTTED_LIST_LENGTH,
  DASHDOT_LIST_LENGTH,
  DASHED_LIST_LENGTH,
  DASHDOTDOTTED_LIST_LENGTH,
  LONG_DASHED_LIST_LENGTH
  };
static unsigned char solid[SOLID_LIST_LENGTH] =
  {0};
static unsigned char dotted[DOTTED_LIST_LENGTH] =
  {3,1};
static unsigned char dash_dot[DASHDOT_LIST_LENGTH] =
  {3,4,3,1};
static unsigned char dashed[DASHED_LIST_LENGTH] =
  {4,4};
static unsigned char dash_dot_dot[DASHDOTDOTTED_LIST_LENGTH] =
  {3,4,3,1,3,1};
static unsigned char int_dash[LONG_DASHED_LIST_LENGTH] =
  {4,7};

static unsigned char *dash_list[] = {
  solid,
  dotted,
  dash_dot,
  dashed,
  dash_dot_dot,
  int_dash
  };

int dash_offset = 0;
if ((*linestyle < 1) || (*linestyle > 6)) *linestyle = 1;
if (*linestyle == 1) {
  gcv.line_style = LineSolid;
  XChangeGC(display5,plotw5[c_win5].gc,GCLineStyle,&gcv);
}
else {
  gcv.line_style = LineOnOffDash;
  XChangeGC(display5,plotw5[c_win5].gc,GCLineStyle,&gcv);
  XSetDashes(display5,plotw5[c_win5].gc,dash_offset,(const char*)dash_list[*linestyle-1],
	     dash_list_length[*linestyle-1]);
}
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    110195:  Original Version
*******************************************************************************/
