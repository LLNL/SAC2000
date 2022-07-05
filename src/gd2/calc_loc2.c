#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/Xresource.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
#include "../../inc/gdm.h"

void calc_loc2(xloc,yloc,cbarxoffset,cbaryoffset,w_width,w_height,i_width,i_height,xpmn,xpmx,xmin,first,last,ypmn,ypdel,vsratio,nerr)
unsigned int *xloc, *yloc, *cbarxoffset,*cbaryoffset;
unsigned int w_width, w_height, i_width, i_height;
float xpmn,xpmx,xmin,first,last,ypmn,ypdel,vsratio;
int *nerr;

{
  float xtemp,ypmx,ypmntemp;
  float proportion;

  xtemp = (xpmn+(((xmin-first)/(last-first))*(xpmx-xpmn)))*(float)w_width;
  *xloc = (xtemp/(float)w_width)*XW;

  ypmx = 1.0 - (ypmn+ypdel);
  ypmntemp = ypmx - (float)((float)i_height/(float)w_height)-RNDOFF;
/*  *yloc = ypmntemp*vsratio*YW; */
  *yloc = ypmntemp*YW;

  
  *cbarxoffset = (45.0/(float)w_width)*XW;
  proportion = (float)cmgdm.npscimage/(float)i_height;
  *cbaryoffset = ((i_height - (proportion*(float)i_height))/(float)w_height)*YW;

L_8888:
	return;

} /* end of function */






