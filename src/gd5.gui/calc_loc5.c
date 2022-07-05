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
#include "../../inc/gd5.gui.h"


void calc_loc5(unsigned int *xloc,unsigned int *yloc,unsigned int *cbarxoffset,unsigned int *cbaryoffset,unsigned int w_width,unsigned int w_height,float xpmn,float xpmx,float xmin,float first,float last,float ypmn,float ypdel,int *nerr)
{
      *nerr = 0;

      *xloc = (xpmn + (((xmin-first)/(last-first))*(xpmx-xpmn)))*(float)w_width;
      *yloc = (ypdel * (float)w_height)+ypmn*(float)w_height;
      *cbarxoffset = 45;
      *cbaryoffset = 0;

L_8888:
	return ;

} /* end of function */






