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
#include "../../inc/gem.h"

void cbar_window5(xloc,yloc,height,w_height,w_width,vspaceratio,ypmax,nerr)
unsigned int xloc,yloc,height,w_height,w_width;
float vspaceratio,ypmax;
int *nerr;
{
      *nerr = 0;


/*        cmgem.ypmx = 1.0 - ((float)yloc/(float)w_height); */
        cmgem.ypmx = ypmax;

        cmgem.ypmn = cmgem.ypmx - ((float)height/(float)w_height);

        cmgem.xpmn = (float)xloc/(float)w_width;

        cmgem.ypmnu = cmgem.ypmn*vspaceratio;
        cmgem.ypmxu = cmgem.ypmx*vspaceratio;

        cmgem.xpmnu = cmgem.xpmn;


L_8888:
	return ;

} /* end of function */






