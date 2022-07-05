#define HT_ADJUST 32 /* these parameters also exist in gdm/adj_geometry */
#define WD_ADJUST 32 /* these parameters also exist in gdm/adj_geometry */
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
#include "../../inc/gem.h"

void cbar_window2(unsigned int xloc,unsigned int yloc,unsigned int height,unsigned int w_height,unsigned int w_width,float vspaceratio,float ypmax,int* nerr)
{

/*        cmgem.ypmx = 1.0 - ((float)yloc/((float)w_height*(float)HT_ADJUST));*/
        cmgem.ypmx = ypmax;

        cmgem.ypmn = cmgem.ypmx - ((float)height/(float)w_height);

        cmgem.xpmn = (float)xloc/((float)w_width*(float)WD_ADJUST);

/* is multiplication by vspaceratio necessary for gd2? */
        cmgem.ypmnu = cmgem.ypmn*vspaceratio;
        cmgem.ypmxu = cmgem.ypmx*vspaceratio;

        cmgem.xpmnu = cmgem.xpmn;

L_8888:
	return;

} /* end of function */






