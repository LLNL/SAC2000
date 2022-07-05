#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
#include "../../inc/gdm.h"
void calc_loc5(unsigned int *xloc,unsigned int *yloc,unsigned int *cbarxoffset,unsigned int *cbaryoffset,unsigned int w_width,unsigned int w_height,float xpmn,float xpmx,float xmin,float first,float last,float ypmn,float ypdel,int *nerr);

void calcloc(xloc,yloc,cbarxoffset,cbaryoffset,w_width,w_height,i_width,i_height,xpmn,xpmx,xmin,first,last,ypmn,ypdel,vsratio,nerr)
unsigned int *xloc, *yloc, *cbarxoffset, *cbaryoffset;
unsigned int w_width, w_height,i_width,i_height;
float xpmn,xpmx,xmin,first,last,ypmn,ypdel,vsratio;
int *nerr;

{

        *nerr = 0;

	if( Lgdon[1] )
            *nerr = 1;
	if( Lgdon[2] )
            calc_loc2(xloc,yloc,cbarxoffset,cbaryoffset,w_width,w_height,
                      i_width,i_height,xpmn,xpmx,xmin,first,last,ypmn,ypdel,vsratio,nerr);
	if( Lgdon[3] )
            calc_loc3(xloc,yloc,cbarxoffset,cbaryoffset,w_width,w_height,
                                               xpmn,xpmx,xmin,first,last,ypmn,ypdel,nerr);
	if( Lgdon[4] )
            *nerr = 1;

	if( Lgdon[5] )
            calc_loc5(xloc,yloc,cbarxoffset,cbaryoffset,w_width,w_height,
                                               xpmn,xpmx,xmin,first,last,ypmn,ypdel,nerr);

L_8888:
	return;
} /* end of function */







