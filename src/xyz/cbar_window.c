#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "gem.h"
#include "gam.h"
#include "gdm.h"
void cbar_window2(unsigned int xloc,unsigned int yloc,unsigned int height,unsigned int w_height,unsigned int w_width,float vspaceratio,float ypmax,int* nerr);
void cbar_window3(unsigned int xloc,unsigned int yloc,unsigned int height,unsigned int w_height,unsigned int w_width,float vspaceratio,float ypmax,int* nerr);
void cbar_window5(unsigned int xloc,unsigned int yloc,unsigned int height,unsigned int w_height,unsigned int w_width,float vspaceratio,float ypmax,int* nerr);


void cbar_window(unsigned int xloc,unsigned int yloc,unsigned int height,unsigned int w_height,unsigned int w_width,float vspaceratio,float ypmax,int* nerr)
{

    *nerr = 0;

    if( Lgdon[1] )
        *nerr = 1;
    if( Lgdon[2] )
        cbar_window2(xloc,yloc,height,w_height,w_width,vspaceratio,ypmax,nerr);

    if( Lgdon[3] )
        cbar_window3(xloc,yloc,height,w_height,w_width,vspaceratio,ypmax,nerr);

    if( Lgdon[4] )
        *nerr = 1;

    if( Lgdon[5] )
        cbar_window5(xloc,yloc,height,w_height,w_width,vspaceratio,ypmax,nerr);

L_8888:
    return;
} /* end of function */







