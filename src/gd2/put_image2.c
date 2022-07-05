#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gd2.h"



void put_image2(data, xloc, yloc, width, height, nerr)
char *data;
unsigned int xloc, yloc;
unsigned int width, height;
int *nerr;
{
  static short is8 = 8;
  unsigned short int icl, icr;
  short int izshft();
  int j, nw, nchars, copycount;
  float unused, xvpmin, xvpmax, xvsmin, xvsmax, xwcmin, xwcmax;
  float xfactor, xpsize;

	/*=====================================================================
	 * PURPOSE:  To do an image plot with limited options.
	 *           Need to add options to allow user to specify size of
         *           image.  Default is the size of the raw data.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *      array:  Two-dimensional array of data. [fa]  
	 *     nxsize:  Number of elements in the x (horizontal) direction. [i]
	 *     nysize:  Number of elements in the y (vertical) direction. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL: contouring/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    gtm:         xvpmin, xvpmax, yvpmin, yvpmax
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *     saclib:  move, draw, setcolorname
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        if(cmgd2.encodesize){
          getvport(&xvpmin, &xvpmax, &unused, &unused);
          getvspace(&xvsmin, &xvsmax, &unused, &unused);
          xfactor = (xvsmax-xvsmin)/(xvpmax-xvpmin);

          if(strncmp(kmgd2.sizetype,"FIXED",5) == 0) {
              xpsize = cmgd2.sizevalue * xfactor;
	  } else if(strncmp(kmgd2.sizetype,"SCALED",6) == 0){
              getworld(&xwcmin, &xwcmax, &unused, &unused);
              xpsize = cmgd2.sizevalue * (xwcmax-xwcmin) * xfactor;
	  } else {
              xpsize = 10.0;
	  }
          
          Mfbuf[cmgd2.jfbpnt] = MOPSIZ;
          Mfbuf[cmgd2.jfbpnt+1] = 1;
          Mfbuf[cmgd2.jfbpnt+2] = (int)fmin((100.0*xpsize),XW);
          cmgd2.jfbpnt += 3;
          cmgd2.encodesize = FALSE;
        }

        nw = (width*height*3)/2;
        if(nw*2 != (width*height*3))nw++;

	Mfbuf[cmgd2.jfbpnt] = MOPCIMAGE;
	Mfbuf[cmgd2.jfbpnt + 1] = width;
	Mfbuf[cmgd2.jfbpnt + 2] = height;
	Mfbuf[cmgd2.jfbpnt + 3] = xloc;
	Mfbuf[cmgd2.jfbpnt + 4] = yloc;
        cmgd2.jfbpnt += 5;

        flushbuffer2(nerr);

        nchars = 3*width*height;
        copycount = 0;
 
/*        loop to store the data  */
        while ( (copycount+(2*JFBMAX)) <= nchars ) {
          memcpy((char *)&Mfbuf[cmgd2.jfbpnt],&data[copycount],2*JFBMAX);
          cmgd2.jfbpnt += JFBMAX;
          flushbuffer2(nerr);
          copycount += 2*JFBMAX;
	}

        if( (nchars-copycount) > 0 ) {
          memcpy((char *)&Mfbuf[cmgd2.jfbpnt],&data[copycount],nchars-copycount);
          cmgd2.jfbpnt += (nchars-copycount)/2;
          if((nchars-copycount) % 2 ) cmgd2.jfbpnt += 1;
          flushbuffer2(nerr);
	}


L_8888:
	return;

} /* end of function */






