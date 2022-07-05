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
#include "../../inc/gtm.h"
#include "../../inc/contouring.h"
#include "../../inc/gd5.gui.h"
#include "../../inc/gdm.h"


void put_image5(char* data, unsigned int xloc, unsigned int yloc, unsigned int width, unsigned int height, int* nerr)
{

   XImage *image;
   static int screen;


	/*=====================================================================
	 * PURPOSE:
	 *         
         *         
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    
	 *    
	 *    
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *  
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *  
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        screen = DefaultScreen(display5);

        /* create the image structure. */

        image =  XCreateImage(display5,DefaultVisual(display5,screen),
                              8, ZPixmap, 0, NULL, width,
                              height, 8, 0);
        image->data = data;


        XPutImage(display5, plotw5[cmgdm.iwindow].win, plotw5[cmgdm.iwindow].gc,
                  image, 0, 0, xloc, yloc, width, height);

        XFlush(display5);

        /* destroy the image structure */
        XDestroyImage(image);


L_8888:

	return;

} /* end of function */






