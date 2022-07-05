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
#include "../../inc/gd3.x11.h"
#include "../../inc/gdm.h"


void put_image3(char* data, unsigned int xloc, unsigned int yloc, unsigned int width, unsigned int height, int* nerr)
{

   XImage *image;
   static int screen;
   unsigned int depth;
   int bytes_per_line = 0; /* scan lines are contiguous. */
   int bitmap_pad = 8; /* data is byte organized. */
   int offset = 0; /* pixels to skip at start of each scan line. */

	*nerr = 0;

        screen = DefaultScreen(display3);

        /* create the image structure. */

        depth = DefaultDepth( display3, DefaultScreen(display3));
        printf( "Depth = %d\n", depth );
        /* original depth used in program was 8. See what this gives... */
        image =  XCreateImage(display3,DefaultVisual(display3,screen),
                              depth, ZPixmap, offset, NULL, width,
                              height, bitmap_pad, bytes_per_line);
        image->data = data;


        XPutImage(display3, plotw3[cmgdm.iwindow].win, plotw3[cmgdm.iwindow].gc,
                  image, 0, 0, xloc, yloc, width, height);

        XFlush(display3);

        /* destroy the image structure */
        XDestroyImage(image);


L_8888:

	return;

} /* end of function */






