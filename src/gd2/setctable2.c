#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
#include "../../inc/gdm.h"
#define DOINITS
#include "../../inc/sgfcolor.h"
#undef DOINITS


void /*FUNCTION*/ setctable2(iwindow, nentry, red, green, blue)
int iwindow, nentry;
double red, green, blue;
{

	/*=====================================================================
	 * PURPOSE:  To set the color table for graphics device 2 (SGF).
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IWINDOW: Graphics window number. [i]
	 *    NENTRY:  Number of entries in the color table. [i]
	 *    RED:     Array of red color values. [fa]
	 *    GREEN:   Array of green color values. [fa]
	 *    BLUE:    Array of blue color values. [fa]
	 *=====================================================================
	 * SEE SETCTABLE subroutine documentation for important information
	 *               about the format and use of these color tables.
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    861020:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  861020
	 *===================================================================== */
	/* PROCEDURE: */

        
    int i;

    for (i = 0; i < cmgdm.npscimage; i++){

      sred[i] = sgfred[i];
      sgreen[i] = sgfgreen[i];
      sblue[i] = sgfblue[i];

    }

L_8888:
	return;

} /* end of function */

