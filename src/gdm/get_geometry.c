#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"

#include "mach.h"
#include "gdm.h"

void get_geometry2( unsigned int* width_return, unsigned int* height_return, int* nerr);
void get_geometry3( int window, unsigned int* width_return, unsigned int* height_return, int* nerr);
void get_geometry5( int window, unsigned int* width_return, unsigned int* height_return, int* nerr);


void get_geometry(width_return, height_return, nerr)
unsigned int *width_return, *height_return;
int *nerr;
{

	/*=====================================================================
	 * PURPOSE:  To get the geometry for the active window;
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *  
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gdm:    lgdon
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gdm:    
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *   saclib:  get_geometry3
	 *            
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *  
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *  
	 *  940823: Original version.
	 *  
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */
	/* PROCEDURE: */

		if( Lgdon[1] ){
                   *nerr = 1;
		}
		if( Lgdon[2] ){
                   get_geometry2(width_return, height_return, nerr);
		}
		if( Lgdon[3] ){
                   get_geometry3(cmgdm.iwindow, width_return, height_return, nerr);
		}
		if( Lgdon[4] ){
                   *nerr = 1;
		}
		if( Lgdon[5] ){
                   get_geometry5(cmgdm.iwindow, width_return, height_return, nerr);
		}

L_8888:
	return;

} /* end of function */

