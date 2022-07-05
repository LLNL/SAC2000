#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
void changectable2(int nentry,int icolortable);
void changectable5(int nentry,int icolortable);
void changectable(int nentry, int ctable)
{

	/*=====================================================================
	 * PURPOSE:  To change SAC's auxiliary colors.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    nentry:  Number of entries in the standard color table.
	 *    ctable:  Requested color table.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *=====================================================================
	 * MODULE/LEVEL:  gdm/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  changectable3
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    940921:  Original version.
	 *=====================================================================
	 * DOCUMENTED:
	 *===================================================================== */
	/* PROCEDURE: */

	/* - Send color table to all active graphics devices. */

	if( Lgdon[1] )
                return;
	if( Lgdon[2] )
                changectable2( nentry, ctable);
	if( Lgdon[3] )
		changectable3( nentry, ctable);
	if( Lgdon[4] )
                return;
	if( Lgdon[5] )
		changectable5( nentry, ctable);

L_8888:
	return;

} /* end of function */

