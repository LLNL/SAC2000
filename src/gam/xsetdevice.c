#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gdm.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ xsetdevice(nerr)
int *nerr;
{
	int index;


	/*=====================================================================
	 * PURPOSE:  To execute the action command SETDEVICE.
	 *           This command sets the default graphics device.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  GAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG
	 *    GDM:     MGD, KGDNAM
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GAM:     KGDDEF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCCHAR
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870416:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870416
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- "text":  the name of the default graphics device. */
		if( lclist( (char*)kmgdm.kgdnam,13, MGD, &index ) ){
			strscpy( kmgam.kgddef, kmgdm.kgdnam[index - 1], 8 );
		}

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

