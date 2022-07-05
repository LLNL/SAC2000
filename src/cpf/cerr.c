#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ cerr(nerr)
int nerr;
{



	/*=====================================================================
	 * PURPOSE: To format and store a command parsing error.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NERR:    Error number.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     ncerr
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     setmsg, apimsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    821004:  Removed error message for error number 1001.
	 *    820415:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Save error number. */
	cmcom.ncerr = nerr;

	/* - If error number was not 1001 (the most common one: bad syntax),
	 *   get the formatted message from disk file and create error message. */

	if( cmcom.ncerr != 1001 ){
		setmsg( "ERROR", cmcom.ncerr );
		apimsg( cmcom.jcom );
		}

L_8888:
	return;

} /* end of function */

