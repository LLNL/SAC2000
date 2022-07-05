#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ reperr(nerr)
int nerr;
{
	char knumer[9], ksacer[9] = "        " ;
	int nzerr, nchar;

	/*=====================================================================
	 * PURPOSE:  To report the current SAC error condition.
	 *           This report is sent to the global variable file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  SETBBV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870301:  Modifications due to new global variable structure.
	 *    860206:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Send SAC error status to global variable file. */
	strcpy( ksacer, "FALSE   " );
	if( nerr != 0 )
		strcpy( ksacer, "TRUE    " );
	setbbv( "SACERROR", ksacer, &nzerr, 8, 8 );

	/* - Also send the error number. */

        sprintf(knumer,"%4d",nerr);
        nchar = strlen(knumer) + 1;
	ljust( knumer,nchar );
	setbbv( "NUMERROR", knumer, &nzerr, 8, nchar-1 );

L_8888:
	return;

} /* end of function */

