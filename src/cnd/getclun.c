#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/cpf.h"
void /*FUNCTION*/ getclun(nun, nerr)
FILE **nun;
int *nerr;
{
	char kcl[MCMSG+1];


	/*=====================================================================
	 * PURPOSE: To parse the action command DO.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NUN:     Fortran logical unit number for file currently being
	 *             used to get commands. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  cnd/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MUNINP
	 *    cpf:     kvarsname
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     getvvinteger
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880401
	 *===================================================================== */
	/* PROCEDURE: */
	if( strcmp(kmcpf.kvarsname,"macro000") == 0 ){
		*nun = MUNINP;
		}
	else{
		getvFILEptr( kmcpf.kvarsname,9, "fileunit",9, nun, nerr );
		}

L_8888:
	return;

} /* end of function */

