#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkint(kkey, kkey_s, intv)
char *kkey;   int kkey_s;
int *intv;
{
	int lkint_v;
	int nerr;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed integer variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkint:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    intv:    Integer variable found in command. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, ialpha, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820423:  Adjustments due to new command parsing system.
	 *    820415:  Added information hiding logic.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkint_v = lckey( kkey,kkey_s );

	/* - If key was found:
	 * -- Get integer variable from next symbol.
	 * -- Increment command pointer.
	 * -- Perform standard command error recovery if not found. */

	if( lkint_v ){
L_2000:
		if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
			*intv = (int)( Flnum[cmcom.jcom] + 0.1 );
			cmcom.jcom = cmcom.jcom + 1;
			}
		else{
			cfmt( "NEED A INTEGER VARIABLE:$",26 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}

L_8888:
	return( lkint_v );

} /* end of function */

