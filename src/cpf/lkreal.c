#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkreal(kkey, kkey_s, realv)
char *kkey;   int kkey_s;
float *realv;
{
	int lkreal_v;
	int nerr;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed real variable" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkreal:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    realv:   Real variable found in command. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkreal_v = lckey( kkey,kkey_s );

	/* - If key was found:
	 * -- Get real variable from next symbol.
	 * -- Perform standard command error recovery if not found. */

	if( lkreal_v ){
L_2000:
		if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
			*realv = Flnum[cmcom.jcom];
			cmcom.jcom = cmcom.jcom + 1;
			}
		else{
			cfmt( "NEED A REAL VARIABLE:$",23 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}

L_8888:
	return( lkreal_v );

} /* end of function */

