#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkrrc(kkey, kkey_s, realmn, realmx, realv)
char *kkey;   int kkey_s;
double realmn, realmx;
float *realv;
{
	int lkrrc_v;
	int nerr;
	float rv;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed range-checked real variable" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkrrc:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    realmn:  Minimum allowed value for real variable. [f]
	 *    realmx:  Maximum allowed value for real variable. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    realv:   Real variable found in command. [f]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    901157:  Split line 63 to two lines, was inter than 72. wct
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkrrc_v = lckey( kkey,kkey_s );

	/* - If key was found:
	 * -- Get real variable from next symbol.
	 * -- Check variable against allowed range.
	 * -- Perform standard command error recovery if not found. */

	if( lkrrc_v ){
L_2000:
		if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
			rv = Flnum[cmcom.jcom];
			if( rv >= realmn && rv <= realmx ){
				*realv = rv;
				cmcom.jcom = cmcom.jcom + 1;
				}
			else{
				cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                                fprintf(MUNOUT," Allowed range is: %12.5g%12.5g\n",
				                                   realmn, realmx );
				cresp();
				if( lcmore( &nerr ) )
					goto L_2000;
				}
			}
		else{
			cfmt( "NEED A REAL VARIABLE:$",23 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}

L_8888:
	return( lkrrc_v );

} /* end of function */

