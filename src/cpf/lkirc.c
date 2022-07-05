#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkirc(kkey, kkey_s, intmn, intmx, intv)
char *kkey;   int kkey_s;
int intmn, intmx, *intv;
{
	int lkirc_v;
	int iv, nerr;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed range-checked integer variable" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkirc:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    intmn:   Minimum allowed value for integer variable. [i]
	 *    intmx:   Maximum allowed value for integer variable. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    intv:    Integer variable found in command. [i]
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
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkirc_v = lckey( kkey,kkey_s );

	/* - If key was found:
	 * -- Get integer variable from next symbol.
	 * -- Check variable against allowed range.
	 * -- Perform standard command error recovery if not found. */

	if( lkirc_v ){
L_2000:
		if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
			iv = (int)( Flnum[cmcom.jcom] + 0.1 );
			if( iv >= intmn && iv <= intmx ){
				*intv = iv;
				cmcom.jcom = cmcom.jcom + 1;
				}
			else{
				cfmt( "OUTSIDE ALLOWED RANGE:$",24 );
                                fprintf(MUNOUT," Allowed range is: %10d%10d\n",
				                                   intmn, intmx );
				cresp();
				if( lcmore( &nerr ) )
					goto L_2000;
				}
			}
		else{
			cfmt( "NEED A INTEGER VARIABLE:$",26 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}

L_8888:
	return( lkirc_v );

} /* end of function */

