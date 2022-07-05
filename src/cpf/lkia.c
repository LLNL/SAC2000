#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkia(kkey, kkey_s, mnint, mxint, ia, nia)
char *kkey;   int kkey_s;
int mnint, mxint, ia[], *nia;
{
	int lkia_v;
	int intv;

	int *const Ia = &ia[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed integer array" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkia:    .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    mnint:   The minimum number of integers to return. [i]
	 *    mxint:   The maximum number of integers to return. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ia:      Array of integers found. [ia]
	 *    nia:     The number of integers returned. [i]
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
	 *    900410:  Fixed bug involving negative integers.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820415:  Added information hiding logic.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900410
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkia_v = lckey( kkey,kkey_s );

	/* - If key was found:
	 * -- Get integer variables until maximum is reached or
	 *    until an alphanumeric token is found.
	 * -- Perform standard command error recovery if not found. */

	if( lkia_v ){
		*nia = 0;
L_2000:
		if( Itypcm[cmcom.jcom] == cmcom.inumbr ){
			if( *nia < mxint ){
				if( Flnum[cmcom.jcom] >= 0.0 ){
					intv = (int)( Flnum[cmcom.jcom] + 0.1 );
					}
				else{
					intv = (int)( Flnum[cmcom.jcom] - 0.1 );
					}
				*nia = *nia + 1;
				Ia[*nia] = intv;
				cmcom.jcom = cmcom.jcom + 1;
				}
			else{
				goto L_8888;
				}
			}
		else if( *nia >= mnint ){
			goto L_8888;
			}
		else{
			cfmt( "NEED A INTEGER VARIABLE:$",26 );
			cresp();
			}
		if( cmcom.jcom <= cmcom.ncom )
			goto L_2000;
		}

L_8888:
	return( lkia_v );

} /* end of function */

