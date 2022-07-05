#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ lkrtw(kkey, kkey_s, lrtw, krtw, krtw_s, ortw)
char *kkey;   int kkey_s;
int *lrtw;
char *krtw;   int krtw_s;
float ortw[];
{
#define KRTW(I_,J_)	(krtw+(I_)*(krtw_s)+(J_))
	int lkrtw_v;
	int nerr;

	float *const Ortw = &ortw[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed reference time window" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *     lkrtw:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    lrtw:    Set to .TRUE. if RTW was turned on, .FALSE. if
	 *             turned off. Otherwise not changed. [l]
	 *    krtw:    RTW reference times [ka=2]
	 *             First is starting reference time, second stopping time.
	 *    ortw     RTW offset times [fa=2]
	 *             First is starting offset, second is stopping offset.
	 *=====================================================================
	 * MODULE/LEVEL: cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcrtw, cfmt, cresp, lcmore
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820623:  Changed form of output arguments.
	 *    820610:  Rewrote to use standard parsing functions.
	 *    820312:  Factored test for key to LCKEY function.
	 *    810206:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  861128
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkrtw_v = lckey( kkey,kkey_s );

	/* - Return if key was not found. */

	if( !lkrtw_v )
		goto L_8888;

	/* - Use LCRTW to perform parsing.
	 * - Perform standard error recovery if not found. */

L_2000:
	if( lcrtw( lrtw, krtw,krtw_s, ortw ) ){

		}
	else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
		if( lcmore( &nerr ) )
			goto L_2000;
		}

L_8888:
	return( lkrtw_v );

#undef	KRTW
} /* end of function */

