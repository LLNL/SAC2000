#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lklogc(kkey, kkey_s, logv, kchar, kchar_s)
char *kkey;   int kkey_s;
int *logv;
char *kchar;   int kchar_s;
{
	int lklogc_v;
	int nchar, nerr, nret;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed logical/character variable" construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklogc:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    logv:    Logical variable found in command. [l]
	 *    kchar:   Integer variable found in command. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, lclog, lcchar, cfmt, cresp, lcmore
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890925:  Fixed bug involving setting of "logv".
	 *    860306:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890925
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of character variable. */
	nchar = (kchar_s - 1);

	/* - Check for key. */

	lklogc_v = lckey( kkey,kkey_s );

	/* - Get logical variable from next symbol if key was found.
	 * - Perform standard error recovery if not found. */

	if( lklogc_v ){
L_2000:
		if( lclog( logv ) ){
			}
		else if( lcchar( nchar, kchar,kchar_s, &nret ) ){
			*logv = TRUE;
			}
		else{
			cfmt( "Need an \"on\", an \"off\", or an alpha:$",38 );
			cresp();
			if( lcmore( &nerr ) )
				goto L_2000;
			}
		}

L_8888:
	return( lklogc_v );

} /* end of function */

