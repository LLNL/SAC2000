#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ lklog2(kkey, kkey_s, ktrue, ktrue_s, kfalse, 
	 kfalse_s, logv)
char *kkey;   int kkey_s;
char *ktrue;   int ktrue_s;
char *kfalse;   int kfalse_s;
int *logv;
{
	char kmsg[MCMSG+1];
	int lklog, lklog2_v;
	int nerr, nfalse, ntrue;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed logical variable" command construct.
	 *          The first token turns logical flag on, the second token
	 *          turns it off.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklog2:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    ktrue:   The token to search for corresponding to .TRUE. [c]
	 *    kfalse:  The token to search for corresponding to .FALSE. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    logv:    Logical variable found in command. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPW
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, cfmt, cresp, lcmore
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kmsg:    Used to format error message [c].
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    961009:  Fixed 2 bugs: 1. endless loop if token was bad
	 *                           2. the last letter of the token was ignored
	 *    820622:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820622
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lklog2_v = lckey( kkey,kkey_s );

	/* - Return if key was not found. */

	if( !lklog2_v )
		goto L_8888;

	/* - Check for the "true" token. */

L_1000:
	if( lckey( ktrue,ktrue_s ) ){
		*logv = TRUE;
		lklog = TRUE;

		/* - Check for the "false" token. */

		}
	else if( lckey( kfalse,kfalse_s ) ){
		*logv = FALSE;
		lklog = TRUE;

		/* - Neither token found.
		 * - Attempt standard error correction procedure. */

		}
	else{
		ntrue = indexc( ktrue,ktrue_s, '$' );
		nfalse = indexc( kfalse,kfalse_s, '$' );
                fstrncpy(kmsg,MCMSG,"NEED EITHER \"",13);
                memcpy(kmsg+13,ktrue,ntrue);			/* removed "-1"  maf 961009 */
                memcpy(kmsg+13+ntrue,"\" or \"",6);		/* removed "-1"  maf 961009 */
                memcpy(kmsg+13+ntrue+6,kfalse,nfalse);		/* removed "-1"  maf 961009 */
                memcpy(kmsg+13+ntrue+6+nfalse,"\":$",3);	/* removed "-1"  maf 961009 */
		cresp();
		/* next 2 lines removed to stop endless loop if parameter is bad. maf 961009 */
/*		if( lcmore( &nerr ) )
			goto L_1000;	*/

		}

L_8888:

	return( lklog2_v );

} /* end of function */

