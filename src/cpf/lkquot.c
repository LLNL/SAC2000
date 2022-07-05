#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkquot(kkey, kkey_s, mquot, kquot, kquot_s, 
	 nquot)
char *kkey;   int kkey_s;
int mquot;
char *kquot;   int kquot_s;
int *nquot;
{
	int lkquot_v;
	int nchstr, nerr;



	/*=====================================================================
	 * PURPOSE:    (To parse a "keyed character string" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkquot:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    mquot:   Maxiumum length of KQUOT. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kquot:   Output character string. [c]
	 *    nquot:   Length of KQUOT. [i]
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
	 *    sac:     lckey, copykc
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    nchstr:  Number of characters in current command token.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    910113:  Adjustment of counter jcom is added a la lcquot
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810928:  Modified due to changes in TOKENS.
	 *    810416:  Removed option to use underline as substitute for blank.
	 *    810208:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lkquot_v = lckey( kkey,kkey_s );

	/* - Return if key was not found. */

	if( !lkquot_v )
		goto L_8888;

	/* - Logic for quoted string case. Quotes can occur for two reasons:
	 *   (1) Text contained embedded blanks. Quotes were supplied by user.
	 *   (2) Text had no embedded blanks but was inter than MCPW, the
	 *       maximum number of characters per word. Quotes are added
	 *       by TOKENS in this case. */

L_2000:
	if( strcmp(kmcom.kcom[cmcom.jcom - 1],"'       ") == 0 || strcmp(kmcom.kcom[cmcom.jcom - 1]
	 ,"\"       ") == 0 ){

		/* -- Determine number of characters in string. */
		cmcom.jcom = cmcom.jcom + 1;
		nchstr = (int)( Flnum[cmcom.jcom] + 0.1 );

		/* -- Handle case when string is too int. */
		cmcom.jcom = cmcom.jcom + 1;
		if( nchstr > mquot ){
			*nquot = mquot;
			copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, *nquot, kquot);

			/* -- Case when string length is ok. */
			}
		else{
			*nquot = nchstr;
			copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, *nquot, kquot);
			subscpy( kquot, *nquot, mquot - 1, kquot_s - 1, " " );
			}

		/* -- Adjust counter in current command. */
		cmcom.jcom = cmcom.jcom + (nchstr - 1)/MCPW + 1;
		}
	else{
		cfmt( "NEED AN QUOTED STRING:$",24 );
		cresp();
		if( lcmore( &nerr ) )
			goto L_2000;
		}

L_8888:
	return( lkquot_v );

} /* end of function */

