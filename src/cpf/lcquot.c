#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcquot(mquot, kquot, kquot_s, nquot)
int mquot;
char *kquot;   int kquot_s;
int *nquot;
{
	int lcquot_v;
	int nchstr;



	/*=====================================================================
	 * PURPOSE: To parse a "quoted character string" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcquot:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
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
	 *    com:     jcom, ncerr
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     copykc
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890828:  Minor modified to make it palatable to DEC ULTRIX f77.
	 *    820505:  Now allowing both single and double quotes.
	 *    820423:  Adjustments due to new command parsing system.
	 *    810928:  Modified due to changes in TOKENS.
	 *    810416:  Removed option to use underline as substitute for blank.
	 *    810208:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return with .FALSE. if next token is not a single (')
	 *   or double (") quotation mark. */
	if( strcmp(kmcom.kcom[cmcom.jcom - 1],"'       ") == 0 || strcmp(kmcom.kcom[cmcom.jcom - 1]
	 ,"\"       ") == 0 ){
		lcquot_v = TRUE;
		}
	else{
		lcquot_v = FALSE;
		}

	if( lcquot_v ){

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

L_8888:
	return( lcquot_v );

} /* end of function */

