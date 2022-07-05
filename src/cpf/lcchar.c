#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "com.h"
int /*FUNCTION*/ lcchar(mchar, kchar, kchar_s, nchar)
int mchar;
char *kchar;   int kchar_s;
int *nchar;
{
	char ktemp[9];
	int lcchar_v;
	int nchstr;



	/*=====================================================================
	 * PURPOSE:  To parse a "character string" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcchar:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    mchar:   Maxiumum length of KCHAR. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kchar:   Output character string. [c]
	 *    nchar:   Length of KCHAR. [i]
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
	 *    sac:     copykc
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    nchstr:  Number of characters in current command token. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870211:  Added check to see if command has more tokens.
	 *    830208:  Allowed for a numeric token to be used as alphanumeric.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810928:  Modified due to changes in TOKENS.
	 *    810416:  Removed option to use underline as substitute for blank.
	 *    810208:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Return with a value of false if there are no more tokens in command. */
	if( cmcom.jcom > cmcom.ncom ){
		lcchar_v = FALSE;
		return( lcchar_v ) ;
	}
	else{
		lcchar_v = TRUE;
	}

	/* - Logic for int message (quoted string) case.  Occurs for two reasons:
	 *   (1) Text contained embedded blanks. Quotes were supplied by user.
	 *   (2) Text had no embedded blanks but was inter than MCPW, the
	 *       maximum number of characters per word. A pound sign delimiter
	 *       is added by TOKENS in this case. */

	strcpy( ktemp, kmcom.kcom[ cmcom.jcom - 1 ] ) ;
	if( ( strcmp ( kmcom.kcom[ cmcom.jcom - 1 ] , "'       " ) == 0 ||
	      strcmp ( kmcom.kcom[ cmcom.jcom - 1 ] , "\"       " ) == 0) ||
	      strcmp ( kmcom.kcom[ cmcom.jcom - 1 ] , "#       " ) == 0 ) {

		/* -- Determine number of characters in string. */
		cmcom.jcom = cmcom.jcom + 1;
		nchstr = (int)( Flnum[cmcom.jcom] + 0.1 );

		/* -- Handle case when string is too int. */
		cmcom.jcom = cmcom.jcom + 1;
		if( nchstr > mchar ){
			*nchar = mchar;
			copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, mchar, kchar);
			kchar[ 8 ] = '\0' ;
		}

		/* -- Case when string length is ok. */
		else{
			*nchar = nchstr;
			copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, *nchar, kchar);
			subscpy( kchar, *nchar, mchar - 1, kchar_s - 1, " " );
		}

		/* -- Adjust counter in current command. */
		cmcom.jcom = cmcom.jcom + (nchstr - 1)/MCPW + 1;
	}

	/* - Case where length of text is less than or equal to MCPW. */
	else{

		/* -- Copy current command word to output string. */
		subscpy( kchar, 0, mchar - 1, kchar_s - 1, kmcom.kcom[cmcom.jcom - 1]);

		/* -- Determine length of output string to first blank. */
		*nchar = indexc( kchar,kchar_s, ' ' );

		/* -- Increment command counter. */
		cmcom.jcom = cmcom.jcom + 1;

	}

	return( lcchar_v );

} /* end of function */

