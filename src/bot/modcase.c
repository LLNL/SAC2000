#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void modcase(int upflag, char* input, int nchar, char* output)
{
	int quoted;
	byte kchar;
	int itemp, jchar;
	static int iconv = 32;

	/*=====================================================================
	 * PURPOSE: To modify the case of a character string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    UPFLAG:   Set to .TRUE. to convert to upper case,
	 *              to .FALSE. for lowercase. [l]
	 *              Text between single or double quotes is unchanged.
	 *    INPUT:    Input character string. [c]
	 *    NCHAR:    Number of characters in INPUT. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    OUTPUT:   Output character string. May be same as INPUT. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    F77LIB:  CHAR, ICHAR
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    ICONV:   Offset used to convert from lower case to upper case.
	 *             INTEGER CONSTANT: set to "-32" for ASCII character set.
	 *             See SPECIAL NOTE below.
	 *    KCHAR:   Character that is currently being converted.
	 *=====================================================================
	 * SPECIAL NOTE:
	 * - Defined for the ASCII character set.  Only the value of ICONV
	 *   would probably change for a different character set.
	 *   For example, ICONV would be "-64" for the EBCDIC character set.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900508:  Removed use of ktemp from previous fix. (jet)
	 *    871214:  Added ktemp and length variables for bug fix. (bkh)
	 *    870514:  Fixed bug when converting to lower case.
	 *    861229:  Original version based upon UPCASE.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870514
	 *===================================================================== */
	/* PROCEDURE: */
	quoted = FALSE;

	/* - For each character in input string: */

	for ( jchar = 0 ; jchar < nchar ; jchar++ ){

		/* -- Copy to local variable. */

		kchar = input[jchar];

		/* -- Toggle quote flag if necessary. */
		if( kchar == '"' || kchar == '\'' ){
			quoted = !quoted;

			/* -- If a character and not inside quotation marks:
			 * --- Convert character to integer.
			 * --- Added conversion offset.
			 * --- Convert from integer back to character. */
		}
		else if( !quoted ){
			if( (upflag && (kchar >= 'a'))  && (kchar <= 'z') ){
				itemp = ( kchar );
				itemp = itemp - iconv;
				kchar = (itemp);
			}
			else if( (!upflag && (kchar >= 'A')) && (kchar <= 'Z') ){
				itemp = ( kchar );
				itemp = itemp + iconv;
				kchar = (itemp);
			}
		}

		/* -- Copy local variable to output string. */

		output[jchar] = kchar;

	}

L_8888:
	return;

} /* end of function */

