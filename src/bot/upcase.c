#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ upcase(kinput, nchar, koutpt, koutpt_s)
char *kinput;
int nchar;
char *koutpt;   int koutpt_s;
{
	char ktemp[133];
	byte kchar;
	int itemp, jchar, length;
	static int iconv = -32;

	/*=====================================================================
	 * PURPOSE: To convert a character string to upper case.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KINPUT:  Character string with (possible) lower case letters.
	 *    NCHAR:   Number of characters in KINPUT.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KOUTPT:  Character string with lower case letters
	 *             converted to upper case.
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
	 *   For example, ICONV would be "+64" for the EBCDIC character set.
	 *===================================================================== */
	/* PROCEDURE: */

	/* - Initialize output string to blanks: */
        memset(ktemp,(int)' ',132);
        ktemp[132] = '\0';

	/* - For each character in input string: */

	length = nchar;
	if( length > 132 )
		length = 132;
	for ( jchar = 1 ; jchar <= length ; jchar++ ){

		/* -- Copy to local variable. */

		kchar = kinput[jchar - 1];

		/* -- If between "a" and "z":
		 * --- Convert character to integer.
		 * --- Added conversion offset.
		 * --- Convert from integer back to character. */

		if( (kchar >= 'a') && (kchar <= 'z') ){
			itemp = ( kchar );
			itemp = itemp + iconv;
			kchar = (itemp);
		}

		/* -- Copy local variable to output string. */

		ktemp[jchar - 1] = kchar;

	} /* end for */

        if ( koutpt_s == 1 )
		*koutpt = ktemp[0];
        else
		fstrncpy(koutpt,koutpt_s - 1,ktemp,jchar-1);

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    871214:  Added ktemp and length variables for bug fix. (BKH)
	 *    820309:  Original version.
	 *===================================================================== */

} /* end of function */

