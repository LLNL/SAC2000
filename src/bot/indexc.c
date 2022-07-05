#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
int /*FUNCTION*/ indexc(string, string_s, kchar)
char *string;   int string_s;
int kchar;
{
	int indexc_v, j;

	/*=====================================================================
	 * PURPOSE:  Finds the length of a character string.
	 *           String is terminated by first occurance of a character
	 *           or by the end of the string.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    STRING:  Character string. [c]
	 *    KCHAR:   Requested character. [c1]
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    INDEXC:  Index of the character before the requested character. [i]
	 *=====================================================================
	 * EXAMPLE:
	 *    CHARACTER*(MCMSG) STRING
	 *    STRING='#Test of INDEXC.#'
	 *    IC=INDEXC(STRING)
	 *
	 *    Now IC would be 14.
	 *    The string is therefore STRING(2:IC).
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXA
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Call general character search routine, searching for the first
	 *   occurance of the delimiter searching forward. */
	j = indexa( string,string_s, kchar, TRUE, TRUE );

	/* - If CHAR was found, return with pointer at previous character.
	 *   If CHAR was not found, return with pointer at end of string. */

	if( j > 0 ){
		indexc_v = j - 1;
		}
	else{
		indexc_v = (string_s - 1);
		}

L_8888:
	return( indexc_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    920326:  Changed parameter char to kchar.
	 *    830527:  Original version.
	 *===================================================================== */

} /* end of function */

