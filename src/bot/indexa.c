#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
int /*FUNCTION*/ indexa(string, string_s, kchar, lfwd, locc)
char *string;   int string_s;
int kchar;
int lfwd, locc;
{
	int _d_l, _d_m, _do0, _do1, ic, ic1, ic2, ic3, ic_, indexa_v, 
	 nc, do_count;

	/*=====================================================================
	 * PURPOSE: Searches for the occurrence of a single character within
	 *          a character string.  The search can be performed in the
	 *          forward or backward directions.  The search can be made
	 *          for the occurrence of the non-occurrence of the character.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    STRING:  Character string. [c]
	 *    CHAR:    Character to search for. [c1]
	 *    LFWD:    Forward search flag. [l]
	 *             Set to .TRUE. if string is to be forward searched
	 *             and .FALSE. if string is to search backward.
	 *    LOCC:    Occurrence flag. [l]
	 *             Set to .TRUE. if search is for first occurrence of CHAR
	 *             and .FALSE. if search is for first non-occurrence.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    INDEXA:  The index of the character if found, 0 otherwise. [i]
	 *=====================================================================
	 * EXAMPLE:
	 *    CHARACTER*MCMSG STRING
	 *    STRING='$aabcde$'
	 *    IC1=INDEXA2(STRING,'c',.TRUE.,.TRUE.)
	 *    IC2=INDEXA2(STRING,'a',.TRUE.,.FALSE.)
	 *    IC3=INDEXA2(STRING,'a',.FALSE.,.TRUE.)
	 *    IC4=INDEXA2(STRING,'e',.FALSE.,.FALSE.)
	 *
	 *    For this example, IC1 would be 5, IC2 would be 4,
	 *    IC3 would be 3, and IC$ would be 6.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NC:      Maximum length of character string. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of character string. */
	nc = (string_s - 1);
        if ( nc == 0 ) nc = 1;

	/* - Set up loop parameters based on search direction. */

	if( lfwd ){
		ic1 = 1;
		ic2 = nc;
		ic3 = 1;
		}
	else{
		ic1 = nc;
		ic2 = 1;
		ic3 = -1;
		}

	/* - Search each character in input string for requested character. */

	indexa_v = 0;
        do_count = nc;
	for( ic = ic1, do_count = nc; do_count > 0; ic += ic3, do_count-- ){
		ic_ = ic - 1;
		if( string[ic - 1] == kchar && locc ){
			indexa_v = ic;
			goto L_8888;
			}
		else if( string[ic - 1] != kchar && !locc ){
			indexa_v = ic;
			goto L_8888;
			}
		}

L_8888:
	return( indexa_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    190618:  Corrected every instance of occurrence to the proper spelling.
	 *    920326:  Changed parameter char to kchar.
	 *    830505:  Original version.
	 *===================================================================== */

} /* end of function */

