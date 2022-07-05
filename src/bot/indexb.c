#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
int /*FUNCTION*/ indexb(string, string_s)
char *string;   int string_s;
{
	byte null_, carriage;
	int indexb_v, inull, icarriage;

	/*=====================================================================
	 * PURPOSE:  Finds the length of a character string.
	 *           Length is defined by location of last non-blank character.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    STRING:  Character string. [c]
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    INDEXB:  Index of the last non-blank character in the string. [i]
	 *=====================================================================
	 * EXAMPLE:
	 *    CHARACTER*MCMSG STRING
	 *    STRING='Test of INDEXB.'
	 *    IC=INDEXB(STRING)
	 *
	 *    Now IC would be 13.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXA
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Call general character search routine, searching for:
	 *   (1) the first non-occurrence of a SPACE working backwards OR
	 *   (2) the first occurrence of a NULL working forward.
	 *   (3) the first occurrence of a \r line feed.
	 *   */
	indexb_v = indexa( string,string_s, ' ', FALSE, FALSE );
	null_ = '\000';
	inull = indexa( string,string_s, null_, TRUE, TRUE );
	if( inull > 0 )
		indexb_v = min( indexb_v, inull - 1 );

	carriage = '\r';
	icarriage = indexa( string,string_s, carriage, TRUE, TRUE );
	if( icarriage > 0 )
		indexb_v = min( indexb_v, icarriage - 1 );

L_8888:
	return( indexb_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    190618:  Added check for \r line feeds in addition to NULL and SPACE.
	 *    870629:  Changed logic when checking for a NULL.
	 *    850806:  Added check for NULL as well as SPACE.
	 *    830527:  Original version.
	 *===================================================================== */

} /* end of function */

