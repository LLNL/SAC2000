#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "com.h"
int /*FUNCTION*/ lcidi(int1 , int2 )
int *int1 , *int2 ;
{
	int lcidi_v , padding = FALSE ;
	char token[ 9 ] , *ptr ;
	int temp ;


	/*=====================================================================
	 * PURPOSE: To parse a "integer dash (-) integer" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    LCIDI:   .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    int1:    Fisrt integer variable found in command. [i]
	 *    int2:    Second integer found. [i]
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
	 * MODIFICATION HISTORY:
	 *    990521:  Original version.  Plagerized from lcint.  (maf)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */

	/* String should conform to a stict format, wherein there may be one
	   dash (-) and the rest of the characters are digits (the only
	   permited whitespace is end padding).  The dash may not be the 
	   first character, nor the last character before the padding. */

	strcpy ( token , kmcom.kcom[ cmcom.jcom - 1 ] ) ;	/* copy token */
	terminate( token ) ;				/* remove end padding */

	if ( !isdigit ( token[0] ) )
	    return FALSE ;

	for ( ptr = token ; *ptr && isdigit ( *ptr ) ; ptr++ )
	{ /* do nothing */ }

	if ( *ptr != '-' )
	    return FALSE ;

	ptr++ ;

	while ( *ptr ) {
	    if ( !isdigit ( *ptr ) )
		return FALSE ;
	    ptr++ ;
	}


	/* If we get here, the token is of the correct form. */

	/* get the two ints, and get out of here. */
	ptr = strtok ( token , "-" ) ;
	*int1 = atol ( ptr ) ;
	ptr = strtok ( NULL , "-" ) ;
	*int2 = atol ( ptr ) ;

	if ( *int1 > *int2 ) {
	     temp = *int1 ;
	    *int1 = *int2 ;
	    *int2 =  temp ;
	}
	
	cmcom.jcom = cmcom.jcom + 1;

	return TRUE ;

} /* end of function */

