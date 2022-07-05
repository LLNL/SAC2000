#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"

void /* FUNCTION */ xpickphase ( nerr )
int *nerr;
{

    /* Declare local variables. */
    char prefsFileName [ MCPFN + 1 ] ;
    int nchar , idx , length , nAuthors = 0 ;

    /*=====================================================================
     * PURPOSE:  To parse the parameter-setting command PICKAUTHOR.
     *           This command controls the picks read by READCSS. 
     *=====================================================================
     * OUTPUT ARGUMENTS:
     *    NERR:    Error flag. Set to 0 if no error occurred.
     *=====================================================================
     * MODULE/LEVEL:  
     *=====================================================================
     * GLOBAL INPUT:
     *=====================================================================
     * GLOBAL OUTPUT:
     *    DFM:     IAUTHORS, KAUTHORS, KTPH, KTAU
     *=====================================================================
     * SUBROUTINES CALLED:
     *=====================================================================
     * MODIFICATION HISTORY:
     *    970409:  Original version.
     *===================================================================== */

    /* PROCEDURE: */
    *nerr = 0;

    /* - Look for order dependance key, FILE */
	/* if no tokens present, assume FILE by default. */
    if ( !lcmore( nerr ) || lckey ( "FILE$" , 6 ) ) {
	/* get file name if present. */
	if ( lcchar ( MCPFN , prefsFileName , MCPFN , &nchar ) ) {
	    prefsFileName [ nchar ] = '\0' ;
	    strcpy ( kmdfm.kprefsFileName , prefsFileName ) ;
	}

	/* get list of phases from user-defined file. */
	getprefs ( FALSE , TRUE ) ;

	return ;
    }

    /* - Look for order dependance key, AUTHOR */
    if ( lckey ( "AUTH#OR$" , 9 ) ) {
	/* get file name if present. */
        if ( lcchar ( MCPFN , prefsFileName , MCPFN , &nchar ) ) {
	    prefsFileName [ nchar ] = '\0' ;
	    strcpy ( kmdfm.kprefsFileName , prefsFileName ) ;
	}

	/* get authors and phases from the user-defined file. */
	getprefs ( TRUE , TRUE ) ;

	return ;
    }

    /* Loop through order independent keys, t0 - t9, picking up
	phases and authors. */
    lkt ( nerr ) ;
    return ;
}
