#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"

void /* FUNCTION */ xpickauthor ( nerr )
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
     *    970409:  Original version.  maf
     *===================================================================== */
    /* PROCEDURE: */
    *nerr = 0;

    /* - Look for order dependance key, FILE */
	/* also if no tokens are present, assume FILE be default */
    if ( !lcmore ( nerr ) || lckey ( "FILE$" , 6 ) ) {
	/* see if a filename was given. */
	if ( lcchar ( MCPFN , prefsFileName , MCPFN , &nchar ) ) {
	    prefsFileName [ nchar ] = '\0' ;
	    strcpy ( kmdfm.kprefsFileName , prefsFileName ) ;
	}

	/* get list of authors from user-defined file. */
	getprefs ( TRUE , FALSE ) ;

	return ;
    }

    if ( lckey ( "PHASE$" , 7 ) ) {
	/* see if a filename was given. */
        if ( lcchar ( MCPFN , prefsFileName , MCPFN , &nchar ) ) {
	    prefsFileName [ nchar ] = '\0' ;
	    strcpy ( kmdfm.kprefsFileName , prefsFileName ) ;
	}

	/* get authors and phases from the user-defined file. */
	getprefs ( TRUE , TRUE ) ;

	return ;
    }

    /* free up kmdfm.kauthors */
    if ( kmdfm.kauthors != NULL ) {
	for ( idx = 0 ; idx < cmdfm.iauthors ; idx++ )
	    free ( kmdfm.kauthors[idx] ) ;
	free ( kmdfm.kauthors ) ;
	cmdfm.iauthors = 0 ;
    } /* end if ( kmdfm.kauthors != NULL ) */

    /* - Loop on each token in command: */
    while ( lcmore ( nerr ) ) {
	if ( nAuthors % 10 == 0 ) {	/* stop every 10 authors and get space */
	    char **temp = NULL ;

	    temp = ( char ** ) realloc ( (void *) kmdfm.kauthors ,
	      ( nAuthors + 10 ) * sizeof ( char * ) ) ;
	    if ( temp == NULL ) {
		/* handle error */
		goto L_8888 ;
	    } /* end temp == NULL */
	    else
		kmdfm.kauthors = temp ;
	} /* end if ( nAuthors % 10 == 0 ) */

	/* allocate a string for the author name. */
	kmdfm.kauthors[nAuthors] = (char *) malloc ( 16 * sizeof ( char ) ) ;
	if ( kmdfm.kauthors[nAuthors] == NULL ) {
	    /* handle error */
	    goto L_8888 ;
	} /* end if ( kmdfm.kauthors[nAuthors] == NULL ) */

	/* copy the author name. */
	lcchar ( 15 , kmdfm.kauthors[nAuthors] , 15 , &length ) ;
	kmdfm.kauthors[nAuthors][length] = '\0' ;

	/* convert to lower case for case insensitive comparisons. */
	modcase ( FALSE , kmdfm.kauthors[nAuthors] ,
		  length, kmdfm.kauthors[nAuthors] ) ;

	nAuthors++ ;    /* increment number of authors. */

    } /* end while */

    cmdfm.iauthors = nAuthors ;		/* save number of authors. */

    return ;

L_8888:
    /* only come here on allocation error */
    *nerr = 301 ;
    setmsg ( "ERROR" , *nerr ) ;
    outmsg () ;
    clrmsg () ;

    /* free up memory */
    if ( kmdfm.kauthors != NULL ) {
	for ( idx = 0 ; idx < nAuthors ; idx ++ )
	    free ( kmdfm.kauthors[idx] ) ;
	free ( kmdfm.kauthors ) ;
    } /* end if ( kmdfm.kauthors != NULL ) */
}
