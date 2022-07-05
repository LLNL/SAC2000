#include <stdio.h>
#include <strings.h>
#include <string.h>
#include "mach.h"
#include "hdr.h"
#include "extfunc.h"
#include "cssStrucs.h"
#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "smDataIO.h"

/*
	This routine makes sure that station names and components (channels)
	are defined.  If one is undefigned, it is given a unique name.  

	Design Note:  The definision and initialization of the tree
		      variable is distributed into the if blocks because
		      this function will be called a lot, and will usually
		      not be needed.  The code will run a little faster
		      by not defining and initializing the tree in the 
		      vast majority of cases where it will not be used.
*/

int uniqueStaAndChan ( ) 
{
    int returnValue = FALSE ;

    if ( !strcmp ( kstnm , AUNDEF ) ) {
	int idx ;
	DBlist tree ;

	/* get tree for default wordset */
	tree = smGetDefaultTree () ;

	strcpy ( kstnm , MakeUniqueSiteName ( tree , "sta" ) ) ;

	if ( strlen ( kstnm ) < 8 ) {
	    for ( idx = strlen ( kstnm ) ; idx < 8 ; idx++ )
		kstnm[ idx ] = ' ' ;
	    kstnm[ 8 ] = '\0' ;
	}

	returnValue = TRUE ;
    }

    if ( !strcmp ( kcmpnm , AUNDEF ) ) {
	int idx ;
	DBlist tree ;

	/* get tree for default wordset */
	tree = smGetDefaultTree () ;

	strcpy ( kcmpnm , MakeUniqueChanName ( tree , kstnm , "Q" ) ) ;

        if ( strlen ( kcmpnm ) < 8 ) {
            for ( idx = strlen ( kcmpnm ) ; idx < 8 ; idx++ )
                kcmpnm[ idx ] = ' ' ;
            kcmpnm[ 8 ] = '\0' ;
        }

	returnValue = TRUE ;
    }

    return returnValue ;
}
