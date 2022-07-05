#include <stdio.h>
#include <string.h>
#include <ctype.h>


/* Takes a space-padded string, and terminates it at the first space. *
 * Returns 0 if terminated successfully, or 1 if no padding found.    */

/* Note:  Requires that string be terminated at the end of the padding. */

int terminate ( char * paddedString )
{
    char * cPtr ;
    int nLen ;

    for ( nLen = strlen ( paddedString ) - 1 ; isspace ( paddedString[ nLen ] ) ;
	  nLen-- )
	paddedString[ nLen ] = '\0' ;
    return 0;
}

