#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
#include "../../inc/dfm.h"

/* define max length of phase and author names. */
#define  MPH 8
#define  MAU 15

void /*FUNCTION*/ lkt( nerr )
int *nerr ;
{
	int nchstr;
	int lfound ;
	int idx ;
	char tee[3] ;
	char kchar [ MAU + 1 ] ;


	/*=====================================================================
	 * PURPOSE:  To parse a series of "keyed t? phase author" command 
	 *           constructs for xpickphase.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kchar:   Output character string. [c]
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
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, copykc
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    nchstr:  Number of characters in current command token.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970409:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0 ;
	strcpy ( tee , "t0" ) ;

	/* loop between t? keys. */
	while ( lcmore ( nerr ) ) {
	    /* - Check for key. */
	    for ( idx = '0' ; idx <= '9' ; idx++ ) {
		tee[1] = idx ;
		modcase ( TRUE , tee , 2 , tee ) ;
	    	lfound = lckey( tee , 3 );
		if ( lfound )
		    break ;
	    } /* end for ( idx ) */

	    /* if key not found, it's an error. */
	    if ( !lfound ) {
		/* handle error */
		*nerr = 923 ;
		setmsg ( "ERROR" , *nerr ) ;
		outmsg () ;
		return ;
	    }

	    /* reset idx to index global array's later. */
	    idx -= '0' ;

	    /* - Look for phase */
	    /* deal with unlikely case of phase name inter than MCPW chars. */
	    if( ( strcmp(kmcom.kcom[cmcom.jcom - 1] , "'       "  ) == 0 || 
		  strcmp(kmcom.kcom[cmcom.jcom - 1] , "\"       " ) == 0) || 
		  strcmp(kmcom.kcom[cmcom.jcom - 1] , "#       "  ) == 0 ){
		char * temp ;

                /* -- Determine number of characters in string. */
                cmcom.jcom = cmcom.jcom + 1;
                nchstr = (int)( Flnum[cmcom.jcom] + 0.1 );

                /* -- Handle case when string is too int. */
                cmcom.jcom = cmcom.jcom + 1;
                if( nchstr > MPH ){
		    copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, MPH, kchar );

                }
                else{	/* -- Case when string length is ok. */
		    copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, nchstr, kchar );
		    subscpy( kchar, nchstr, MPH , MPH , " " );
                }

                /* if it's the next key, continue */
                if ( ( kchar[0] == 't' || kchar[0] == 'T' ) && isdigit ( kchar[1] ) )
		    continue ;

		/* if it's an empty string, continue */
		if ( strlen ( kchar ) == 0 )
		    continue ;

		/* make sure kchar is terminated */
		kchar[MPH] = '\0' ;
		terminate ( kchar ) ;

                /* -- Adjust counter in current command. */
                cmcom.jcom = cmcom.jcom + (nchstr - 1)/MCPW + 1;

	    }
	    else{	/* - Case where length of text is less than or equal to MCPW. */

                /* -- Copy current command word to output string. */
                subscpy( kchar, 0, MPH , MPH , kmcom.kcom[cmcom.jcom - 1] ) ;

                /* -- Determine length of output string to first blank. */
                kchar[ indexc ( kchar , MPH , ' ' ) ] = '\0' ;

                /* if it's the next key, continue */
                if ( ( kchar[0] == 't' || kchar[0] == 'T' ) && isdigit ( kchar[1] ) )
                    continue ;

                /* -- Increment command counter. */
                cmcom.jcom = cmcom.jcom + 1;

	    }

	    /* put kchar into global variable. */
	    strcpy ( kmdfm.ktPh[idx] , kchar ) ;


	    /* - Look for author */
            /* deal with case of author name inter than MCPW chars. */
            if( ( strcmp(kmcom.kcom[cmcom.jcom - 1] , "'       "  ) == 0 ||
                  strcmp(kmcom.kcom[cmcom.jcom - 1] , "\"       " ) == 0) ||
                  strcmp(kmcom.kcom[cmcom.jcom - 1] , "#       "  ) == 0 ){
                char * temp ;

                /* -- Determine number of characters in string. */
                cmcom.jcom = cmcom.jcom + 1;
                nchstr = (int)( Flnum[cmcom.jcom] + 0.1 );

                /* -- Handle case when string is too int. */
                cmcom.jcom = cmcom.jcom + 1;
                if( nchstr > MAU ){
                    copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, MAU, kchar );

                }
                else{	/* -- Case when string length is ok. */
                    copykc( (char*)kmcom.kcom[cmcom.jcom - 1],9, nchstr, kchar );
                    subscpy( kchar, nchstr, MAU , MAU , " " );
                }

                /* if it's the next key, continue */
                if ( ( kchar[0] == 't' || kchar[0] == 'T' ) && isdigit ( kchar[1] ) )
                    continue ;

                /* make sure kchar is terminated */
                kchar[MAU] = '\0' ;
		terminate ( kchar ) ;

                /* -- Adjust counter in current command. */
                cmcom.jcom = cmcom.jcom + (nchstr - 1)/MCPW + 1;

            }
            else{       /* - Case where length of text is less than or equal to MCPW. */

                /* -- Copy current command word to output string. */
                subscpy( kchar, 0, MAU , MAU , kmcom.kcom[cmcom.jcom - 1] );

                /* -- Determine length of output string to first blank. */
                kchar[ indexc ( kchar , MAU , ' ' ) ] = '\0' ;

                /* if it's the next key, continue */
                if ( ( kchar[0] == 't' || kchar[0] == 'T' ) && isdigit ( kchar[1] ) )
                    continue ;

		/* if it's an empty string, continue */
		if ( strlen ( kchar ) == 0 ) 
		    continue ;

                /* -- Increment command counter. */
                cmcom.jcom = cmcom.jcom + 1;

            }

	    /* convert kchar to lower case for case insensitive comparisons. */
	    modcase ( FALSE , kchar , strlen ( kchar ) , kchar ) ;

            /* put kchar into global variable. */
            strcpy ( kmdfm.ktAu[idx] , kchar ) ;

	} /* end while ( lcmore ( nerr ) ) */

} /* end of function */

