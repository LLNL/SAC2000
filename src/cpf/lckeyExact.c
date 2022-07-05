#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "complex.h"
#include "proto.h"
#define	MCHECK	136

#include "mach.h"
#include "com.h"
int lckeyExact(char* kkey,int kkey_s)
{
	char kcheck[137], ktoken[137];
	int lckeyExact_v, lnoabb;
	int idx, j, jc, jcopy, jicom, jsym, ncabb, ncheck, nckey, 
	 ncsym;
        char *strtemp;


	/*=====================================================================
	 * PURPOSE: To search for a key at the current command symbol.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lckeyExact: .TRUE. if the exact key was found at the current
	 *                command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *             For example, if the keyword is BOTTOM and the minimum
	 *             abbreviation is BOT, then KKEY would be 'BOT#TOM$'.
	 *             If the pound sign was omitted, then B and BO would
	 *             also be acceptable abbreviations.
	 *=====================================================================
	 * NOTE:       You can prepend an apersand ("&") to beginning of
	 *             a keyword if NO abbreviation is to be allowed.
	 *             This is an obsolete option that has been retained
	 *             for compatibility with older codes.  The same effect
	 *             can be achieved by appending a pound sign to the
	 *             end of the keyword but before the dollar sign.
	 *             For example 'ALPHA#$' is the same as "&ALPHA$'.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, kcom, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     indexc, modcase
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    jsym:    Pointer to current command symbol. Differs
	 *             from JCOM if current symbol is a quoted string. [i]
	 *    ncsym:   Number of characters in quoted string. [i]
	 *    jicom:   Number of symbols to increment if key
	 *             is found at current command symbol.  Differs
	 *             from 1 if current symbol is a quoted string. [i]
	 *    mcheck:  The maximum size of a  possible token. [i]
	 *             mcheck should be a multiple of 8.
	 *    mchkmul: The smallest multiple used for the length of mcheck. [i]
	 *    NCKEY:   Number of characters in keyword. [i]
	 *    NCABB:   Number of characters in minimum allowed abbreviation. [i]
	 *    KCHECK:  Local variable used to store keyword. [i]
	 *    NCHECK:  Number of characters used when comparing
	 *             input key to current command symbol. [i]
	 *    KTOKEN:  Local variable used to store current command token. [k]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970702:  Original version, based on lckey.c.  maf
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870730
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of input key (i.e., find trailing dollar sign.) */
	nckey = indexc( kkey,kkey_s, '$' );

	for( idx = 0 ; idx < 136 ; idx++ ) {
	    ktoken[ idx ] = ' ' ;
	    kcheck[ idx ] = ' ' ;
	}
	ktoken[ 136 ] = '\0' ;
	kcheck[ 136 ] = '\0' ;

	/* - If trailing dollar sign is missing find last non-blank character. */

	if( nckey <= 0 )
	    nckey = indexb( kkey,kkey_s );

	/* - If character length of key is still 0, set function value to .TRUE.
	 *   and return immediately.  Do not increment command pointer. */

	if( nckey == 0 ){
	    lckeyExact_v = TRUE;
	    goto L_8888;
	}

	/* - Copy key to local variable, deleting special characters if present. */

	lnoabb = kkey[0] == '&';
	ncabb = indexa( kkey,kkey_s, '#', TRUE, TRUE );
	if( lnoabb ){
            fstrncpy(kcheck,136,kkey+1,kkey_s-2);
	    nckey = nckey - 1;
	}
	else if( ncabb > 0 ){
            fstrncpy(kcheck,136,kkey,ncabb-1);
            memcpy(kcheck+ncabb-1,kkey+ncabb,kkey_s - (ncabb + 1));
	    nckey = nckey - 1;
	}
	else{
            fstrncpy(kcheck,136,kkey,strlen(kkey));
	}

	/* - Determine length of current command symbol and increment in command counter.
	 *   Copy current command symbol to local storage. */

	/* -- Long command symbol case: */
	if( kmcom.kcom[cmcom.jcom - 1][0] == '\'' ){
	    jsym = cmcom.jcom + 2;
	    ncsym = (int)( Flnum[cmcom.jcom + 1] + 0.1 );

	    /* -- Check ktoken size, truncate and issue warning if token is too int. */
	    if( ncsym > MCHECK ){
		ncsym = MCHECK;
		setmsg( "WARNING", 921 );
		wrtmsg( MUNOUT );
		clrmsg();
	    }

	    /* -- Not sure where else this logic is required... so leave it here */
	    jcopy = (ncsym - 1)/MCPW + 1;
	    jc = 1;

	    for( j = 1; j <= jcopy; j++ ){
		strtemp = malloc(MCPW+1);
		strncpy(strtemp,kmcom.kcom[jsym + j - 2],MCPW);
		strtemp[MCPW] = '\0';
		subscpy( ktoken, jc - 1, jc + MCPW - 2, 136, strtemp);
		free(strtemp);
		jc = jc + MCPW;
	    }
	    jicom = jcopy + 2;
	}

	/* -- Normal command symbol case: */
	else{
	    jsym = cmcom.jcom;
	    ncsym = indexb( (char*)kmcom.kcom[cmcom.jcom - 1],9 );
            fstrncpy(ktoken,136,kmcom.kcom[jsym - 1],strlen(kmcom.kcom[jsym - 1]));
	    jicom = 1;
	}

	/* - Determine number of characters to check. */

	if ( nckey != ncsym )
	    return FALSE ;

	ncheck = nckey;

        ncheck = min(ncheck,MCHECK);
	if( ncabb > 0 )
	    ncheck = max( ncheck, ncabb - 1 );

	/* - Convert current command token upper case. */

	modcase( TRUE, ktoken, ncheck, ktoken );

	/* - Check input key versus current token. */

	lckeyExact_v = memcmp(kcheck,ktoken,ncheck) == 0;

	/* - Increment current command pointer if key was found. */

	if( lckeyExact_v )
	    cmcom.jcom = cmcom.jcom + jicom;

L_8888:

	return( lckeyExact_v );

} /* end of function */

