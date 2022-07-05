#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#define	MMATCH	5

#include "mach.h"
#include "com.h"
int /*FUNCTION*/ lklist(kkey, kkey_s, klist, klist_s, nlist, index)
char *kkey;   int kkey_s;
char *klist;   int klist_s;
int nlist, *index;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	char ktoken[9];
	int lklist_v;
	int imatch[MMATCH], j, j_, jicom, nchar, nerr, nmatch;

	int *const Imatch = &imatch[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed alphanumeric list" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklist:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    klist:   List of possible alphanumeric tokens to search
	 *             for if key was found [k]
	 *    nlist:   Length of KLIST. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    index:   Index in KLIST of the alphanumeric token found. [i]
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
	 *    sac:     modcase, indexb, lckey, cfmt, cresp, wrindx
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    mmatch:  Maximum number of allowed matches. [ip]
	 *    nmatch:  Number of token-list matches. [i]
	 *    imatch:  List of indices of matched tokens. [ia]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870730:  Added logic to convert current token to uppercase.
	 *    860314:  Changed logic due to use of WRINDX instead of WRLIST.
	 *    820927:  Fixed bug involving an exact and inexact match.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check for key. */
	lklist_v = lckey( kkey,kkey_s );

	/* - If key was found: */

	if( lklist_v ){
L_2000:
	    nmatch = 0;

	    /* - Copy next token to local storage and convert to upper case.
	     *   (See if token is a quoted string or not.) */

	    if( strcmp(kmcom.kcom[cmcom.jcom - 1],"'       ") == 0 ){
		modcase( TRUE, (char*)kmcom.kcom[cmcom.jcom + 1],MCPW, ktoken );
		nchar = MCPW;
		jicom = ((int)( Flnum[cmcom.jcom + 1] + 0.1 ) - 1)/MCPW + 3;
	    }
	    else{
		modcase( TRUE, (char*)kmcom.kcom[cmcom.jcom - 1],MCPW, ktoken );
		nchar = indexb( ktoken,9 );
		jicom = 1;
	    }
	    ktoken[ MCPW ] = '\0' ;

	    /* -- Check token against each item in list.
	     * --- If it is an exact match, return with the result.
	     * --- If only a match through "NCHAR" characters, saved as
	     *     as a possible match and continue search. */
	    for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		if( memcmp(ktoken,KLIST(j_,0),strlen(ktoken)) == 0 ){
		    nmatch = 1;
		    Imatch[1] = j;
		    break ;
		}
		else if( memcmp(ktoken,KLIST(j_,0),nchar) == 0 ){
		    if( nmatch < MMATCH )
			nmatch = nmatch + 1;
		    Imatch[nmatch] = j;
		}
	    }

	    /* -- If only one match, return with this result. */
	    if( nmatch == 1 ){
		*index = Imatch[1];
		cmcom.jcom = cmcom.jcom + jicom;
	    }

	    /* -- If more than one match, perform standard error recovery */
	    else if( nmatch > 0 ){
		cfmt( "AMBIGUOUS OPTION$",18 );
		fprintf(MUNOUT," Possible matches are:\n");
		wrindx( klist,klist_s, imatch, nmatch );
		cresp();
		if( lcmore( &nerr ) )
		    goto L_2000;
	    }

	    /* -- If no match, also perform standard error recovery but
	     *    with a different message. */
	    else{
		cfmt( "ILLEGAL OPTION$",16 );
		fprintf(MUNOUT," Available options are:\n");
		wrlist( klist,klist_s, nlist );
		cresp();
		if( lcmore( &nerr ) )
		    goto L_2000;
	    }
	}

L_8888:
	return( lklist_v );

#undef	KLIST
} /* end of function */

