#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#define	MMATCH	5

#include "../../inc/mach.h"
int /*FUNCTION*/ lequal(ksrch, ksrch_s, klist, klist_s, nlist, 
	 index)
char *ksrch;   int ksrch_s;
char *klist;   int klist_s;
int nlist, *index;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	char ktoken[MCMSG+1];
	int lequal_v;
	int imatch[MMATCH], j, j_, nc, nmatch;
        char *strtemp;


	int *const Imatch = &imatch[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To search for a token from a list of tokens.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lequal:  .TRUE. if token was found, .FALSE. otherwise.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    ksrch:   Token to search for. [k]
	 *    klist:   List of possible tokens to search for. [k]
	 *    nlist:   Length of KLIST.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    index:   Index in KLIST of the token found. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  service/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MUNOUT, MUNINP
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    MMATCH:  Maximum number of allowed matches. [ip]
	 *    nmatch:  Number of token-list matches. [i]
	 *    imatch:  Indices (hopefully only one) of matched items. [ia]
	 *    kmsg:    Response from user when there is more than one match. [c]
	 *===================================================================== */
	/* PROCEDURE: */

        fstrncpy( ktoken, MCMSG, " ", 1 );

	nc = indexb( ksrch,ksrch_s );
	modcase( TRUE, ksrch, nc, ktoken );
L_2000:
	nmatch = 0;

	/* - Check token against each item in list.
	 * -- If it is an exact match, return with the result.
	 * -- If only a match through "NC" characters, saved as
	 *    as a possible match and continue search. */

	for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		if( memcmp(ktoken,KLIST(j_,0),strlen(KLIST(j_,0))) == 0 ){
			nmatch = 1;
			Imatch[1] = j;
			goto L_3100;
			}
		else if( memcmp(ktoken,KLIST(j_,0),nc) == 0)
		        {
			if( nmatch < MMATCH )
				nmatch = nmatch + 1;
			Imatch[nmatch] = j;
			}
		}

	/* - If only one match, return with this result. */

L_3100:
	if( nmatch == 1 ){
		*index = Imatch[1];
		lequal_v = TRUE;

		/* - If more than one match, perform standard error recovery. */

		}
	else if( nmatch > 0 ){
                strtemp = malloc(nc+1);
                strncpy(strtemp,ktoken,nc);
                strtemp[nc] = '\0';

                fprintf(MUNOUT," %s%s\n", strtemp, " is an AMBIGUOUS OPTION." );

                free(strtemp);

                fprintf(MUNOUT," %s\n", "Possible matches are:");

		for( j = 1; j <= nmatch; j++ ){
                        fprintf(MUNOUT,"   %s\n", KLIST(Imatch[j] - 1,0));
			}
                fprintf(MUNOUT," %s", "Please enter desired option or <cr>.");
                /* gets(ktoken); */
                fgets( ktoken, MCMSG, stdin ) ;
		nc = indexb( ktoken,MCMSG+1 );
		if( nc <= 0 ){
			lequal_v = FALSE;
			*index = 0;
			goto L_8888;
			}
		else{
			modcase( TRUE, ktoken, nc, ktoken );
			goto L_2000;
			}

		/* - If no match found, return with LEQUAL set to .FALSE. */

		}
	else{
		lequal_v = FALSE;
		*index = 0;
		}

L_8888:
	return( lequal_v );

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    830406:  Added logic to handle quoted string at current token.
	 *    820927:  Fixed bug involving an exact and inexact match.
	 *    820423:  Adjustments due to new command parsing system.
	 *    820312:  Factored test for key to LCCKEY function.
	 *    810207:  Original version.
	 *===================================================================== */

#undef	KLIST
} /* end of function */

