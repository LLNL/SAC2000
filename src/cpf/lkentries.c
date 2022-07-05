#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ lkentries(kkey, kkey_s, klist, klist_s, nlist, 
	 llist)
char *kkey;   int kkey_s;
char *klist;   int klist_s;
int nlist;
int llist[];
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	char kmsg[MCMSG+1];
	int lcentries, lkentries_v;
	int index, j, j_, nerr;

	int *const Llist = &llist[0] - 1;


	/*=====================================================================
	 * PURPOSE: To parse a "keyed alphanumeric entry list" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkentries:  .TRUE. if the construct was found at the current
	 *                command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to match. [k]
	 *    klist:   List of possible alphanumeric tokens to search
	 *             for if key was found [k]
	 *    nlist:   Length of KLIST. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    llist:   Logical array indicating which entries were found.
	 *             llist(j) is set to .TRUE. if  entry klist(j) was found
	 *             and set to .FALSE. if not found.
	 *             The keyword "ALL" sets all entries to .TRUE.
	 *             The keyword "NONE" sets all entries to .FALSE.
	 *             Parsing ends when the command is exhausted or a
	 *             token is found that does not match the entry list.
	 *=====================================================================
	 * MODULE/LEVEL:  com/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPW, MCMSG
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lckey, lclist, cfmt, wrlist, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890105:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890105
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Check next token for key.
	 *   Return if key is not found. */
	lkentries_v = lckey( kkey,kkey_s );
	if( !lkentries_v )
		goto L_8888;

	/* - Initialize output values. */

	lcentries = FALSE;
	for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		Llist[j] = FALSE;
		}

	/* - Loop until command is exhausted or the next token does not match. */

L_2000:
	if( lcmore( &nerr ) ){

		/* -- Test for individual entries. */
		if( lclist( klist,klist_s, nlist, &index ) ){
			lcentries = TRUE;
			Llist[index] = TRUE;
			goto L_2000;

			/* -- Test for ALL keyword. */
			}
		else if( lckey( "ALL#$",6 ) ){
			lcentries = TRUE;
			for( j = 1; j <= nlist; j++ ){
				j_ = j - 1;
				Llist[j] = TRUE;
				}

			/* -- Test for NONE keyword. */
			}
		else if( lckey( "NONE#$",7 ) ){
			lcentries = TRUE;
			for( j = 1; j <= nlist; j++ ){
				j_ = j - 1;
				Llist[j] = FALSE;
				}

			/* -- Perform standard error recovery if we still have not found an entry. */
			}
		else if( !lcentries ){
                        fstrncpy(kmsg,MCMSG,"NEED \"ALL\", \"NONE\", or one or more of the following:",
                                   strlen("NEED \"ALL\", \"NONE\", or one or more of the following:"));
			cfmt( kmsg,MCMSG+1 );
			wrlist( klist,klist_s, nlist );
			cresp();
			goto L_2000;

			}

		}

L_8888:
	return( lkentries_v );

#undef	KLIST
} /* end of function */

