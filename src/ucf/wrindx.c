#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ wrindx(klist, klist_s, ilist, nlist)
char *klist;   int klist_s;
int ilist[], nlist;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	char kmsg[MCMSG+1], ktemp[21];
	int _d_l, _d_m, _do0, _do1, j, j1, j1_, j2, j_, jc1, jc2, 
	 nc, ncmax, nperl;

	int *const Ilist = &ilist[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To write a list of tokens to the user's terminal.
	 *           A list of index numbers determines order of writing.
	 *           Tokens are written with two spaces between them. Number
	 *           of tokens per line based on length of intest token.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KLIST:   List of tokens to write. [ca]
	 *    ILIST:   Index numbers of tokens to be written. [ia]
	 *    NLIST:   Number of tokens to write. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG, MUNOUT
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860314:  Original version (based on WRLIST.)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860314
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine maximum number of characters in list of tokens. */
	ncmax = 0;
	for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		j1 = Ilist[j];
		fstrncpy( ktemp, 20, KLIST(j1 - 1,0), strlen(KLIST(j1 - 1,0)));
		nc = indexb( ktemp,21 );
		ncmax = max( ncmax, nc );
		}

	/* - Determine how many tokens to put on each line.
	 *   (Two spaces will be put between each token.) */

	nperl = MCMSG/(2 + ncmax);

	/* - Write each line to standard output. */

	for( j1 = 1; j1 <= nlist; j1 += nperl ){
		j1_ = j1 - 1;
		j2 = min( j1 + nperl - 1, nlist );
		fstrncpy( kmsg, MCMSG, " ", 1 );
		jc1 = 3;
		for( j = j1; j <= j2; j++ ){
			j_ = j - 1;
			jc2 = jc1 + ncmax - 1;
			fstrncpy( ktemp, 20, KLIST(Ilist[j] - 1,0),
                                      strlen(KLIST(Ilist[j] - 1,0)));
			subscpy( kmsg, jc1 - 1, jc2 - 1, MCMSG, ktemp );
			jc1 = jc2 + 2;
			}
                fprintf(MUNOUT," %s\n",kmsg);
		}

L_8888:
	return;

#undef	KLIST
} /* end of function */

