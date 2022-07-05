#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ wrlist(klist, klist_s, nlist)
char *klist;   int klist_s;
int nlist;
{
#define KLIST(I_,J_)	(klist+(I_)*(klist_s)+(J_))
	char kmsg[MCMSG+1];
	int _d_l, _d_m, _do0, _do1, j, j1, j1_, j2, j_, jc1, jc2, 
	 ncmax, nperl;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE:  To write a list of tokens to the user's terminal.
	 *           Tokens are written with two spaces between them. Number
	 *           of tokens per line based on length of intest token.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    klist:   List of tokens to write. [ca]
	 *    nlist:   Number of tokens to write. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  service/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MUNOUT
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860124:  Added ability to determine format based on token size.
	 *    840119:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860124
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine maximum number of characters in list of tokens. */
	ncmax = 0;
	for( j = 1; j <= nlist; j++ ){
		j_ = j - 1;
		ncmax = max( ncmax, indexb( KLIST(j_,0),klist_s ) );
		}

	/* - Determine how many tokens to put on each line.
	 *   (Two spaces will be put between each token.) */

	nperl = MCMSG/(2 + ncmax);

	/* - Write each line to standard output. */

	for( j1 = 1; j1 <= nlist; j1 += nperl){
		j1_ = j1 - 1;
		j2 = min( j1 + nperl - 1, nlist );
                memset(kmsg,(int)' ',MCMSG);
                kmsg[MCMSG] = '\0';
		jc1 = 3;
		for( j = j1; j <= j2; j++ ){
			j_ = j - 1;
			jc2 = jc1 + ncmax - 1;
			subscpy( kmsg, jc1 - 1, jc2 - 1, MCMSG, KLIST(j_,0) );
			jc1 = jc2 + 2;
			}
                strtemp = malloc(jc2+1);
                strncpy(strtemp,kmsg,jc2);
                strtemp[jc2] = '\0';
                fprintf(MUNOUT," %s\n",strtemp);
                free(strtemp);
		}

L_8888:
	return;

#undef	KLIST
} /* end of function */

