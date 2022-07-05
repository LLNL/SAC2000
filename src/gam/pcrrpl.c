#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MCTEXT	80

#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ pcrrpl(nunrpl, kchar, kchar2, lend, lquit)
FILE *nunrpl;
byte *kchar, *kchar2;
int *lend, *lquit;
{
	char ktext[MCMSG+1];
	int icloc1, icloc2, icpntr, itype, nctext, nctok, nerr, numchar;
	void *_p0;
	static char kbdlin[26] = "Bad line in replay file: ";
        char *strtemp1, *strtemp2;

	/*=====================================================================
	 * PURPOSE:
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *=====================================================================
	 * MODULE/LEVEL:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MUNOUT
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *=====================================================================
	 * EXTERNAL DEPENDENCIES:
	 *===================================================================== */
	/* PROCEDURE: */
	*lend = FALSE;
	*lquit = FALSE;
L_5000:
        if(fgets(ktext,MCMSG+1,nunrpl) == NULL) goto L_8000;
        if(ktext[(numchar=strlen(ktext)-1)] == '\n')ktext[numchar] = '\0';

	/* - Loop back if it is a comment card or a blank line. */

	if( ktext[0] == kmgam.kopcmt || memcmp(ktext,"        ",8) == 0 )
		goto L_5000;

	/* - Check for "termination" op code. */

	if( ktext[0] == kmgam.kopqui ){
		*lquit = TRUE;
		*lend = TRUE;
		goto L_8888;
		}

	/* - Pop first token from line and convert to upper case. */

	nctext = indexb( ktext,MCMSG+1 );
	icpntr = 0;
	poptok( ktext, nctext, &icpntr, &icloc1, &icloc2, &itype );
	nctok = icloc2 - icloc1 + 1;

        strtemp1 = malloc(nctok+1);
        strtemp2 = malloc(nctok+1);
        strncpy(strtemp1,ktext+icloc1-1,nctok);
        strncpy(strtemp2,ktext+icloc1-1,nctok);
        strtemp1[nctok] = '\0';
        strtemp2[nctok] = '\0';

	upcase( strtemp1, nctok, strtemp2, icloc2 - icloc1 + 2 );
	subscpy( ktext, icloc1 - 1, icloc2 - 1, MCMSG, strtemp2 );

        free(strtemp1);
        free(strtemp2);

	/* - Store first two characters into local variables. */

	*kchar = ktext[icloc1 - 1];
	*kchar2 = ktext[icloc1];

	/* - If first character is the "change environment op", save
	 *   entire token in common and return. */

	if( *kchar == kmgam.kopbe ){
		fstrncpy( kmgam.kopetx, 80, ktext+icloc1 - 1, min(icloc2,MCMSG) - 
		 icloc1 + 1);
		goto L_8888;
		}

	/* - Pop next two tokens off line and convert to floating point
	 *   numbers.  These are the (x and y) locations of the current data point.
	 *   Scale the y location to the viewspace of the graphics device. */

	poptok( ktext, nctext, &icpntr, &icloc1, &icloc2, &itype );

        strncpy((strtemp1=malloc(icloc2-icloc1+2)),ktext+icloc1-1,
                            icloc2-icloc1+1);
        strtemp1[icloc2-icloc1+1] = '\0';

	cnvatf( strtemp1, icloc2 - icloc1 + 2, &cmgam.xcdp, 0, &nerr );
							/* add 0. maf 970129 */

        free(strtemp1);

	if( nerr != 0 ){
                strncpy((strtemp1=malloc(nctext+1)),ktext,nctext);
                strtemp1[nctext] = '\0';

                fprintf(MUNOUT," %s%s\n", kbdlin, strtemp1);

                free(strtemp1);
		goto L_5000;
		}
	poptok( ktext, nctext, &icpntr, &icloc1, &icloc2, &itype );

        strncpy((strtemp1=malloc(icloc2-icloc1+2)),ktext+icloc1-1,
                                         icloc2-icloc1+1);
        strtemp1[icloc2-icloc1+1] = '\0';

	cnvatf( strtemp1, icloc2 - icloc1 + 2, &cmgam.ycdp, 0, &nerr );
							/* add 0. maf 970129 */

        free(strtemp1);

	if( nerr != 0 ){
                strncpy((strtemp1=malloc(nctext+1)),ktext,nctext);
                strtemp1[nctext] = '\0';

                fprintf(MUNOUT," %s%s\n", kbdlin, strtemp1);

                free(strtemp1);
		goto L_5000;
		}
	goto L_8888;

	/* - Set end-of-file flag after entire file has been read.
	 *   Backspace over end-of-file before returning. */

L_8000:
	*lend = TRUE;
	backspace( nunrpl, 1L );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    840619:  Added check for "termination" op code.
	 *    810000:  Original version.
	 *===================================================================== */

} /* end of function */

