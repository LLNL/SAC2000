#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ wrcom()
{
	char kmsg[MCMSG+1], ktemp[MCMSG+1];
	int ibeg, iend_, ispace, j, nchar;
        char *s1;

	/*=====================================================================
	 * PURPOSE:  To write the current command to the terminal.
	 *=====================================================================
	 * MODULE/LEVEL:  service/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCMSG, MUNOUT
	 *    COM:     NCOM, KCOM, ITYPCM, FLNUM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  COPYKC, INDEXB
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    IBEG:    Counter in KMSG array. [i]
	 *    IEND:    Counter in KMSG array. [i]
	 *    J:       Counter in command array. [i]
	 *    KTEMP:   Character string used for scratch space. [c]
	 *=====================================================================
	 * SPECIAL NOTE:  Logic in this subroutine should be similiar to that
	 *                in "cpf" function "lcrest." 
	 *                If either is changed, then the other should be updated.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871022:  Cleaned up output of strings and int tokens.
	 *    870513:  Deleted call to wrtxtd.  Writing to terminal directly.
	 *    870218:  Fixed bug in formatting quoted strings.
	 *    840206:  Improved method of formatting current command.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871022
	 *===================================================================== */
	/* PROCEDURE: */
	ibeg = 1;
	j = 1;

	/* - Loop through each token in the current command. */

L_1000:
	if( j <= cmcom.ncom ){

		/* -- Quoted text string.  There are two cases:
		 *    (1) If there are spaces in the string, include the quotes.
		 *    (2) If no spaces, this was an "internally" produced string. Exclude quotes. */
		if( strcmp(kmcom.kcom[j - 1],"'       ") == 0 || strcmp(kmcom.kcom[j - 1]
		 ,"\"       ") == 0 ){
			nchar = (int)( Flnum[j + 1] + 0.1 );
			fstrncpy( ktemp, MCMSG, " ", 1 );
			copykc( (char*)kmcom.kcom[j + 1],9, nchar, ktemp );
			ispace = indexa( ktemp,MCMSG+1, ' ', TRUE, TRUE );
			if( ispace < nchar ){
				kmsg[ibeg - 1] = kmcom.kcom[j - 1][0];
				ibeg = ibeg + 1;
				}
			iend_ = ibeg + nchar - 1;
			if( iend_ > MCMSG )
				iend_ = iend_ - 1;
			subscpy( kmsg, ibeg - 1, iend_ - 1, MCMSG, ktemp );
			ibeg = iend_ + 1;
			if( ispace < nchar ){
				kmsg[ibeg - 1] = kmcom.kcom[j - 1][0];
				ibeg = ibeg + 1;
				}
			kmsg[ibeg - 1] = ' ';
			ibeg = ibeg + 1;
			j = j + (nchar - 1)/MCPW + 3;

			/* -- Number that was too large for token.
			 *    Encode the number itself into the temporary string and copy to message. */
			}
		else if( Itypcm[j] == cmcom.inumbr && kmcom.kcom[j - 1][MCPW - 
		 1] == '$' ){
                        sprintf(ktemp,"%16.5g", Flnum[j] );
			ljust( ktemp,MCMSG+1 );
			nchar = indexb( ktemp,MCMSG+1 );
			iend_ = min( ibeg + nchar, MCMSG );
                        strncpy((s1=malloc(nchar+1)),ktemp,nchar);
                        s1[nchar] = '\0';
			subscpy( kmsg, ibeg - 1, iend_ - 2, MCMSG, s1);
                        free(s1);
			kmsg[iend_ - 1] = ' ';
			ibeg = iend_ + 1;
			j = j + 1;

			/* -- Simple token.  Copy to message. */
			}
		else{
			nchar = indexc( (char*)kmcom.kcom[j - 1],9, ' ' );
			iend_ = min( ibeg + nchar, MCMSG );
                        strncpy((s1=malloc(nchar+1)),kmcom.kcom[j - 1],nchar);
                        s1[nchar] = '\0';
			subscpy( kmsg, ibeg - 1, iend_ - 2, MCMSG, s1 );
                        free(s1);
			kmsg[iend_ - 1] = ' ';
			ibeg = iend_ + 1;
			j = j + 1;
			}

		/* -- Loop until all tokens have been processed. */
		goto L_1000;
		}

	/* - Write message to standard output. */
     
        strncpy((s1=malloc(ibeg)),kmsg,ibeg-1);
        s1[ibeg-1] = '\0';
        fprintf(MUNOUT," %s\n",s1);
        free(s1);

L_8888:
	return;

} /* end of function */

