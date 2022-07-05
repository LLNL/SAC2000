#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcrest(mchar, kchar, kchar_s, nchar)
int mchar;
char *kchar;   int kchar_s;
int *nchar;
{
	char ktemp[MCMSG+1];
	int lcrest_v;
	int ibeg, iend_, ispace, j;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE:  To parse the "rest" of the command line.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcrest:  .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    mchar:   Maxiumum length of KCHAR. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS: 
	 *    kchar:   Output character string. [c]
	 *    nchar:   Length of KCHAR. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    com:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     copykc, indexb
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    ibeg:    Counter in KCHAR array. [i]
	 *    iend:    Counter in KCHAR array. [i]
	 *    j:       Counter in command array. [i]
	 *    ktemp:   Character string used for scratch space. [c]
	 *=====================================================================
	 * SPECIAL NOTE:  
	 *   Logic in this function should be similiar to that in "ucf/wrcom.f"
	 *   If either is changed, then the other should be updated.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871022:  Gutted routine and replaced with logic from WRCOM.
	 *    870211:  Major change in logic.
	 *    820825:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871022
	 *===================================================================== */
	/* PROCEDURE: */
	ibeg = 1;
	j = cmcom.jcom;
        memset(kchar,(int)' ',kchar_s-1);
        kchar[kchar_s-1] = '\0';

	/* - Loop through remaining tokens in the current command. */

L_1000:
	if( j <= cmcom.ncom ){

		/* -- Quoted text string.  There are two cases:
		 *    (1) If there are spaces in the string, include the quotes.
		 *    (2) If no spaces, this was an "internally" produced string. Exclude quotes. */
		if( strcmp(kmcom.kcom[j - 1],"'       ") == 0 || strcmp(kmcom.kcom[j - 1]
		 ,"\"       ") == 0 ){
			*nchar = (int)( Flnum[j + 1] + 0.1 );

                        memset(ktemp,(int)' ',MCMSG);
                        ktemp[MCMSG] = '\0';
			copykc( (char*)kmcom.kcom[j + 1],9, *nchar, ktemp );
			ispace = indexa( ktemp,MCMSG+1, ' ', TRUE, TRUE );
			if( ispace < *nchar ){
				kchar[ibeg - 1] = kmcom.kcom[j - 1][0];
				ibeg = ibeg + 1;
				}
			iend_ = ibeg + *nchar - 1;
			if( iend_ > mchar )
				iend_ = iend_ - 1;
			subscpy( kchar, ibeg - 1, iend_ - 1, kchar_s - 1, ktemp
			  );
			ibeg = iend_ + 1;
			if( ispace < *nchar ){
				kchar[ibeg - 1] = kmcom.kcom[j - 1][0];
				ibeg = ibeg + 1;
				}
			kchar[ibeg - 1] = ' ';
			ibeg = ibeg + 1;
			j = j + (*nchar - 1)/MCPW + 3;

			/* -- Number that was too large for token.
			 *    Encode the number itself into the temporary string and copy to message. */
			}
		else if( Itypcm[j] == cmcom.inumbr && kmcom.kcom[j - 1][MCPW - 
		 1] == '$' ){
                        sprintf(ktemp,"%16.5g", Flnum[j]);
			ljust( ktemp,MCMSG+1 );
			*nchar = indexc( ktemp,MCMSG+1, ' ' );
			iend_ = min( ibeg + *nchar, mchar );
                        strtemp = malloc(*nchar + 1);
                        strncpy(strtemp,ktemp,*nchar);
                        strtemp[*nchar] = '\0';
			subscpy( kchar, ibeg - 1, iend_ - 2, kchar_s - 1, strtemp);
                        free(strtemp);
			kchar[iend_ - 1] = ' ';
			ibeg = iend_ + 1;
			j = j + 1;

			/* -- Simple token.  Copy to message. */
			}
		else{
			*nchar = indexc( (char*)kmcom.kcom[j - 1],9, ' ' );
			iend_ = min( ibeg + *nchar, mchar );
                        strtemp = malloc(*nchar +1);
                        strncpy(strtemp,kmcom.kcom[j - 1],*nchar);
                        strtemp[*nchar] = '\0';
			subscpy( kchar, ibeg - 1, iend_ - 2, kchar_s - 1, strtemp);
                        free(strtemp);
			kchar[iend_ - 1] = ' ';
			ibeg = iend_ + 1;
			j = j + 1;
			}

		/* -- Loop until all tokens have been processed. */
		goto L_1000;
		}

	*nchar = ibeg - 1;
	lcrest_v = *nchar > 0;

L_8888:
	return( lcrest_v );

} /* end of function */

