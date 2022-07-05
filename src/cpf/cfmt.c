#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
void /*FUNCTION*/ cfmt(kmsg2, kmsg2_s)
char *kmsg2;   int kmsg2_s;
{
	char kmsg[MCMSG+1];
	int iarrow, ibeg = 0, iend_ = 0, j, nchar, ncmsg1, ncmsg2, ndiff;
	void *_p0;
	static byte kbell = 0x07;
	static char kmsg1[28] = "ERROR interpreting command:";
        char *cattemp;
        char *strtemp1, *strtemp2;


	/*=====================================================================
	 * PURPOSE: To format and send error message containing current command.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kmsg2:   Second line of error message with a "$" appended to it.
	 *             A pointer to the bad token will be added to this line.
	 *             The first line will contain a standard error message
	 *             with the current command appended to it.
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCMSG, MCPW, MUNOUT
	 *    com:     ncom, jcom, kcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     indexc, copykc, cerr
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    ibeg:    Character location for beginning of each token. [i]
	 *    iend:    Character location for end of each token. [i]
	 *    iarrow:  Character location for arrow pointed to bad token. [i]
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - jcom is pointing to the bad token.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820615:  Added call to CERR to set error condition.
	 *    820420:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Raise command syntax error condition. */
	cerr( 1001 );

        iarrow = 0;

	/* - Determine length of input message. */

	ncmsg1 = 27;
	ncmsg2 = indexc( kmsg2,kmsg2_s, '$' );

	/* - Initialize pointers. */

	ndiff = ncmsg2 - ncmsg1;
	if( ndiff > 0 ){
		ibeg = 3 + ndiff;
		}
	else{
		ibeg = 3;
		}

	/* - Prepare first line of output:
	 * -- Determine number of characters in each token.
	 * -- Put space delimited tokens in output line.
	 * -- Save location of bad token for use in generating second line. */

	subscpy( kmsg, 0, ibeg - 2, MCMSG, " " );
	j = 1;
L_1000:
	if( j <= cmcom.ncom ){
		if( strcmp(kmcom.kcom[j - 1],"'       ") == 0 || strcmp(kmcom.kcom[j - 1]
		 ,"\"       ") == 0 ){
			nchar = (int)( Flnum[j + 1] + 0.1 );
			iend_ = ibeg + nchar + 2;
			if( iend_ <= MCMSG ){
				kmsg[ibeg - 1] = kmcom.kcom[j - 1][0];
                                strtemp1 = malloc(MCMSG+1-(ibeg+1));
                                strncpy(strtemp1,kmsg+ibeg,MCMSG+1-(ibeg+1));
                                strtemp1[MCMSG+1-(ibeg+1)] = '\0';
				copykc( (char*)kmcom.kcom[j + 1],9, nchar, strtemp1);
                                free(strtemp1);
				kmsg[iend_ - 2] = kmcom.kcom[j - 1][0];
				kmsg[iend_ - 1] = ' ';
				if( j == cmcom.jcom )
					iarrow = ibeg - ndiff;
				ibeg = iend_ + 1;
				}
			j = j + (nchar - 1)/MCPW + 3;
			}
		else if( strcmp(kmcom.kcom[j - 1],"#       ") == 0 ){
			nchar = (int)( Flnum[j + 1] + 0.1 );
			iend_ = ibeg + nchar;
			if( iend_ <= MCMSG ){
                                strtemp1 = malloc(MCMSG+1-ibeg);
                                strncpy(strtemp1,kmsg+ibeg-1,MCMSG+1-ibeg);
                                strtemp1[MCMSG+1-ibeg] = '\0';
				copykc( (char*)kmcom.kcom[j + 1],9, nchar, strtemp1);
                                free(strtemp1);
				kmsg[iend_ - 1] = ' ';
				if( j == cmcom.jcom )
					iarrow = ibeg - ndiff;
				ibeg = iend_ + 1;
				}
			j = j + (nchar - 1)/MCPW + 3;
			}
		else{
			nchar = indexc( (char*)kmcom.kcom[j - 1],9, ' ' );
			iend_ = ibeg + nchar;
			if( iend_ <= MCMSG ){
                                cattemp = malloc(nchar+2);
                                strncpy(cattemp,kmcom.kcom[j - 1],nchar);
                                cattemp[nchar] = ' ';
                                cattemp[nchar+1] = '\0';
				subscpy( kmsg, ibeg - 1, iend_ - 1, MCMSG, cattemp);
                                free(cattemp);
				if( j == cmcom.jcom )
					iarrow = ibeg - ndiff;
				ibeg = iend_ + 1;
				}
			j = j + 1;
			}
		goto L_1000;
		}

	/* - Send first line to terminal. */

        strtemp1 = malloc(ncmsg1+1);
        strtemp2 = malloc(iend_+1);
        strncpy(strtemp1,kmsg1,ncmsg1);
        strtemp1[ncmsg1] = '\0';
        strncpy(strtemp2,kmsg,iend_);
        strtemp2[iend_] = '\0';

        fputc((int)kbell,MUNOUT);
        fprintf(MUNOUT," %s%s\n", strtemp1, strtemp2 );

        free(strtemp1);
        free(strtemp2);

	/* - Format and send second line to terminal. */

	if( iarrow > 0 ){
		subscpy( kmsg, 0, iarrow - 2, MCMSG, " " );
		kmsg[iarrow - 1] = '^';
                strtemp1 = malloc(ncmsg2+1);
                strtemp2 = malloc(iarrow+1);
                strncpy(strtemp1,kmsg2,ncmsg2);
                strtemp1[ncmsg2] = '\0';
                strncpy(strtemp2,kmsg,iarrow);
                strtemp2[iarrow] = '\0';
                fprintf(MUNOUT," %s%s\n", strtemp1, strtemp2 );
                free(strtemp1);
                free(strtemp2);
		}
	else{
		subscpy( kmsg, 0, iend_ - 3, MCMSG, " " );
		subscpy( kmsg, iend_ - 2, iend_ - 1, MCMSG, "->" );
                strtemp1 = malloc(ncmsg2+1);
                strtemp2 = malloc(iend_+1); 
                strncpy(strtemp1,kmsg2,ncmsg2);
                strtemp1[ncmsg2] = '\0';
                strncpy(strtemp2,kmsg,iend_);
                strtemp2[iend_] = '\0';
                fprintf(MUNOUT," %s%s\n", strtemp1, strtemp2 );
                free(strtemp1);
                free(strtemp2);
		}

L_8888:

	return;

} /* end of function */

