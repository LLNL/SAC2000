#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "com.h"
void /*FUNCTION*/ ctype(jdx)
int *jdx;
{
	char kfloat[MCMSG+1];
	int jc, jj, joffst, jw, nchar, nerr, nzerr;

	kfloat[0] = '\0' ;

	/*=====================================================================
	 * PURPOSE:  To see if jth token in current command is numeric or not.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    J:       The pointer to the token to be typed.
	 *=====================================================================
	 * MODULE/LEVEL: COM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPW
	 *    COM:     KCOM, IALPHA, INUMBR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    COM:     KCOM, ITYPCM, FLNUM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  CNVATF
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Handle quoted string case. */
	if( strcmp(kmcom.kcom[*jdx - 1],"'       ") == 0 ){

	    /* -- Convert character count to integer. */
	    cnvatf( (char*)kmcom.kcom[*jdx],9, &Flnum[*jdx + 1], TRUE, &nerr );
	    nchar = (int)( Flnum[*jdx + 1] + 0.1 );

	    /* -- Try to convert character string to floating point. */
	    jc = 1;
	    for( jw = *jdx + 1; jw < (*jdx + 2 + (nchar - 1)/MCPW); jw++ ){
		subscpy( kfloat, jc - 1, -1, MCMSG, kmcom.kcom[jw] );
		jc = jc + MCPW;
	    }

            if (non_num_com(kmcom.kcom[0],9)){
		nzerr = 1;
            }
	    else {
		cnvatf( kfloat,MCMSG+1, &Flnum[*jdx], TRUE, &nzerr );
	    }

            savearg(*jdx,kfloat,nchar);

	    /* -- If can't be converted, then token is alphanumeric. */
	    if( nzerr != 0 ){
		Itypcm[*jdx] = cmcom.ialpha;
		joffst = (nchar - 1)/MCPW + 2;
		*jdx = *jdx + joffst;
	    }
	    /* -- If numeric adjust command so number only takes one token. */
	    else{
		Itypcm[*jdx] = cmcom.inumbr;
                fstrncpy(kmcom.kcom[*jdx - 1],8,kmcom.kcom[*jdx + 1],MCPW-1);
                kmcom.kcom[*jdx-1][MCPW-1] = '$';
		joffst = (nchar - 1)/MCPW + 2;
		cmcom.ncom = cmcom.ncom - joffst;
		for( jj = *jdx ; jj < cmcom.ncom; jj++ ){
			strcpy( kmcom.kcom[jj], kmcom.kcom[jj + joffst] );
		}
	    }
	}
	/* - Handle case where token is less than or equal to MCPW characters.*/
	else{
            savearg(*jdx,kmcom.kcom[*jdx-1],MCPW);

	    cnvatf( (char*)kmcom.kcom[*jdx - 1],9, &Flnum[*jdx], TRUE, &nzerr );
	    if( nzerr != 0 ){
		Itypcm[*jdx] = cmcom.ialpha;
	    }
	    else{
		Itypcm[*jdx] = cmcom.inumbr;
	    }
	}

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    820426:  Original version (from GETCMD).
	 *=====================================================================
	 * DOCUMENTED:  820426
	 *===================================================================== */

} /* end of function */

