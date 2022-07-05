#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ xhelp( lprint , nerr )
int lprint ;
int *nerr;
{
	char ktoken[30], *cptr;
        static char khflin[MCMSG+1];
        static int nhflin;
	int lintro, nchar, idx;
	static char kintro[9] = "HLPINTRO";

	/*=====================================================================
	 * PURPOSE: To parse the action command HELP.
	 *          This command prints files from online help package.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  EXM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCTOK, ICTOK, WRHELP
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	lintro = TRUE;

	/* - Loop on each token in command: */

        if (lcmore(nerr)){
	    if( lcdfl ( khflin , MCMSG + 1 , &nhflin ) ) {
		lintro = FALSE;
		cptr = strchr(khflin,(int)' ')+1;

		for(idx=0; idx<nhflin; idx++){
		    /* lcdfl returns a blank in the first position of the file list */
		    strncpy(ktoken,cptr,(nchar=strchr(cptr,(int)' ')-cptr));
		    ktoken[nchar] = '\0';
		    modcase ( FALSE , ktoken , strlen ( ktoken ) , ktoken ) ;
		    wrhelp(ktoken,nchar+1,1, lprint , nerr);
		    if(*nerr != 0){
			if( *nerr < 0 ) *nerr = 0;
			goto L_8888;
		    }
		    cptr = strchr(cptr,(int)' ')+1;
		} /* end for */
	    }
	    else{
		cfmt("ILLEGAL OPTION:$",17);
		cresp();
	    }
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
	    goto L_8888;

	/* - If there were no tokens in command, print the
	 *   introductory help package. */

	if( lintro )
	    wrhelp( kintro,9, 1, lprint , nerr );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820823:  Factored from original larger subroutine.
	 *===================================================================== */

} /* end of function */

