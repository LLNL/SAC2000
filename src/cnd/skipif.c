#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ skipif(nerr)
int *nerr;
{
	char kiline[MCMSG+1], kname[9], ktemp1[MCPFN+1], ktemp2[MCPFN+1], ktoken[9];
	int _l0, ic, ic1, ic2, itype, nc, niline, numifs;
        FILE *nun;
        int nchars;
        int numchar;
        char *strtemp;
	void zgpmsg();


	/*=====================================================================
	 * PURPOSE: To skip over a clause of an if statement.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  CND/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  getclun, getvvstring, zgpmsg, poptok, modcase
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    numifs:  Number of ifs nested in the clause.
	 *    nun:     Fortran file unit used in opening command file. [i]
	 *    kiline:  Input (raw) line from command file. [c]
	 *    niline:  Length of kiline without trailing blanks. [i]
	 *    ktemp1:  Used to store prompt sent to terminal when in
	 *             interactive mode. Also used when creating prompt. [c]
	 *    ktemp2:  Used when creating prompt.  Return from call to getdir
	 *             is the directory and filename parts of a pathname. [c]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	numifs = 0;

	/* - Get the fortran file unit. */

	getclun( &nun, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Read next input line.  End-of-file or error terminates macro. */

L_1000:
	if( nun != MUNINP ){
                if(fgets(kiline,MCMSG,nun) == NULL) goto L_9000;
                if(kiline[(numchar=strlen(kiline)-1)] == '\n')kiline[numchar] = '\0';
		}
	else{
		getvvstring( kname,9, "prompt",7, &nchars, ktemp1,MCPFN+1, 
		 nerr );
		if( *nerr != 0 )
			goto L_8888;
		zgpmsg( ktemp1,MCPFN+1, kiline,MCMSG+1 );
		}
	niline = indexb( kiline,MCMSG+1 );

	/* - Check for nested ifs. */

	ic = 0;
	poptok( kiline, niline, &ic, &ic1, &ic2, &itype );
	nc = min( MCPW, ic2 - ic1 + 1 );

        strtemp = malloc(nc+1);
        strncpy(strtemp,kiline+ic1 - 1,nc);
        strtemp[nc] = '\0';

	modcase( TRUE, strtemp, nc, ktoken );

        free(strtemp);

	/* -- If "IF", increment number of nested ifs  */
	if( memcmp(ktoken,"IF",2) == 0 )
		numifs = numifs + 1;
	if( memcmp(ktoken,"ENDIF",5) == 0 )
		numifs = numifs - 1;
	if( ((memcmp(ktoken,"ENDIF",5) == 0) && (numifs < 0)) || 
	 (((memcmp(ktoken,"ELSE",4) == 0) || (memcmp(ktoken
	 ,"ELSEIF",6) == 0)) && (numifs < 1)) )
		goto L_8000;

	goto L_1000;

L_8000:
	backspace( nun, 1L );

L_8888:
	return;


L_9000:
	*nerr = 1;
	goto L_8888;

} /* end of function */

