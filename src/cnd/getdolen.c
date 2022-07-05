#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
void /*FUNCTION*/ getdolen(nlines, nerr)
int *nlines, *nerr;
{
	char kiline[MCMSG+1], kname[9], ktemp1[MCPFN+1], ktemp2[MCPFN+1], ktoken[9];
	int _l0, i, i_, ic, ic1, ic2, itype, nc, niline, numdos, 
	 nchars;
        int numchar;
        FILE *nun;
        char *strtemp;
	void zgpmsg();


	/*=====================================================================
	 * PURPOSE: To get the length of a do clause.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nlines:  Number of lines in the do clause. [i]
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers: 2704, 2705
	 *=====================================================================
	 * MODULE/LEVEL:  cnd/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     getclun, poptok, modcase, setmsg, apcmsg
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    numdos:  Number of dos nested in the clause.
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
	numdos = 0;
	*nlines = 0;
        ic = 0;
        ic1 = 0;
        ic2 = 0;

	/* - Get the fortran file unit. */

	getclun( &nun, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Read next input line.  End-of-file or error terminates macro. */

L_1000:
	if( nun != MUNINP ){
                if(fgets(kiline,MCMSG,nun) == NULL) {
                  if(feof(nun)) goto L_9100;
                  goto L_9000;
		}
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
	*nlines = *nlines + 1;

	/* - Check for nested dos. */

	ic = 0;
	poptok( kiline, niline, &ic, &ic1, &ic2, &itype );
	nc = min( MCPW, ic2 - ic1 + 1 );

        strtemp = malloc(nc+1);
        strncpy(strtemp,kiline+ic1 - 1,nc);
        strtemp[nc] = '\0';
	modcase( TRUE, strtemp, nc, ktoken );
        free(strtemp);

	/* -- If "WHILE", increment number of nested dos  */
	if( memcmp(ktoken,"WHILE",5) == 0 )
		numdos = numdos + 1;
	if( memcmp(ktoken,"DO",2) == 0 )
		numdos = numdos + 1;
	if( memcmp(ktoken,"ENDDO",5) == 0 )
		numdos = numdos - 1;
	if( (memcmp(ktoken,"ENDDO",5) == 0) && (numdos < 0) )
		goto L_2000;

	goto L_1000;

	/* -- Backspace to Starting do. */

L_2000:
        backspace(nun,*nlines);

L_8888:
	return;

L_9000:
	*nerr = 2704;
	setmsg( "ERROR", *nerr );
	goto L_8888;

L_9100:
	*nerr = 2705;
	setmsg( "ERROR", *nerr );
	apcmsg( "\"enddo\"",8 );
	goto L_8888;

} /* end of function */

