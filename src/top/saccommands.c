#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/exm.h"
#include "../../inc/dfm.h"	/* to post SACNFILES, maf 961010 */

void zsysop(char* comstr,int dummylen,int* pnumc,int* perr);


void /*FUNCTION*/ saccommands(kinmsg, kinmsg_s, nerr)
char *kinmsg;   int kinmsg_s;
int *nerr;
{
	char kcommand[9] = "        " ;
	int lexist, lfound;
	int index, module, notused, nchar;
        int memerr;
        char *temp;
	char kNumberOfFiles[5] ;  /* gets cmdfm.ndfl in ascii to post SACNFILES
					maf 961010 */
       
        int malloc_verify();

	/*=====================================================================
	 * PURPOSE: To execute one or more SAC commands in a message.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kinmsg:  Message containing SAC commands. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error return flag. Set to 0 if no error occurred. [i]
	 *             A positive number indicates an error.
	 *=====================================================================
	 * MODULE/LEVEL:   top/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    exm:     lecho, ntraces
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     pcmsg, gc, reperr, proerr, lcchar, wrcom, 
	 *             findcommand, executecommand, tracereport, setmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    961010:  Posts number of files to the black board after each
	 *             command. maf
	 *    910624:  Changed error handling logic around after executecommand
	 *             to get the system command error to print (wct).
	 *    891005:  Deleted "itpmsg" argument and changed "nerr" argument.
	 *    890110:  Added call to "tracereport".
	 *    870722:  Modified ISTAT output to include execution error reporting.
	 *    860206:  Added call to REPERR to report current error status.
	 *    820901:  Major modification due to new command parsing logic.
	 *    811125:  Added call to ZSHSG.
	 *    810429:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	kcommand[0] = '\0' ; /* fix an access violation, maf 980507 */


	if ( SeisMgrCode ( kinmsg , nerr ) ) 
	    goto L_8888 ;

        memerr = -1;
	/* - Load message into command stack. */

#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
	pcmsg( kinmsg,kinmsg_s, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Get next command off command stack. */

L_1000:

#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
	gc( &lexist, nerr );
	if( *nerr != 0 ){
		reperr( *nerr );
		if( *nerr != 0 )
			proerr( nerr );
		goto L_1000;
		}
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

	/* - If a command exists in the command stack: */

	if( lexist ){
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

		/* -- Get command name and convert to uppercase. */
		lcchar( MCPW, kcommand,9, &notused );
		modcase( TRUE, kcommand, MCPW, kcommand );
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

		/* -- Echo command if requested. */
		if( cmexm.lecho && strcmp(kcommand,"ECHO    ") != 0 )
			wrcom();
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

		/* -- Validate command name and find module and index number. */
		findcommand( kcommand, &lfound, &module, &index );
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

		/* -- If valid, execute command.
		 *    Report any errors. */
		if( lfound ){
			executecommand( module, index, nerr );
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
			reperr( *nerr );
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
			if( *nerr != 0 ){
				setmsg( "ERROR", *nerr );
				proerr( nerr );
				}
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
			if( cmexm.ntraces > 0 )
				tracereport( nerr );
			}
		else{
                        nchar = indexb(kinmsg,kinmsg_s);
                        if(nchar > 0){
                          temp = kinmsg;
                          while ( (*temp == ' ') || (*temp == '\t') ) temp++;
                   /* make sure that the first char is not something like *, and */
                   /* disable the dangerous rm command.                          */
                          if ( isalpha ((int)*temp) || (*temp == '/')){
                               if(strncmp(temp,"rm ",3) != 0){
                                  zsysop(kinmsg,(int)0,&nchar,nerr);
			       }else{
                                  *nerr = 1106;
			       }
			  }
			}
                        if(*nerr != 0 ) {
			  *nerr = 1106;
			  setmsg( "ERROR", *nerr );
			  reperr( *nerr );
			  if( *nerr != 0 )
				proerr( nerr );
		        }
			goto L_8888;
			}

		/* -- Loop until command stack is empty. */
		goto L_1000;

		}

L_8888:
	/* Post the number of files to the black board before returning. maf 961010 */
	sprintf ( kNumberOfFiles , "%d", cmdfm.ndfl ) ;
	setbbv ( "SACNFILES" , kNumberOfFiles , nerr , 9 , strlen ( kNumberOfFiles ) ) ;

	return;

} /* end of function */

