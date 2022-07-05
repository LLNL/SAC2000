#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ zruntext(text, text_s, nfun, nerr)
char *text;   int text_s;
FILE *nfun;
int nerr;
{
	int nctext;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE:  To add a line of text to runfile.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     text:    Line of text. [c]
	 *     nfun:    Fortran file unit that runfile is open on. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *     nerr:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871014:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  871014
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Write line to the file. */
	nctext = max( 1, indexb( text,text_s ) );

        strtemp = malloc(nctext+1);
        strncpy(strtemp,text,nctext);
        strtemp[nctext] = '\0';

        fprintf(nfun,"%s\n",strtemp);
  
        free(strtemp);
L_8888:
	return;

} /* end of function */

