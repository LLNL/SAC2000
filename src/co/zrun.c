#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ zrun(nfun, runfile, runfile_s, nerr)
FILE **nfun;
char *runfile;   int runfile_s;
int *nerr;
{
	char runtext[81];
	int nc;
	void zsysop();

	/*=====================================================================
	 * PURPOSE:  To execute the runfile produced by zrunname and zruntext.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     nfun:    Fortran file unit that runfile is open on. [i]
	 *     runfile: Name of runfile. [c]
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
	/* - Write last line to the runfile. */
        fprintf(*nfun,"%s\n","endrun");

	/* - Close runfile. */

	zcloses( nfun, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Execute the runfile as a shell script using "zsysop". */

	subscpy( runtext, 0, 2, 80, "sh " );
	subscpy( runtext, 3, -1, 80, runfile );
	nc = indexb( runtext,81 );
	zsysop( runtext,81, &nc, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Destroy the runfile. */

	zdest( runfile,runfile_s, nerr );

L_8888:
	return;

} /* end of function */

