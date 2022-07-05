#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ zquit()
{
	int nerr;

	/*======================================================================
	 * PURPOSE: To terminate SAC program.
	 *======================================================================
	 * MODULE/LEVEL:  co/5
	 *======================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *======================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  endgraphics, capf, chpf, zdestf, xyzcleanup
	 *======================================================================
	 * SPECIAL NOTE: There is no longer any system dependent coding
	 * in this subroutine.  There was in previous versions.  It is 
	 * kept in this subdirectory for both historical and slothful reasons.
	 *======================================================================
	 * MODIFICATON HISTORY:
	 *    900310:  Added call to utahcleanup.
	 *    891002:  Deleted calls to zsysop and zintof that were
	 *             no longer doing anything.
	 *    880520:  Fixed bug in call to engraphics.
	 *    860930:  Deleted call to zendfr.
	 *    840822:  Retyped on B4.2 UNIX VAX--D. Trimmer
	 *    830818:  Cleanup and documented.
	 *    820823:  Delete LSAVE input argument.
	 *    811112:  Added a call to ZENDFR to end current graphics frame.
	 *======================================================================
	 * DOCUMENTED/REVIEWED:  891002
	 *====================================================================== */
	/* PROCEDURE: */
	/* - Close and dispose of any output files. */
	/* -- Plot files. */
	endgraphics( &nerr );
	if( nerr != 0 )
		goto L_8888;

	/* -- Pick files. */
	capf( &nerr );
	if( nerr != 0 )
		goto L_8888;
	chpf( &nerr );
	if( nerr != 0 )
		goto L_8888;

	/* - Destroy any scratch files created during execution. */

	zdestf( "ZDFL ",6, &nerr );
	if( nerr != 0 )
		goto L_8888;

	xyzcleanup();

	/* - Terminate program. */

	exit(0);

L_8888:
	return;

} /* end of function */

