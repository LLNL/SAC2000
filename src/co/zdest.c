#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ zdest(kname, kname_s, nerr)
char *kname;   int kname_s;
int *nerr;
{
	int lexist;
	int nfu, nlen, nc;

	/*=====================================================================
	 * PURPOSE:  To destroy a disk file if it exists.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of disk file to be destroyed. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error.
	 *             Possible error numbers from this call:
	 *             0108:  File does not exist.
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871222:  Moved file inquire to zinquire.
	 *    850108:  Call ZEXPND to expand file path name--D. Trimmer
	 *    830812:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Check existence of file. */

	zinquire( kname, &lexist );

	/* - If file exists: */

	if( lexist ){
            nc = indexb(kname,kname_s);
            kname[nc] = '\0';
            *nerr = unlink(kname);
	}
	else{
	    *nerr = 108;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	}

L_8888:
	return;

} /* end of function */

