#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ getfhv( kname, fvalue, nerr, kname_s )
char *kname;   int kname_s;
float *fvalue;
int *nerr;
{
	char ktest[9];
	int index, ntest;

	/* the following line was added to help the function work the same whether
	   it's called from C or FORTRAN.  From FORTRAN, don't pass in the last
	   parameter, the compiler does it for you.  From C, don't count the
	   terminator.  maf 970917 */
	kname_s++ ;


	/*=====================================================================
	 * PURPOSE: To get a floating point header value from the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to get. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    FVALUE:  Value of header field from current SAC data file. [f]
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1336 Header variable is undefined.
	 *             = 1337 Header variable does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     FHDR, MFHDR, FUNDEF
	 *    LHF:     KFHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, MODCASE, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in FHDR array of requested variable. [i]
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - A data file has been read by RSAC1 or RSAC2  OR
	 * - A header has been retrieved from working memory by GETFIL
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870918:  Added conversion of kname to uppercase.
	 *    870902:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870902
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Convert input name to uppercase and check versus list of legal names. */

	ntest = min( indexb( kname,kname_s ), MCPW );
	strcpy( ktest, "        " );
	modcase( TRUE, kname, ntest, ktest );
	index = nequal( ktest, (char*)kmlhf.kfhdr,9, MFHDR );

	/* - If legal name, return current value.
	 *   Otherwise, set error condition. */

	if( index > 0 ){
	    *fvalue = Fhdr[index];
	    if( *fvalue == cmhdr.fundef )
		*nerr = 1336;
	}
	else{
	    *nerr = 1337;
	    *fvalue = cmhdr.fundef;
	}

	/* - Create error message and write to terminal. */

	if( *nerr != 0 ){
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	    wrtmsg( MUNOUT );
	}

L_8888:
	return;

} /* end of function */



/* The following is a wrapper to make the code more convenient for 
   FORTRAN programmers.  */

void /*FUNCTION*/ getfhv_( kname, fvalue, nerr, kname_s )
char *kname;   int kname_s;
float *fvalue;
int *nerr;
{
	getfhv ( kname , fvalue , nerr , kname_s ) ;
}
