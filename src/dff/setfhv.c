#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ setfhv(kname, fvalue, nerr, kname_s)
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
	 * PURPOSE: To set a floating point header value in the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to set. [c]
	 *    FVALUE:  New value of header field. [f]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1337 Header field does not exist.
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
	 * - The data file will be written by WSAC  OR
	 * - The header will be stored in working memory by PUTFIL.
	 *=====================================================================
	 * MODIFICATION HISTORY:
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

	/* - Store value in appropriate header field. */

	if( index > 0 ){
	    Fhdr[index] = *fvalue;
	}
	else{
	    *nerr = 1337;
	    Fhdr[index] = cmhdr.fundef;
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



/* Wrapper to make the function more convenient for FORTRAN programmers. */

void /*FUNCTION*/ setfhv_ (kname, fvalue, nerr, kname_s)
char *kname;   int kname_s;
float *fvalue;
int *nerr;
{
	setfhv ( kname , fvalue , nerr , kname_s ) ;
}
