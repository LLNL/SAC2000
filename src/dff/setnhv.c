#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void setnhv_ (char* kname, int* nvalue, int* nerr, int kname_s);

void setnhv(char* kname, int* nvalue, int* nerr, int kname_s)
{
	char ktest[9];
	int index, ntest;

        /* the following line was added to help the function work the same whether
           it's called from C or FORTRAN.  From FORTRAN, don't pass in the last
           parameter, the compiler does it for you.  From C, don't count the
           terminator.  maf 970917 */
        kname_s++ ;



	/*=====================================================================
	 * PURPOSE: To set an integer header value in the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to set. [c]
	 *    NVALUE:  New value of header field. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1337 Header field does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     NHDR, MNHDR, NUNDEF
	 *    LHF:     KNHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, MODCASE, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in NHDR array of requested variable. [i]
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
	index = nequal( ktest, (char*)kmlhf.knhdr,9, MNHDR );

	/* - Store value in appropriate header field. */

	if( index > 0 ){
	    Nhdr[index] = *nvalue;
	}
	else{
	    *nerr = 1337;
	    Nhdr[index] = cmhdr.nundef;
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


void setnhv_ (char* kname, int* nvalue, int* nerr, int kname_s)
{
	setnhv ( kname , nvalue , nerr , kname_s ) ;
}
