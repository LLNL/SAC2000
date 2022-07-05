#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ setihv(kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;  int kvalue_s;
int *nerr;
{
	char ktest[9];
	int index, ivalue, ntest;

        /* the following lines were added to help the function work the same whether
           it's called from C or FORTRAN.  From FORTRAN, don't pass in the last
           two parameters, the compiler does it for you.  From C, don't count the
           terminator.  maf 970917 */
        kname_s++ ;
	kvalue_s++;


	/*=====================================================================
	 * PURPOSE: To set an emumerated header value in the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to set. [c]
	 *    KVALUE:  New value of header field. [c]
	 *             Each value represents a specific condition.
	 *             See table in Users Manual for allowed values.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1337 Header variable does not exist.
	 *             = 1365 Illegal enumerated value.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     IHDR, MIHDR, IUNDEF
	 *    LHF:     KIHDR, KIV, MIV
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXC, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in IHDR array of requested variable. [i]
	 *    IVALUE:  Equivalent enumerated integer value for header field. [i]
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - The data file will be written by WSAC  OR
	 * - The header will be stored in working memory by PUTFIL.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870918:  Changed from integer to character input for value field.
	 *    870902:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870902
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Convert input value to uppercase and check versus list of allowed values. */

	ntest = min( indexb( kvalue,kvalue_s ), MCPW );
	strcpy( ktest, "        " );
	modcase( TRUE, kvalue, ntest, ktest );
	ivalue = nequal( ktest, (char*)kmlhf.kiv,9, MIV );

	/* - If not a match, set and report error condition. */

	if( ivalue <= 0 ){
	    *nerr = 1365;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kvalue,kvalue_s );
	    wrtmsg( MUNOUT );
	    ivalue = cmhdr.iundef;
	}

	/* - Convert input name to uppercase and check versus list of legal names. */

	ntest = min( indexb( kname,kname_s ), MCPW );
	strcpy( ktest, "        " );
	modcase( TRUE, kname, ntest, ktest );
	index = nequal( ktest, (char*)kmlhf.kihdr,9, MIHDR );

	/* - If legal header name, store value in appropriate header field.
	 *   Otherwise, set and report error condition. */

	if( index > 0 ){
	    Ihdr[index] = ivalue;
	}
	else{
	    *nerr = 1337;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	    wrtmsg( MUNOUT );
	}

L_8888:
	return;

} /* end of function */




/* Wrapper to make the function more convenient for FORTRAN programmers. */

void /*FUNCTION*/ setihv_ (kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;  int kvalue_s;
int *nerr;
{
	setihv ( kname , kvalue , nerr , kname_s , kvalue_s ) ;
}
