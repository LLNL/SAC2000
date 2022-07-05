#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ getihv(kname, kvalue, nerr, kname_s, kvalue_s)
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
	/* kvalue_s++; */


	/*=====================================================================
	 * PURPOSE: To get an emumerated header value from the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to get. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KVALUE:  Value of header field from current SAC data file. [c]
	 *             Each value represents a specific condition.
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1336 Header variable is undefined.
	 *             = 1337 Header variable does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     IHDR, MIHDR, IUNDEF
	 *    LHF:     KIHDR, KIV
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in IHDR array of requested variable. [i]
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - A data file has been read by RSAC1 or RSAC2  OR
	 * - A header has been retrieved from working memory by GETFIL
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
	index = nequal( ktest, (char*)kmlhf.kihdr,9, MIHDR );

	/* - If legal name, return current value.
	 *   Otherwise, set error condition. */

	if( index > 0 ){
	    ivalue = Ihdr[index];
	    if( ivalue == cmhdr.iundef ){
    	        fstrncpy( kvalue, kvalue_s-1, "UNDEFINED", 9);
		*nerr = 1336;
	    }
	    else{
    	        fstrncpy( kvalue, kvalue_s-1, kmlhf.kiv[ivalue - 1],
                          strlen(kmlhf.kiv[ivalue - 1]) );
	    }
	}
	else{
    	    fstrncpy( kvalue, kvalue_s-1, "ILLEGAL", 7);
	    *nerr = 1337;
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

void /*FUNCTION*/ getihv_ (kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;  int kvalue_s;
int *nerr;
{
	getihv ( kname , kvalue , nerr , kname_s , kvalue_s ) ;
}
