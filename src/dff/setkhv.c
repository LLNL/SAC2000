#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ setkhv(kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;  int kvalue_s;
int *nerr;
{
	char ktest[9];
	int index, ntest;

        /* the following lines were added to help the function work the same whether
           it's called from C or FORTRAN.  From FORTRAN, don't pass in the last
           two parameters, the compiler does it for you.  From C, don't count the
           terminator.  maf 970917 */
        kname_s++ ;
	kvalue_s++;


	/*=====================================================================
	 * PURPOSE: To set an alphanumeric header value in the current SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field to set. [c]
	 *    KVALUE:  New value of header field. [c]
	 *             KEVNM is 16 characters int.  All others are 8.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             = 1337 Header field does not exist.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     KHDR, MKHDR, FUNDEF
	 *    LHF:     KKHDR
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB, MODCASE, NEQUAL, SETMSG, APCMSG, WRTMSG
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCNAME:  Number of characters in KNAME. [i]
	 *    KTEST:   Local storage for KNAME [k]
	 *    INDEX:   Index in KHDR array of requested variable. [i]
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
	index = nequal( ktest, (char*)kmlhf.kkhdr,9, MKHDR );

	/* - Store value in appropriate header field. */

	if( index > 0 ){
	    fstrncpy( kmhdr.khdr[index - 1], 8, kvalue, strlen(kvalue));
	    if( index == 2 )
		fstrncpy( kmhdr.khdr[2], 8, kvalue+8, kvalue_s - 9);
	}
	else{
	    *nerr = 1337;
	    strcpy( kmhdr.khdr[index - 1], kmhdr.kundef );
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




/* Wrapper to make the function for convenient for FORTRAN programmers. */

void /*FUNCTION*/ setkhv_ (kname, kvalue, nerr, kname_s, kvalue_s)
char *kname;   int kname_s;
char *kvalue;  int kvalue_s;
int *nerr;
{
	setkhv ( kname , kvalue , nerr , kname_s , kvalue_s ) ;
}
