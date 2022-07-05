#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#define	MRECLB	512

#include "../../inc/mach.h"
void /*FUNCTION*/ znfile(nfu, kname, kname_s, ktype, ktype_s, nerr)
int *nfu;
char *kname;   int kname_s;
char *ktype;   int ktype_s;
int *nerr;
{
	int lnewfl, lro;
	int noerr;
	void zopenc();

	/*=====================================================================
	 * PURPOSE: To open an existing disk file or create a new one.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NFU:     Fortran file unit to open file on. [i]
	 *    KNAME:   Name of disk file to open. [c]
	 *    KTYPE:   Type of disk file to open [k]:
	 *             = 'DATA' for unformatted, direct-access file.
	 *             = 'TEXT' for formatted sequential-access file.
	 *    NLEN:    Word length of file. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error.
	 *             Possible error numbers from this call:
	 *             = 0101:  System error during open or create.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/5
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPFN
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    OFM:     KOUTM, MINTL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    MRECLB:  Length of a fixed record in bytes for "DATA" type files.
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - Record size of MRECLB bytes same in ZOPEN, ZNFILE, ZRABS, and ZWABS.
	 *=====================================================================
	 * KNOWN BUGS:
	 * - 'READ' option cannot be implemented using standard Fortran 77.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871222:  Changed file inquire to zinquire.
	 *             Included MASSCOMP differences.
	 *    870526:  Merged BSD4.2 and SYSTEM V versions.
	 *    860819:  Changed to new message system.
	 *    851216:  Modified for UNIX file I/O--D. Trimmer
	 *    850108:  Call ZEXPND to expand file path name.--D. Trimmer
	 *    840117:  Now using ZDEST to destroy existing files.
	 *    830812:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870526
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Open data file. */

	if( memcmp(ktype,"DATA",4) == 0 ){
	    lnewfl = TRUE;
	    lro = FALSE;
	    zopenc( nfu, kname, &lnewfl, &lro, &noerr, kname_s );
	    if( noerr != 0 ){
		*nerr = 101;
		setmsg( "ERROR", *nerr );
		apcmsg( kname,kname_s );
		if( noerr == 1 )
		    apcmsg( "(Insufficient access rights.)",30 );
		else
		    apcmsg( "(System error occurred.)",25 );
		goto L_8888;
	    }
	}

	/* - Open text file. */
	else{
	    *nerr = 101;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	    aplmsg( "Bad value for file type = ",27 );
	    apcmsg( ktype,ktype_s );
	    goto L_8888;
	}

L_8888:
	return;

} /* end of function */

