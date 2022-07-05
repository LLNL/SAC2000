#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#define	MRECLB	512

#include "mach.h"
void /*FUNCTION*/ zopen_sac(nfu, kname, kname_s, ktype, ktype_s, nerr)
int *nfu;
char *kname;   int kname_s;
char *ktype;   int ktype_s;
int *nerr;
{
	int lexist, lnewfl, lro;
	int noerr;
	void zopenc();

	/*=====================================================================
	 * PURPOSE: To open an existing disk file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NFU:     Fortran file unit to open file on. [i]
	 *    KNAME:   Name of disk file to open. [c]
	 *    KTYPE:   Type of disk file to open [k]:
	 *             = 'DATA' for unformatted, direct-access file.
	 *             = 'RODATA' for read-only DATA file.
	 *             = 'ROUNFR' for read-only FORTRAN unformatted file.
	 *             = 'TEXT' for formatted sequential-access file.
	 *             = 'ROTEXT' or 'READ' for read-only TEXT file.
	 *             = 'SCRATCH' for temporary file 'tmp...' that gets
	 *                         deleted when the file is closed.
	 *    NLEN:    Word length of file. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error.
	 *             Possible error numbers from this call:
	 *             108 - File does not exist.
	 *             101 - System error occurred during open.
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
	 *    920805:  Added 'ROUNFR' option.
	 *    920319:  Added precompiler flag for SCRATCH file open on 1.
	 *    910320:  Added option of status='SCRATCH' (wct)
	 *    871222:  Moved file inquire to zinquire.
	 *             Included MASSCOMP differences.
	 *    870729:  Added 'RODATA' option.
	 *    860819:  Changed to new message system.
	 *    851216:  Modified for UNIX file I/O--D. Trimmer
	 *    841219:  Expand KBASDR to path name, and convert full path name
	 *             to lower case--D. Trimmer
	 *    830812:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870526
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Error exit if file does not exist. */

	zinquire( kname, &lexist );

	if( !lexist ){
	    *nerr = 108;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	    goto L_8888;
	}

	/* - Open data file. */

	if( memcmp(ktype,"DATA",4) == 0 ){
	    lnewfl = FALSE;
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
	else if( memcmp(ktype,"RODATA",6) == 0 ){
	    lnewfl = FALSE;
	    lro = TRUE;
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

	/* - Open text file (read-only mode ignored for text files in UNIX.) */
	else if( memcmp(ktype,"ROUNFR",6) == 0 ){
	    noerr = 1;
	    if( noerr != 0 ){
		*nerr = 101;
		setmsg( "ERROR", *nerr );
		apcmsg( kname,kname_s );
		apcmsg( "ROUNFR files not supported at this time-zopen",46 );
		apimsg( noerr );
		apcmsg( ")",2 );
		goto L_8888;
	    }
	}

	/* - Open data file (read-only mode ignored for text files in UNIX.) */
	else if( memcmp(ktype,"RODIR",5) == 0 ){
	    noerr = 1;
	    if( noerr != 0 ){
		*nerr = 101;
		setmsg( "ERROR", *nerr );
		apcmsg( kname,kname_s );
		apcmsg( "RODIR files not supported at this time-zopen",45 );
		apimsg( noerr );
		apcmsg( ")",2 );
		goto L_8888;
	    }
	}

	else{
	    *nerr = 101;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	    apcmsg( "(Bad value for file type =",27 );
	    apcmsg( ktype,ktype_s );
	    apcmsg( ")",2 );
	    goto L_8888;
	}

L_8888:
	return;

} /* end of function */


