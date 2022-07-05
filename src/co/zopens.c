#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#define	MRECLB	512

#include "../../inc/mach.h"
void /*FUNCTION*/ zopens(nfu, kname, kname_s, ktype, ktype_s, nerr)
FILE **nfu;
char *kname;   int kname_s;
char *ktype;   int ktype_s;
int *nerr;
{
        char *nkname, *tok;
	int lexist;
	int noerr;

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
        noerr = 0;

	/* - Error exit if file does not exist. */

	zinquire( kname, &lexist );

	if( !lexist ){
	    *nerr = 108;
	    setmsg( "ERROR", *nerr );
	    apcmsg( kname,kname_s );
	    goto L_8888;
	}

	if( (memcmp(ktype,"TEXT",4) == 0 || memcmp(ktype,
	 "ROTEXT",6) == 0) || memcmp(ktype,"READ",4) == 0 ){
            strcpy((nkname=malloc(strlen(kname)+1)),kname);
            tok = strtok(nkname," \0");

            if((*nfu = fopen(tok,"r")) == NULL) noerr = 1;
	    if( noerr != 0 ){
                free(nkname);
		*nerr = 101;
		setmsg( "ERROR", *nerr );
		apcmsg( kname,kname_s );
		apcmsg( "(Fortran i/o error number =",28 );
		apimsg( noerr );
		apcmsg( ")",2 );
		goto L_8888;
	    }
            free(nkname);


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

