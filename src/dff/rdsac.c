#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

void /*FUNCTION*/ rdsac(idfl, kname, kname_s, lname, ldta, nlen, ndxh, ndx1, 
	 ndx2, nerr)
int idfl;
char *kname;   int kname_s;
int lname , ldta;
int *nlen, *ndxh, *ndx1, *ndx2, *nerr;
{
	int jcomp, ncerr, nun, lswap = 0 ;


	/*=====================================================================
	 * PURPOSE:  To read a SAC data file into memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *             Used to store information about location of header
	 *             and data components after read.
	 *    KNAME:   Name of data file to read. [c]
	 *    LNAME:   Set to TRUE if name to be added to list of filenames.
	 *             Set to FALSE otherwise.  [l]
	 *    LDTA:    Set to TRUE if header and data is to be read. [l]
	 *             Set to FALSE if only the header is to read.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NLEN:    Number of data points read. [i]
	 *    NDXH:    Index in SACMEM array of header. [i]
	 *    NDX1:    Index in SACMEM array of first data component. [i]
	 *    NDX2:    Index in SACMEM array of second data component. [i]
	 *             Set to 0 if second component does not exist.
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     ncomp, nlndta
	 *    hdr:     MHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    dfm:     kdfl, ndxhdr, ndxdta, idflc
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  zgtfun, zopen, allamb, putcl, rdhdr, defmem, rddta, zclose
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NUN:     Fortran file unit used to read file. [i]
	 *    NLNFIL:  Length of file that is opened. [i] {UNUSED}
	 *    NCERR:   Error return flag from ZCLOSE. [i] {UNUSED}
	 *=====================================================================
	 * ASSUMPTIONS:
	 * - Files are added to data file list from bottom up.
	 * - No memory blocks for file IDFL have been allocated.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    980922:  Added lname to discriminate between filenames which
	 *             should or should not be added to the list.  maf.
	 *    880913:  Fixed bug in defining data file list.
	 *    850617:  Major rewrite due to addition of memory manager.
	 *             CHANGED NUMBER AND ORDER OF ARGUMENTS.
	 *    821004:  Fixed bug which was incorrectly clearing error flag.
	 *    810120:  Changed to output message retrieval from disk.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850730
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Open file. */

	zopen_sac( &nun, kname,kname_s, "RODATA",7, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Allocate a memory block for header. */

	allamb( &cmmem, MHDR, ndxh, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Save some parameters about this data file. */

	cmdfm.idflc = idfl;
	Ndxhdr[cmdfm.idflc] = *ndxh;
	if ( lname )
	    putcl( kmdfm.kdfl,MAXCHARS, kname,kname_s, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Read header record. */

	lswap = rdhdr( idfl, &nun, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Read data if requested. */

	if( ldta ){
	    /* -- Determine memory requirements for this file. */
	    defmem( idfl, TRUE, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Allocate memory block(s) for data. */
	    for( jcomp = 0; jcomp < Ncomp[idfl]; jcomp++ ){
		allamb( &cmmem, Nlndta[idfl], &cmdfm.ndxdta[idfl - 1][jcomp], nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    /* -- Read data components. */
	    rddta( idfl, &nun, lswap, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Return indicies to data components. */
	    *nlen = Nlndta[idfl];
	    *ndx1 = cmdfm.ndxdta[idfl - 1][0];
	    if( Ncomp[idfl] > 1 )
		*ndx2 = cmdfm.ndxdta[idfl - 1][1];
	    else
		*ndx2 = 0;
	}
	else{
	    *nlen = 0;
	    *ndx1 = 0;
	    *ndx2 = 0;
	}

	/* - Close file and return. */

L_8888:
	zclose( &nun, &ncerr );
	return;

} /* end of function */

