#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>
#include <rpc/types.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);

void /*FUNCTION*/ rdxdrdta(idfl, kname, kname_s, nerr)
int idfl;
char * kname ; int kname_s ;
int *nerr;
{
	int jcomp, nlcmem ;
        int lendata, ncerr;
	float unused;
        FILE *nun;
        XDR xdrs;

	/*=====================================================================
	 * PURPOSE:  To read data components from an XDR format SAC disk file
         *           to memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
         *    KNAME:   Name of the input file.
         *    KNAME_S: Length of the character string holding the file name.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NSTART, NSTOP, NTOTAL, NFILLB, NFILLE
	 *    HDR:     NPTS, LEVEN, DELTA
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     DEPMIN, DEPMAX, DEPMEN, BEGIN, ENND
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  FILL, ZRABS, EXTRMA, MVHDR
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NLCMEM:  Location in memory to store data. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *  010996:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;


	/* Open the input file */
	znfiles(&nun, kname, kname_s, "TEXT", 5, nerr);
	if( *nerr != 0 ) return;

	/* Create a stream for the XDR decoding */
	xdrstdio_create(&xdrs, nun, XDR_DECODE);

	/* Read the header from disk */
	/* For portability, read and throw away the header first
	   to correctly position file for read of data.   */
	xdrhdr(xdrs, cmmem.sacmem[Ndxhdr[idfl]], nerr);
	if( *nerr != 0 ) goto L_8888;

	/* - Define number of points to read. */
	lendata = Nlndta[idfl]; 
 
	/* - For each data component: */
	for( jcomp = 0; jcomp < Ncomp[idfl]; jcomp++ ){

	    /* -- Define initial memory location. */
	    nlcmem = cmdfm.ndxdta[idfl - 1][jcomp];

	    if( !xdr_array(&xdrs, (caddr_t *) &cmmem.sacmem[nlcmem],
		(u_int *)&lendata, (u_int)lendata, (u_int)sizeof(float), (xdrproc_t) xdr_float)){
                  *nerr = 123;
                  goto L_8888;
	    }
	}

	/* - Compute some header values. */

	*npts = Nlndta[idfl];
	extrma( cmmem.sacmem[cmdfm.ndxdta[idfl - 1][0]], 1, *npts, depmin, 
	 depmax, depmen );
	if( *leven ){
	    *ennd = *begin + (float)( *npts - 1 )**delta;
	}
	else{
	    extrma( cmmem.sacmem[cmdfm.ndxdta[idfl - 1][1]], 1, *npts,
		    begin, ennd, &unused );
	}

	/* - Move header back to working memory. */

	putfil( idfl, nerr );

L_8888:
	xdr_destroy( &xdrs );
	zcloses( &nun, &ncerr );

	return;

} /* end of function */

