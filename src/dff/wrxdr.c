#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <rpc/rpc.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);



void /*FUNCTION*/ wrxdr(idfl, kname, kname_s, ldta, nerr)
int idfl;
char *kname;   int kname_s;
int ldta;
int *nerr;
{
	int jcomp, ncerr, nlcmem, nptwr; 
        FILE *nun;
        XDR xdrs;

	/*=====================================================================
	 * PURPOSE:  To write a SAC data file from memory to disk in XDR 
         *           (portable) format.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    KNAME:   Name of disk file to write. [c]
	 *    LDTA:    Set to .TRUE. if header and data are to be written. [l]
	 *             Set to .FALSE. if only header is to be written.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NCOMP, NLNDTA, NDXHDR, NDXDTA
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZNFILES, ZCLOSES
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NUN:     Stream used to write file. [i]
	 *    NLCMEM:  Location in SACMEM array to write from. [i]
	 *    NPTWR:   Number of words to write. [i]
	 *    NCERR:   Error flag returned by ZCLOSE. [i] {UNUSED}
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    010496:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        if( !ldta ){
          *nerr = 122;
          return;
	}

	/* create a file */
	znfiles(&nun, kname, kname_s, "TEXT", 5, nerr);
	if( *nerr != 0 )
	    return;

	/* create a stream for the XDR conversions */
	xdrstdio_create(&xdrs, nun, XDR_ENCODE);

	/* - Write the header to disk. */

	nlcmem = Ndxhdr[idfl];

	xdrhdr(xdrs, cmmem.sacmem[nlcmem],nerr);
	if( *nerr != 0 )
	    goto L_8888;

	/* - Write each data component, if requested. */

	if( ldta ){
	    for( jcomp = 0; jcomp < Ncomp[idfl]; jcomp++ ){
		nlcmem = cmdfm.ndxdta[idfl - 1][jcomp];
		nptwr = Nlndta[idfl];

		if( !xdr_array(&xdrs, (caddr_t *)&cmmem.sacmem[nlcmem],
		    (u_int *)&nptwr, (u_int)nptwr, sizeof(float),  (xdrproc_t) xdr_float)){
		    *nerr = 121;
		    goto L_8888;
		}
	    }
	}

/* - Close disk file. */

L_8888:
        xdr_destroy(&xdrs);
	zcloses( &nun, &ncerr );

	return;

} /* end of function */

