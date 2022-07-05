#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <rpc/rpc.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ xdrhdr( xdrs, headerbuf, nerr)
XDR xdrs;
float *headerbuf;
int *nerr;
{
        int nfloat, nint, nbytes;
        char *cbuf;
        int *lbuf;

	/*=====================================================================
	 * PURPOSE:  To write a SAC header from memory to disk in XDR 
         *           (portable) format.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    HEADERBUF:  Pointer to a buffer containing a SAC header.
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
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    010496:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

        if( !xdr_setpos(&xdrs, 0)){
          *nerr = 121;
          goto L_8888;
	}
/* Read/Write the floating point header block */
        nfloat = MFHDR;
        if( !xdr_array(&xdrs, (caddr_t *)&headerbuf, (u_int *)&nfloat,
                       (u_int)nfloat, sizeof(float), (xdrproc_t) xdr_float)){
          *nerr = 121;
          goto L_8888;
	}

/* Read/Write the int header vars (nhdr, ihdr and lhdr) */
        nint = MNHDR + MIHDR + MLHDR;
        lbuf = (int *)(headerbuf+nfloat);
        if( !xdr_array(&xdrs, (caddr_t *)&lbuf, (u_int *)&nint,
                       (u_int)nint, sizeof(int), (xdrproc_t) xdr_int)){
          *nerr = 121;
          goto L_8888;
	}

/* Read/Write the character header fields */
        nbytes = MKHDR * 9;
        cbuf = (char *)(headerbuf+nfloat+nint);
        if( !xdr_bytes(&xdrs, &cbuf, (u_int *)&nbytes, (u_int)nbytes)){
          *nerr = 121;
          goto L_8888;
        } 

L_8888:

	return;

} /* end of function */

