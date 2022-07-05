#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ rddta(idfl, nun, lswap, nerr)
int idfl, *nun, lswap, *nerr;
{
	int jcomp, nlcdsk, nlcmem, numrd, offset;
        int idx ;
	float unused;
	void zrabs();


	/*=====================================================================
	 * PURPOSE:  To read data components from a SAC disk file to memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    NUN:     Fortran file unit on which data file is open. [i]
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
	 *    SACLIB:  FILL, ZRABS, EXTRMA, PUTFIL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NUMRD:   Number of data points to read from disk file. [i]
	 *    NLCDSK:  Location in disk file to start read. [i]
	 *    NLCMEM:  Location in memory to store data. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870515:  Fixed bug involving zero fill option.
	 *    850415:  Changes due to restructuring of DFM common block.
	 *    811202:  Added calculation of BEGIN and ENND for uneven data.
	 *    811120:  Added calculation of ENND.
	 *    810423:  Deleted option to convert format of spectral files
	 *             as they are read into memory.
	 *    810416:  Replaced CMWORK with local storage.
	 *    810120:  Changed to output message retrieval from disk.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
        offset = 0;
	/* - Define number of points to read and initial disk location. */

	numrd = Nstop[idfl] - Nstart[idfl] + 1 - Nfillb[idfl] - Nfille[idfl];
        nlcdsk = MHDRFILE;

	/* - For each data component: */

	for( jcomp = 0; jcomp < Ncomp[idfl]; jcomp++ ){
            offset = 0;

	    /* -- Define initial memory location. */
	    nlcmem = cmdfm.ndxdta[idfl - 1][jcomp];

	    /* -- Fill beginning with zeros if requested.  Update memory location. */
	    if( Nfillb[idfl] > 0 ){
		fill( cmmem.sacmem[nlcmem], Nfillb[idfl], 0. );
		offset += Nfillb[idfl];
	    }

	    /* -- Update disk location and read data. */
	    if( numrd > 0 ){
		nlcdsk = nlcdsk + Nstart[idfl] - 1 + Nfillb[idfl];
		zrabs( nun, cmmem.sacmem[nlcmem]+offset, numrd, &nlcdsk, nerr );
                if( lswap ){     /* byteswap if necessary. */
                    int idx ;
                    float *ptr ;

                    for( idx = 0, ptr = cmmem.sacmem[nlcmem]+offset ;
                         idx < numrd ; idx++, ptr++ )
                    {
                        byteswap( (void *)ptr, 4 ) ;
                    }
                } /* end if( lswap ) */
		if( *nerr != 0 )
		    goto L_8888;
		offset += numrd;
	    }

	    /* -- Fill end with zeros if requested. */
	    if( Nfille[idfl] > 0 ){
		fill( cmmem.sacmem[nlcmem]+offset, Nfille[idfl], 0. );
		offset += Nfille[idfl];
	    }

	    /* -- Update disk location to point to start of next component. */
	    nlcdsk = nlcdsk + Ntotal[idfl] - Nstart[idfl] + 1;
	} /* end for ( jcomp ) */

	/* - Compute some header values. */

	*npts = Nlndta[idfl];
	extrma( cmmem.sacmem[cmdfm.ndxdta[idfl - 1][0]], 1, *npts, depmin, 
	 depmax, depmen );
	if( *leven )
	    *ennd = *begin + (float)( *npts - 1 )**delta;
	else{
	    extrma( cmmem.sacmem[cmdfm.ndxdta[idfl - 1][1]], 1, *npts, begin, 
	     ennd, &unused );
	}

	/* - Move header back to working memory. */

	putfil( idfl, nerr );

L_8888:
	return;

} /* end of function */

