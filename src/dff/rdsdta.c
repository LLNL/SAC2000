#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sddhdr.h"
void /*FUNCTION*/ rdsdta(idfl, nun, nerr)
int idfl, *nun, *nerr;
{
	int i, i_, jcomp, jcomp_, nlcdsk, nlcmem, numrd, offset;
	float unused;

        float *Sacmem;
        int *Isacmem;

	void zrabs();

	kschan[12]='\0';
	kschdr[80]='\0';
	ksclas[4]='\0';
	kscom[40]='\0';
	ksevnm[8]='\0';
	ksfrmt[8]='\0';
	ksstnm[8]='\0';

	/*=====================================================================
	 * PURPOSE:  To read data components from a SDD disk file to memory.
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
	nlcdsk = MWSHDR;

	/* - For each data component: */

	for( jcomp = 1; jcomp <= Ncomp[idfl]; jcomp++ ){
		jcomp_ = jcomp - 1;

		/* -- Define initial memory location. */
		nlcmem = cmdfm.ndxdta[idfl - 1][jcomp_];

		/* -- Fill beginning with zeros if requested.  Update memory location. */
		if( Nfillb[idfl] > 0 ){
			fill( cmmem.sacmem[nlcmem], Nfillb[idfl], 0. );
                        offset +=  Nfillb[idfl];
			}

		/* -- Update disk location and read data. */
		if( numrd > 0 ){
			nlcdsk = nlcdsk + Nstart[idfl] - 1 + Nfillb[idfl];
                        Sacmem = cmmem.sacmem[nlcmem]+offset;
                        Isacmem = (int *)(cmmem.sacmem[nlcmem]+offset);

			zrabs( nun, Isacmem, numrd, &nlcdsk, nerr );
			for( i = 0; i <= ( numrd - 1); i++ ){
                                *(Sacmem++) = *(Isacmem++)/100.0;
				}
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
		}

	/* - Compute some header values. */

	*npts = Nlndta[idfl];
	extrma( cmmem.sacmem[cmdfm.ndxdta[idfl - 1][0]], 1, *npts, depmin, 
	 depmax, depmen );
	if( *leven ){
		*ennd = *begin + (float)( *npts - 1 )**delta;
		}
	else{
		extrma( cmmem.sacmem[cmdfm.ndxdta[idfl - 1][1]], 1, *npts, begin, 
		 ennd, &unused );
		}

	/* - Move header back to working memory. */

	putfil( idfl, nerr );

L_8888:
	return;

} /* end of function */

