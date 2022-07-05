#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ crsac(idfl, ncmp, nlen, ndxh, ndx1, ndx2, nerr)
int idfl, ncmp, nlen, *ndxh, *ndx1, *ndx2, *nerr;
{
	int jcomp, jcomp_, jrel, jrel_, nrerr;



	/*=====================================================================
	 * PURPOSE:  To create a "new file" in SAC memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    NCMP:    Number of data components. [i]
	 *    NLEN:    Length of each data component. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NDXH:    Index in SACMEM array of header. [i]
	 *    NDX1:    Index in SACMEM array of first (y) data component. [i]
	 *    NDX2:    Index of second (x) component if it exists. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     MHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NDXHDR, NLNDTA, NCOMP, NDXDTA
	 *    MEM:     SACMEM
	 *    HDR:     NPTS, LEVEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ALLAMB, RELAMB, NEWHDR, FILL, PUTFIL
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Allocate memory for header. */

	allamb( &cmmem, MHDR, &Ndxhdr[idfl], nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - For each data component: */

	Nlndta[idfl] = nlen;
	Ncomp[idfl] = ncmp;
	for( jcomp = 1; jcomp <= Ncomp[idfl]; jcomp++ ){
		jcomp_ = jcomp - 1;

		/* -- Allocate memory block. */
		allamb( &cmmem, Nlndta[idfl], &cmdfm.ndxdta[idfl - 1][jcomp_], 
		 nerr );

		/* -- If error occurred, release other blocks before returning. */
		if( *nerr != 0 ){
			relamb( cmmem.sacmem, Ndxhdr[idfl], &nrerr );
			for( jrel = 1; jrel <= (jcomp - 1); jrel++ ){
				jrel_ = jrel - 1;
				relamb( cmmem.sacmem, cmdfm.ndxdta[idfl - 1][jrel_], 
				 &nrerr );
				cmdfm.ndxdta[idfl - 1][jrel_] = 0;
				}
			goto L_8888;
			}

		}

	/* - Set return arguments if no error occurred. */

	*ndxh = Ndxhdr[idfl];
	*ndx1 = cmdfm.ndxdta[idfl - 1][0];
	*ndx2 = cmdfm.ndxdta[idfl - 1][1];

	/* - Initialize header and data components to default values. */

	newhdr();
	*npts = nlen;
	fill( cmmem.sacmem[*ndx1], *npts, 0. );
	*leven = ncmp == 1;
	if( !*leven )
		fill( cmmem.sacmem[*ndx2], *npts, 0. );

	/* - Give file to data manager. */

	putfil( idfl, nerr );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850731:  Changes due to new memory manager.
	 *             CHANGED NUMBER AND ORDER OF ARGUMENTS.
	 *    840120:  Cleaned up and documented.
	 *    821222:  Added zeroing of data arrays.
	 *    810120:  Changed to output message retrieval from disk.
	 *=====================================================================
	 * DOCUMENTED/REVIEWD:  850731
	 *===================================================================== */

} /* end of function */

