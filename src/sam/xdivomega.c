#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ xdivomega(nerr)
int *nerr;
{
	int j, j_, jdfl, jdfl_, jj, ndx1, ndx2, nfreq, nlen;
	float const_, oldimag, oldreal, slope, value;



	/*=====================================================================
	 * PURPOSE: To parse and execute the action command DIVOMEGA.
	 *          This command divides a spectral file by a ramp
	 *          function equal to omega.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  SAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     IFTYPE, DEPMIN, DEPMAX, DEPMEN
	 *    MEM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870918:  Fixed some major logic flaws in execution phase.
	 *    870811:  Now works directly in the frequency domain rather than
	 *             by faking it by first writing file using writesp.
	 *    830000:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */
#ifdef DEBUG
        malloc_verify();
#endif


	/* PROCEDURE: */
	*nerr = 0;

	/* - CHECKING PHASE: */

	/* - Test for a non-null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Make sure each file is a spectral file. */

	vfspec( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - EXECUTION PHASE: */

	/* - Perform the requested function on each file in DFL. */
#ifdef DEBUG
        malloc_verify();
#endif

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		jdfl_ = jdfl - 1;

		/* -- Get next file from the memory manager.
		 *    (Header is moved into common blocks CMHDR and KMHDR.) */
		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Need to divide spectra by "eye omega". */
#ifdef DEBUG
        malloc_verify();
#endif

		/* -- If real-imaginary this means: (REAL, IMAG) = (+IMAG/omega, -REAL/omega) */
		if( *iftype == *irlim ){

#ifdef DEBUG
        malloc_verify();
#endif

			nfreq = *npts/2;
			value = 2.*PI**delta;
			slope = 2.*PI**delta;
			*(cmmem.sacmem[ndx1]) = 0.;
			*(cmmem.sacmem[ndx2]) = 0.;
			for( j = 1; j <= (nfreq - 1); j++ ){
				oldreal = *(cmmem.sacmem[ndx1] + j);
				oldimag = *(cmmem.sacmem[ndx2] + j);
				*(cmmem.sacmem[ndx1] + j) = oldimag/value;
				*(cmmem.sacmem[ndx2] + j) = -oldreal/value;
				jj = *npts - j;
				*(cmmem.sacmem[ndx1] + jj) = *(cmmem.sacmem[ndx1] + j);
				*(cmmem.sacmem[ndx2] + jj) = -(*(cmmem.sacmem[ndx2] + j));
				value = value + slope;
				}
			oldreal = *(cmmem.sacmem[ndx1] + nfreq);
			oldimag = *(cmmem.sacmem[ndx2] + nfreq);
			*(cmmem.sacmem[ndx1] + nfreq) = oldimag/value;
			*(cmmem.sacmem[ndx2] + nfreq) = -oldreal/value;
#ifdef DEBUG
        malloc_verify();
#endif

			/* -- If amplitude-phase this means: (AMP, PHASE) = (AMP/omega, PHASE-pi/2) */
			}
		else{

#ifdef DEBUG
        malloc_verify();
#endif

			nfreq = *npts/2;
			value = 2.*PI**delta;
			slope = 2.*PI**delta;
			*(cmmem.sacmem[ndx1]) = 0.;
			const_ = 0.5*PI;
			*(cmmem.sacmem[ndx2]) += const_;
			for( j = 1; j <= (nfreq - 1); j++ ){
				*(cmmem.sacmem[ndx1] + j) /= value;
				*(cmmem.sacmem[ndx2] + j) -= const_;
				jj = *npts - j;
				*(cmmem.sacmem[ndx1] + jj) = *(cmmem.sacmem[ndx1] + j);
				*(cmmem.sacmem[ndx2] + jj) = -(*(cmmem.sacmem[ndx2] + j));
				value = value + slope;
				}
			*(cmmem.sacmem[ndx1] + nfreq) /= value;
			*(cmmem.sacmem[ndx2] + nfreq) -= const_;
			}
#ifdef DEBUG
        malloc_verify();
#endif

		/* -- Update any header fields that may have changed. */
		extrma( cmmem.sacmem[ndx1], 1, nlen, depmin, depmax, depmen );
#ifdef DEBUG
        malloc_verify();
#endif

		/* -- Return file to memory manager. */
		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

		}

	/* - Calculate and set new range of dependent variable. */
#ifdef DEBUG
        malloc_verify();
#endif

	setrng();

#ifdef DEBUG
        malloc_verify();
#endif

L_8888:
	return;

} /* end of function */

