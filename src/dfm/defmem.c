#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void apcmsg2(char* kalpha, int kalpha_s);
void /*FUNCTION*/ defmem(idfl, lcutnow, nerr)
int idfl, *nerr;
int lcutnow ;
{
	int lall;
	int ic1, ic2;



	/*=====================================================================
	 * PURPOSE:  To define memory requirements for a given data file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list number. [i]
	 *    LCUTNOW: TRUE if the file is being read and needs to be cut. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1321, 1356
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     LCUT
	 *    HDR:     IFTYPE, IRLIM, IAMPH, ITIME, IXY, IUNKN, LEVEN, NPTS
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NSTART, NSTOP, NTOTAL, NFILLB, NFILLE, NCOMP, NLNDTA
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM, DEFCUT
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Get name of data file. */

	lnumcl( kmdfm.kdfl,MAXCHARS, idfl, &ic1, &ic2 );

	/* - Entire file is read if:
	 *   (1) cut option is off.
	 *   (2) file is a spectral file
	 *   (3) file is unevenly spaced */

	if( !cmdfm.lcut || !lcutnow ){
		lall = TRUE;
	}
	else if( *iftype == *irlim || *iftype == *iamph ){
		setmsg( "WARNING", 1321 );
                apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		outmsg();
		lall = TRUE;
	}
	else if( !*leven ){
		setmsg( "WARNING", 1356 );
                apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		outmsg();
		lall = TRUE;
	}
	else{
		lall = FALSE;
	}

	/* - Set parameters if entire file is to be read. */
	if( lall ){
		Nstart[idfl] = 1;
		Nstop[idfl] = *npts;
		Ntotal[idfl] = *npts;
		Nfillb[idfl] = 0;
		Nfille[idfl] = 0;
	}

	/* - Set parameters for partial read. */
	else{
		defcut( kmdfm.kcut , cmdfm.ocut , idfl , nerr );
		if( *nerr != 0 )
			goto L_8888;
	}

	/* - Define number of data components. */

	if( (*iftype == *itime || *iftype == *ixy) || *iftype == *iunkn ){
		if( *leven ){
			Ncomp[idfl] = 1;
		}
		else{
			Ncomp[idfl] = 2;
		}
	}
	else if( *iftype == *ixyz ){
		Ncomp[idfl] = 1;
	}
	else{
		Ncomp[idfl] = 2;
	}

	/* - Define length of each data component, including zero fill. */

	Nlndta[idfl] = Nstop[idfl] - Nstart[idfl] + 1;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850415:  Changes due to restructuring of DFM common block.
	 *    840228:  Moved cut logic to new subroutine, DEFCUT.
	 *    820809:  Changed method of storing cut parameters.
	 *    810903:  Fixed bug in computing stop time when "N" option used.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850415
	 *===================================================================== */

} /* end of function */

