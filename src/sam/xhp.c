#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "sam.h"
void /*FUNCTION*/ xhp(nerr)
int *nerr;
{
	int jdfl, ndx1, ndx2, nlen;
	float fnyq;



	/*=====================================================================
	 * PURPOSE:  To execute the action command HIGHPASS.
	 *           This command applies a IIR highpass filter to data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  SAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    SAM:     KTPIIR, MTPIIR
	 *    ERR:     MBDSYN, MOUTRG
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    SAM:     CFHP, ITPHP, NPOLHP, NPASHP, TBWHP, ATNHP
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * ASSUMPTIONS:
	 *=====================================================================
	 * LIMITATIONS:
	 *=====================================================================
	 * KNOWN ERRORS:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- CORNER v:  define new corner frequency. */
		if( lkrrc( "CORNER$",8, 0., VLARGE, &cmsam.cfhp ) )
		{ /* do nothing */ }

		/* -- BU/BE/C1/C2:  change type of IIR filter to perform. */
		else if( lclist( (char*)kmsam.ktpiir,9, MTPIIR, &cmsam.itphp ) )
		{ /* do nothing */ }

		/* -- NPOLES n:  define npoles of poles in filter. */
		else if( lkirc( "NPOLES$",8, 1, 10, &cmsam.npolhp ) )
		{ /* do nothing */ }

		/* -- TRANBW v:  define new transition bandwidth. */
		else if( lkrrc( "TRANBW$",8, 0., VLARGE, &cmsam.tbwhp ) )
		{ /* do nothing */ }

		/* -- ATTEN v:  define new filter attenuation factor. */
		else if( lkrrc( "ATTEN$",7, 1., VLARGE, &cmsam.atnhp ) )
		{ /* do nothing */ }

		/* -- PASSES n:  Set number of filter passes. */
		else if( lkirc( "PASSES$",8, 1, 2, &cmsam.npashp ) )
		{ /* do nothing */ }

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		return ;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		return ;

	/* - Check to make sure all files are evenly spaced time series files. */

	vfeven( nerr );
	if( *nerr != 0 )
		return ;

	/* EXECUTION PHASE: */

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		/* -- Get the next file in DFL, moving header to CMHDR. */

		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			return ;

		/* -- Check that corner frequency is within proper range. */

		fnyq = 0.5/ *delta;
		if( cmsam.cfhp > fnyq ){
			*nerr = 1611;
			setmsg( "ERROR", *nerr );
			apfmsg( cmsam.cfhp );
			apfmsg( fnyq );
			return ;
		}

		/* -- Perform highpass filter operation. */

		xapiir( cmmem.sacmem[ ndx1 ] , nlen ,
			(char*) kmsam.ktpiir[ cmsam.itphp - 1 ] , cmsam.tbwhp ,
			cmsam.atnhp , cmsam.npolhp , "HP" , cmsam.cfhp , 0. ,
			*delta , cmsam.npashp );

		/* -- Adjust header of file in DFL. */

		extrma( cmmem.sacmem[ndx1], 1, *npts, depmin, depmax, depmen );

		/* -- Reverse the steps used in getting the next file in DFL. */

		putfil( jdfl, nerr );
		if( *nerr != 0 )
			return ;

	}

	/* - Calculate and set new range of dependent variable. */

	setrng();

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820405:  Changed error message involving bad corner frequency.
	 *    820331:  Combined "parse" and "control" modules.
	 *    810302:  Added Chebyshev Type I and II filters.
	 *    810120:  Changed to output message retrieval from disk.
	 *    801211:  Added Bessel filters and two-pass logic.
	 *    801117:  Original version.
	 *===================================================================== */

} /* end of function */

