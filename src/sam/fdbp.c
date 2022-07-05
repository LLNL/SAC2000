#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#define	IPULSE	100
#define	SAMPLING	0.025

#include "mach.h"
#include "mem.h"
#include "sam.h"
#include "dfir.h"
void /*FUNCTION*/ fdbp(memptr, mxmptr, userData, nerr)
int memptr[], mxmptr, *nerr;
float *userData ;
{
	int idx, jdx;
	float _f0, _f1, dummy;
        char s1[3];


	int *const Memptr = &memptr[0] - 1;


	/*=====================================================================
	 * PURPOSE:  Creates filter response files in memory by evaluating
	 *           filter command line parameters for bandpass.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  SAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    SAM:     KTPIIR, MTPIIR, KPROTYP, MPROTYP, NDATPTS, NIMPPTS 
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    SAM:     CFBP, ITPBP, NPOLBP, NPASBP, TBWBP, ATNBP
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ALLAMB, INSPECT, DESIGN, ZERO, APPLY
	 *             FDDELTA
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *  mxmptr:  Number of entries in memptr. [i]
	 *  dummy:   Dummy array for passing to inspect with digital data. [r]
	 *  ipulse:  Array location of the impulse response onset. [i]
	 *=====================================================================
	 * GLOBAL VARIABLES:
	 *  dfir:  
	 *         nsects:  Number of second order sections in impulse. [i]
	 *         sn:       Numerator coefficients. [r]
	 *         sd:       Denominator coefficients. [r]
	 *         filttype: Filter type. [character*2]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *  901219 Added included file dfir for saving coefficients, nsects, and
	 *         filter type.
	 *  901116 Added delta option for selecting sampling interval.
	 *  901001 Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	strcpy( cmfir3.filttype, "BP" );
        strcpy(s1,"  ");

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- CORNERS v1 v2:  define new corner frequencies. */
		if(lkrrcp("CORNERS$",9, 0., VLARGE, &cmsam.cfbp1, &cmsam.cfbp2))
		{ /* do nothing */ }

		/* -- BU/BE/C1/C2:  change type of IIR filter to perform. */
		else if( lclist( (char*)kmsam.ktpiir,9, MTPIIR, &cmsam.itpbp ) )
		{ /* do nothing */ }

		/* -- NPOLES n:  define npoles of poles in filter. */
		else if( lkirc( "NPOLES$",8, 1, 10, &cmsam.npolbp ) )
		{ /* do nothing */ }

		/* -- TRANBW v:  define new transition bandwidth. */
		else if( lkrrc( "TRANBW$",8, 0., VLARGE, &cmsam.tbwbp ) )
		{ /* do nothing */ }

		/* -- ATTEN v:  define new filter attenuation factor. */
		else if( lkrrc( "ATTEN$",7, 1., VLARGE, &cmsam.atnbp ) )
		{ /* do nothing */ }

		/* -- PASSES n:  Set number of filter passes. */
		else if( lkirc( "PASSES$",8, 1, 2, &cmsam.npasbp ) )
		{ /* do nothing */ }

		/* -- DELTA v:  Set the delta. */
		else if( lkrrc( "D#ELTA$",8, VSMALL, VLARGE, &cmsam.fddelta ) )
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
		goto L_8888;

        /* set userData */
        userData[ 0 ] = 3 ;
        userData[ 1 ] = cmsam.itpbp ;
        userData[ 2 ] = cmsam.npolbp ;
        userData[ 3 ] = cmsam.npasbp ;
        userData[ 4 ] = cmsam.tbwbp ;
        userData[ 5 ] = cmsam.atnbp ;
        userData[ 6 ] = cmsam.fddelta ;
        userData[ 7 ] = cmsam.cfbp1 ;
        userData[ 8 ] = cmsam.cfbp2 ;


	/* - Compute each filter response prototype and put results in memory */

	/* -- Allocate and load x and y data arrays for analog signals.
	 *    This will be memptr(1 thru 6) */

	jdx = 1;
	for( idx = 0; idx < 3; idx++ ){
		allamb( &cmmem, NDATPTS, &Memptr[jdx], nerr );
		if( *nerr != 0 )
			goto L_8888;
		allamb( &cmmem, NDATPTS, &Memptr[jdx + 1], nerr );
		if( *nerr != 0 )
			goto L_8888;

                strncpy(s1,kmsam.ktpiir[cmsam.itpbp - 1],2);

                _f0 = 0.0;
                _f1 = 1./ (2.*cmsam.fddelta);
		inspect( cmsam.npolbp, "BP", s1, cmsam.atnbp, cmsam.tbwbp,
			 cmsam.cfbp1, cmsam.cfbp2, cmsam.fddelta,
			 (char*)kmsam.kprotyp[idx], NDATPTS, &_f0, &_f1,
			 "LINEAR  ", cmmem.sacmem[Memptr[jdx]],
			 cmmem.sacmem[Memptr[jdx + 1]] );

		jdx = jdx + 2;
	}

	/* -- Allocate and load y data array only for digital signals.
	 *    This will be memptr(7 thru 9) */

	for( idx = 3; idx < MPROTYP; idx++ ){
		allamb( &cmmem, NDATPTS, &Memptr[idx + 4], nerr );
		if( *nerr != 0 )
			goto L_8888;

                strncpy(s1,kmsam.ktpiir[cmsam.itpbp - 1],2);

                _f0 = 0.0;
                _f1 = 1./ (2.*cmsam.fddelta);
		inspect( cmsam.npolbp, "BP", s1, cmsam.atnbp, cmsam.tbwbp,
			 cmsam.cfbp1, cmsam.cfbp2, cmsam.fddelta,
			 (char*)kmsam.kprotyp[idx], NDATPTS, &_f0, &_f1,
			 "LINEAR  ", cmmem.sacmem[Memptr[idx + 4]],
			 (float*)&dummy );
	}

	/* - Create the impulse response */

	allamb( &cmmem, NIMPPTS, &Memptr[mxmptr], nerr );
	if( *nerr != 0 )
		goto L_8888;

        strncpy(s1,kmsam.ktpiir[cmsam.itpbp - 1],2);

	design( cmsam.npolbp, "BP", s1, 
	 cmsam.atnbp, cmsam.tbwbp, cmsam.cfbp1, cmsam.cfbp2, cmsam.fddelta, 
	 cmfir3.sn, cmfir3.sd, &cmfir3.nsects );
	zero( cmmem.sacmem[Memptr[mxmptr]], NIMPPTS );
	*(cmmem.sacmem[Memptr[mxmptr]] + IPULSE - 1) = 1.;
	apply( cmmem.sacmem[Memptr[mxmptr]], NIMPPTS, FALSE,
	       cmfir3.sn, cmfir3.sd, cmfir3.nsects );

L_8888:
	return;

} /* end of function */



