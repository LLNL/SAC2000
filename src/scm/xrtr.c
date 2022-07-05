#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void /*FUNCTION*/ xrtr(nerr)
int *nerr;
{
	int jdfl, jy, ndx1, ndx2, nlen;
	float corrcf, sddta, sdslp, sdyint, slp, x, yint;

        float *Sacmem, *Sacmem1, *Sacmem2;
        char rtr_slp[20], rtr_sdslp[20];
	char rtr_yint[20], rtr_sdyint[20];
	char rtr_sddta[20], rtr_corrcf[20];
	
	/*=====================================================================
	 * PURPOSE:  To execute the action command RTREND.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag.
	 *=====================================================================
	 * MODULE/LEVEL:  SCM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     DEPMIN, DEPMAX, DEPMEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  VFLIST, VFTIME, GETFIL, LIFITE, LIFITU,
	 *             WRTXTT, EXTRMA, PUTFIL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    SLP:     Slope of linear trend.
	 *    YINT:    Intercept of linear trend.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Check to make sure all files are time series files. */

	vftime( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
		/* -- Get the next file in DFL, moving header to CMHDR. */

		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Two versions of linear fit: one for evenly spaced
		 *    data and one for unevenly spaced data. */

		if( *leven ){
			lifite( *b, *delta, cmmem.sacmem[ndx1], nlen, &slp, &yint, 
			 &sdslp, &sdyint, &sddta, &corrcf );
			x = *b;
                        Sacmem = cmmem.sacmem[ndx1];
			for( jy = ndx1; jy <= (ndx1 + nlen - 1); jy++ ){
                                *Sacmem = *Sacmem - yint - slp*x;
				x = x + *delta;
                                Sacmem++;
			}
		}
		else{
			lifitu( cmmem.sacmem[ndx2], cmmem.sacmem[ndx1], nlen, &slp, &yint, 
			 &sdslp, &sdyint, &sddta, &corrcf );
                        Sacmem1 = cmmem.sacmem[ndx1];
                        Sacmem2 = cmmem.sacmem[ndx2];
			for( jy = ndx1; jy <= (ndx1 + nlen - 1); jy++ ){
                                *Sacmem1 = *Sacmem1 - yint - slp**Sacmem2;
                                Sacmem1++;
                                Sacmem2++;
			}
		}

		/* -- Write results oflinear fit. */

		setmsg( "OUTPUT", 0 );
		apcmsg( "Slope and standard deviation are:",34 );
		apfmsg( slp );
		apfmsg( sdslp );
		aplmsg( "Intercept and standard deviation are:",38 );
		apfmsg( yint );
		apfmsg( sdyint );
		aplmsg( "Data standard deviation is:",28 );
		apfmsg( sddta );
		aplmsg( "Data correlation coefficient is:",33 );
		apfmsg( corrcf );
		outmsg();


		/* -- write the fitting parameters to blackboard variables */
		sprintf(rtr_slp,"%12.6g",slp);
		sprintf(rtr_sdslp,"%12.6g",sdslp);
		sprintf(rtr_yint,"%12.6g",yint);
		sprintf(rtr_sdyint,"%12.6g",sdyint);
		sprintf(rtr_sddta,"%12.6g",sddta);
		sprintf(rtr_corrcf,"%12.6g",corrcf);
		
		setbbv("rtr_slp ",rtr_slp,nerr,7,19);
		setbbv("rtr_sdslp ",rtr_sdslp,nerr,9,19);
		setbbv("rtr_yint ",rtr_yint,nerr,8,19);
		setbbv("rtr_sdyint ",rtr_sdyint,nerr,10,19);
		setbbv("rtr_sddta ",rtr_sddta,nerr,9,19);
		setbbv("rtr_corrcf ",rtr_corrcf,nerr,10,19);

		/* -- Update any header fields that may have changed. */

		extrma( cmmem.sacmem[ndx1], 1, nlen, depmin, depmax, depmen );

		/* -- Reverse the steps used in getting the next file in DFL. */

		putfil( jdfl, nerr );
		if( *nerr != 0 )
			goto L_8888;

	}

	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820817:  Documented subroutine.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    810528:  Original version.
	 *===================================================================== */

} /* end of function */

