#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/spe.h"
/*                                                  SPECTR
 *
 *  Dispatcher for three spectral estimation algorithms.
 *  Also corrects spectral estimate for effects of prewhitening.
 *
 *  Created:  December 17, 1984
 *
 *  Last Modified:  January 3, 1985
 *                  May 28, 1998;  allow window length to be variable. maf
 *
 *  Author:  Dave Harris
 *           L-205
 *           Lawrence Livermore National Laboratory
 *           P. O. Box 808
 *           Livermore, CA  94550
 *
 *  Input Arguments:
 *  ----- ----------
 *
 *    R             REAL*4 Array containing autocorrelation
 *                  sequence.  An integer variable.
 *
 *    R_SIZE        Size of fft used to compute the
 *                  correlation sequence.  Must be a power
 *                  of two.
 *
 *    #LAGS         Number of non-zero correlation function samples.
 *                  An integer variable.
 *
 *    ESTIMATOR     CHARACTER*3 variable containing name of estimator.
 *                  Three names are legal:  MLM, MEM, PDS.
 *
 *    ORDER         An integer variable containing the order of
 *                  the designated spectral estimator. For PDS,
 *                  this is the window size in samples.  For MEM, it
 *                  is the order of the predictor.  For MLM, it is
 *                  the dimension of the autocorrelation matrix.
 *
 *    FFT_SIZE      An integer variable containing the size of the
 *                  fft to be used to compute the displayed spectral
 *                  estimate.
 *
 *    WINDOW_TYPE   A CHARACTER*8 variable containing the name of
 *                  the window to be applied to the correlation
 *                  sequence in the event that the PDS estimator is
 *                  selected.
 *
 *    A             REAL*4 array containing the prediction error
 *                  filter coefficients used to prewhiten the data
 *                  sequence, if prewhitening was performed.
 *
 *    A_ORDER       An integer variable containing the order of
 *                  the prewhitening filter used.  Contents must be
 *                  0 (zero) if no prewhitening is performed.
 *
 *  Output Arguments:
 *  ------ ----------
 *
 *    SPECTRUM      Here it is folks, the much sought after spectrum.
 *                  A REAL*4 array containing the spectrum in the
 *                  peculiar, circularly rotated format that results
 *                  from fft computations.  The dc spectral value is
 *                  in SPECTRUM(1).  There are FFT_SIZE/2 + 1 spectral
 *                  samples spanning the frequency domain from 0 to the
 *                  folding frequency, starting with SPECTRUM(1).  The
 *                  remaining values in the array represent a mirror
 *                  image of these first values.  The dimension of this
 *                  array is FFT_SIZE.
 *
 *    ERROR_MESSAGE CHARACTER*130 variable containing an error message upon
 *                  detection of an error condition, ' ' otherwise.
 *
 *  Auxiliary Arguments:
 *  --------- ----------
 *
 *    AUXILIARY     A REAL*4 array of dimension 2*FFT_SIZE or larger.  Used
 *                  for miscellaneous intermediate calculations.  Contains
 *                  garbage on return.  May contain garbage on entry.
 *
 *  Linkage:  PDS, MLM, MEM, FFT
 *
 *  Replacements:
 *
 * */
void /*FUNCTION*/ spectr(r, nrsize, nlags, etype, order, nfft, wtype, 
	 a, aorder, s, errmsg, errmsg_s, aux)
float r[];
int nrsize, nlags;
char *etype;
int *order, nfft;
char *wtype;
float a[];
int aorder;
float s[];
char *errmsg;   int errmsg_s;
float aux[];
{
	char tempe[131];
	int i;

	float *const A = &a[0] - 1;
	float *const Aux = &aux[0] - 1;
	float *const R = &r[0] - 1;
	float *const S = &s[0] - 1;




	/*  Dispatcher */
	if( memcmp(etype,"PDS",3) == 0 ){
	    pds( r, nrsize, nlags, order, wtype, nfft, s, errmsg,errmsg_s, aux );
	    if( memcmp(errmsg,"        ",8) != 0 ){
                fstrncpy(tempe, 130, errmsg, strlen(errmsg));
                fstrncpy(tempe+strlen(errmsg),130-strlen(errmsg),
                  " from (SPECTR)", 14);
                fstrncpy(errmsg, errmsg_s-1, tempe, strlen(tempe)); 

	    }

	}
	else if( memcmp(etype,"MLM",3) == 0 ){

	    mlm( r, *order, nfft, s, errmsg,errmsg_s, aux );
	    if( memcmp(errmsg,"        ",8) != 0 ){
                fstrncpy(tempe, 130, errmsg, strlen(errmsg));
                fstrncpy(tempe+strlen(errmsg),130-strlen(errmsg),
                  " from (SPECTR)", 14);
                fstrncpy(errmsg, errmsg_s-1, tempe, strlen(tempe)); 

	    }

	}
	else if( memcmp(etype,"MEM",3) == 0 ){

	    mem( r, *order, nfft, s, errmsg,errmsg_s, aux );
	    if( memcmp(errmsg,"        ",8) != 0 ){
                fstrncpy(tempe, 130, errmsg, strlen(errmsg));
                fstrncpy(tempe+strlen(errmsg),130-strlen(errmsg),
                  " from (SPECTR)", 14);
                fstrncpy(errmsg, errmsg_s-1, tempe, strlen(tempe)); 

	    }

	}
	else{

            fstrncpy(errmsg, errmsg_s-1,
             "SPECTR *** Invalid spectral estimator type ***" ,46 ); 
	    return;

	}

	/*  Correction for prewhitening */
	if( !(aorder == 0) ){

	    zero( &Aux[1], nfft );
	    zero( &Aux[cmspe.firstPowerOf2 + 1], nfft );
	    copy( (int*)&A[1], (int*)&Aux[1], aorder + 1 );
	    fft( &Aux[1], &Aux[cmspe.firstPowerOf2 + 1], nfft, -1 );
	    for( i = 1; i <= nfft; i++ )
		S[i] = S[i]/(powi(Aux[i],2) + powi(Aux[cmspe.firstPowerOf2 + i],2));

	}

	/*  Bye */

	return;
} /* end of function */

