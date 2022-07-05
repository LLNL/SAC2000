#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <assert.h>
#include "libresponse.h"

/* The Cooley-Tukey multiple column algorithm, see page 124 of Loan.
   xr is input data, overwritten by output, viewed as n/n2 by n2
   array. flag = 1 for forward and -1 for backward transform.
   
   Note:
   flag also is always forward
   Source: http://staff.science.nus.edu.sg/~phywjs/CZ5101/fft.c
 
 * NAME
 *	odfftr -- 1-D FFT function.

 * FILE
 *	odfftr.c

 * SYNOPSIS
 *	Perform a one-dimensional FFT on a given set of data.

 * DESCRIPTION
 *	Function.  Odfftr does a one-dimensional fft on the series in xr
 *	and xi. xr is the real part of the series and xi is the imaginary
 *	part of the series.  A forward transform (flag = 1) does no
 *	scaling.  Thus a series with a value of 100.0 in the zero position
 *	and 0.0 everywhere else will give a result of 100.0 everywhere
 *	in the real series and 0.0 in the imaginary series.

 *	---- On entry ----
 *	nexp:	Exponent power of 2 representing length of xr.
 *	xr: 	Real part of input series.
 *	flag:	Signifies forward transformation w/o scaling, if positive.	

 *	---- On return ----
 *	xr: 	Real part of new Fourier Transformed series.

 * DIAGNOSTICS
 *	None.

 * NOTES
 *	Input data must be of length 2**nexp.

*/

void odfftr( int* nexp, float* xr, int flag)

{
   /* complex c; */
   float c;
   int i, j, k, m, p, n1;
   int Ls, ks, ms, jm, dk;
   float wr, wi, tr, ti;
  
   float *xi;
   int n2;
   int npts, n, jj;
   
   
   /* need to build imaginary array */
   npts = 1;
   n2 = 1;
   
	npts = 1;
	
	i = *nexp;
	while (i-- > 0) npts <<= 1;
	
	n=npts;
	
	
	xi = (float *)malloc((npts+2)*sizeof(float)); 
	
	
	
	if( flag < 0) 
	{
		for( i = 0; i <= npts/2; i++)
		{
			k = i * 2;
			xi[i] = xr[k+1];
			xr[i] = xr[k];
		}
		for( i = 0; i <= npts/2; i++)
		{
			k = i * 2;
			xi[npts-i] = -xi[i];
			xr[npts-i] = xr[i];
		}
		
	}
	else
	{
		for( i = 0; i < npts; i++)  xi[i] = 0.0;
		
	}
   
  

   n1 = n/n2;                               /* do bit reversal permutation */
   for(k = 0; k < n1; ++k) {        /* This is algorithms 1.5.1 and 1.5.2. */
      j = 0; 
      m = k;
      p = 1;                               /* p = 2^q,  q used in the book */
      while(p < n1) {
         j = 2*j + (m&1);
         m >>= 1;
         p <<= 1;
      }
   
      
      assert(p == n1);                   /* make sure n1 is a power of two */
      if(j > k) {
         for(i = 0; i < n2; ++i) {                     /* swap k <-> j row */
            c = xr[k*n2+i];                              /* for all columns */
            xr[k*n2+i] = xr[j*n2+i];
            xr[j*n2+i] = c;

            c = xi[k*n2+i];                              /* for all columns */
            xi[k*n2+i] = xi[j*n2+i];
            xi[j*n2+i] = c;            
            
         }
      }
   }
                                              /* This is (3.1.7), page 124 */
      
					      
   p = 1;
   while(p < n1) {
      Ls = p;
      p <<= 1;
      jm = 0;                                                /* jm is j*n2 */
      dk = p*n2;
      for(j = 0; j < Ls; ++j) {
         wr = cos(M_PI*j/Ls);                   /* real and imaginary part */
         wi = -flag * sin(M_PI*j/Ls);                      /* of the omega */
         for(k = jm; k < n; k += dk) {                      /* "butterfly" */
            ks = k + Ls*n2;
            for(i = 0; i < n2; ++i) {                      /* for each row */
               m = k + i;
               ms = ks + i;
               tr =  wr*xr[ms] - wi*xi[ms];
               ti =  wr*xi[ms] + wi*xr[ms];
               xr[ms] = xr[m] - tr;
               xi[ms] = xi[m] - ti;
               xr[m] += tr;
               xi[m] += ti;
            }
         }
         jm += n2;
      } 
   }
   
   /* now combine the real/imaginary parts back into xr */

   	if( flag > 0)
	{
		for( i = npts/2; i >= 0; i--)
		{
			k = i * 2;
			xr[k] = xr[i];
			xr[k+1] = xi[i];
		}
	}		
	
	
	
	free(xi);
	   	
   
}
