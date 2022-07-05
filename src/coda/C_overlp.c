#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "coda.h"
/*                                                            OVERLP             
 *
 *  Simplified overlap-save routine, for general filter sequence and             
 *  arbitrary input sequence.                                                    
 *
 *  Author:  Dave Harris                                                         
 *
 *  Last Modified:  December 11, 1985                                            
 *
 *  Created:  December 11, 1985                                                  
 *
 *
 *  Input variables:                                                             
 *  ----------------                                                             
 *
 *      INPUT              REAL*4 array containing input sequence                
 *
 *      NPTS               Integer size of input sequence                        
 *
 *      C                  REAL*4 array containing coefficient sequence of       
 *                         filter                                                
 *
 *      NC                 Integer number of filter coefficients                 
 *
 *      FFT_SIZE           Integer size of FFT used to perform convolutions      
 *
 *  Output variables:                                                            
 *  -----------------                                                            
 *
 *      OUTPUT             REAL*4 array containing filtered output sequence.     
 *                         May be the same array as INPUT, for in-place          
 *                         filtering.                                            
 *
 *  Ancillary variables:                                                         
 *  --------------------                                                         
 *
 *      BUFFER             Temporary storage.  REAL*4 array of size at least     
 *                         2*FFT_SIZE                                            
 *
 *      CBUFFER            Another REAL*4 array for temporary storage of         
 *                         the filter transform.  Also must be of size at least  
 *                         2*FFT_SIZE                                            
 * */
 
void  C_fft(float* xreal, float* ximag, int n, int idir); 
int unary_op (float* data, int* npts, char* op, float arg, float* delta); 
int binary_op (float* data1, float* data2, int n, char* op); 
 
 
void C_overlap(float* input, int npts, float* output, float* c, int nc, int nfft, float* buffer,  float* cbuff)
{
	int i, i_, iptr, nbad, ngood, nload1 = 0, nload2, nrem;
	float ci, cr, scale, scale1, scale2, xi, xr;

	float *const Buffer = &buffer[0] - 1;
	float *const C = &c[0] - 1;
	float *const Cbuff = &cbuff[0] - 1;
	float *const Input = &input[0] - 1;
	float *const Output = &output[0] - 1;

        int nun;
        float delta = 0.0;


	nrem = npts;
	nbad = nc - 1;
	ngood = nfft - nbad;
	iptr = 1;

	/*    DFT of filter sequence                                                     
	 * */
        nun = 2*nfft;
	unary_op( &Cbuff[1], &nun, "zero", 0.0, &delta );
	binary_op( &C[1], &Cbuff[1], nc, "copy" );
	C_fft( &Cbuff[1], &Cbuff[nfft + 1], nfft, -1 );

	/*    Initial conditions in buffer                                               
	 * */
	unary_op( &Buffer[ngood + 1], &nbad, "zero", 0.0, &delta );

L_6:
	;
	if( nrem <= 0 )
		goto L_7;

	/*    Load data into buffer                                                      
	 * */
	nload2 = 0;
        if(ngood < nrem) nload1 = ngood;
        if(ngood >= nrem) nload1 = nrem;
	binary_op(&Input[iptr], &Buffer[1], nload1, "copy" );
	nrem = nrem - nload1;

	/*    Load second buffer if the available data is not exhausted                  
	 * */
	if( nrem > 0 ){
        	if(ngood < nrem) nload2 = ngood;
	        if(ngood >= nrem) nload2 = nrem;
		/*                                                              Data              */
		binary_op( &Input[iptr + nload1], &Buffer[nfft + 1], nload2, "copy" );
		/*                                                              Initial condition */
		binary_op( &Input[iptr + ngood - nbad], &Buffer[nfft + 1 + ngood],  nbad, "copy" );
		nrem = nrem - nload2;

		}
	else{

		unary_op( &Buffer[nfft + 1], &nfft, "zero", 0.0, &delta );

		}

	/*    Scale buffers when both contain data                                       
	 * */
	if( !(nload2 == 0) ){

		scale1 = 0.;
		scale2 = 0.;
		for( i = 1; i <= nfft; i++ ){
			i_ = i - 1;
			scale1 = scale1 + fabs( Buffer[i] );
			scale2 = scale2 + fabs( Buffer[nfft + i] );
			}
		if( scale1 == 0. ){
			scale1 = 1.;
			}
		scale = scale2/scale1;
		if( scale == 0. ){
			scale = 1.;
			}
		for( i = 1; i <= nfft; i++ ){
			i_ = i - 1;
			Buffer[i] = Buffer[i]*scale;
			}

		}

	/*    Transform data                                                             
	 * */
	C_fft( &Buffer[1], &Buffer[nfft + 1], nfft, -1 );

	/*    Product of data transform with filter transform                            
	 * */
	for( i = 1; i <= nfft; i++ ){
		i_ = i - 1;
		xr = Buffer[i];
		xi = Buffer[nfft + i];
		cr = Cbuff[i];
		ci = Cbuff[nfft + i];
		Buffer[i] = xr*cr - xi*ci;
		Buffer[nfft + i] = xr*ci + xi*cr;
		}

	/*    Inverse transform                                                          
	 * */
	C_fft( &Buffer[1], &Buffer[nfft + 1], nfft, 1 );

	/*    Load initial conditions from input sequence                                
	 * */
	if( !(nrem <= 0) ){
		binary_op( &Input[iptr + 2*ngood - nbad], &Buffer[ngood + 1],  nbad, "copy" );
		}

	/*    Save filtered data to output sequence                                      
	 * */
	if( !(nload2 == 0) ){

		for( i = 1; i <= nload1; i++ ){
			i_ = i - 1;
			Buffer[i] = Buffer[i]/scale;
			}

		}

	binary_op(&Buffer[1], &Output[iptr], nload1, "copy" );
	iptr = iptr + nload1;

	if( !(nload2 == 0) ){
		binary_op(&Buffer[nfft + 1], &Output[iptr], nload2, "copy" );
		iptr = iptr + nload2;
		}

	goto L_6;
L_7:
	;

	return;
} /* end of function */

