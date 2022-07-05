#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
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
void /*FUNCTION*/ overlp(input, npts, output, c, nc, nfft, buffer, 
	 cbuff)
float input[];
int npts;
float output[], c[];
int nc, nfft;
float buffer[], cbuff[];
{
	int i, i_, iptr, nbad, ngood, nload1, nload2, nrem;
	float ci, cr, scale, scale1, scale2, xi, xr;

	float *const Buffer = &buffer[0] - 1;
	float *const C = &c[0] - 1;
	float *const Cbuff = &cbuff[0] - 1;
	float *const Input = &input[0] - 1;
	float *const Output = &output[0] - 1;




	nrem = npts;
	nbad = nc - 1;
	ngood = nfft - nbad;
	iptr = 1;

	/*    DFT of filter sequence                                                     
	 * */
	zero( &Cbuff[1], 2*nfft );
	copy( (int*)&C[1], (int*)&Cbuff[1], nc );
	fft( &Cbuff[1], &Cbuff[nfft + 1], nfft, -1 );

	/*    Initial conditions in buffer                                               
	 * */
	zero( &Buffer[ngood + 1], nbad );

L_6:
	;
	if( nrem <= 0 )
		goto L_7;

	/*    Load data into buffer                                                      
	 * */
	nload2 = 0;
	nload1 = min( ngood, nrem );
	copy( (int*)&Input[iptr], (int*)&Buffer[1], nload1 );
	nrem = nrem - nload1;

	/*    Load second buffer if the available data is not exhausted                  
	 * */
	if( nrem > 0 ){

		nload2 = min( ngood, nrem );
		/*                                                              Data              */
		copy( (int*)&Input[iptr + nload1], (int*)&Buffer[nfft + 1], 
		 nload2 );
		/*                                                              Initial condition */
		copy( (int*)&Input[iptr + ngood - nbad], (int*)&Buffer[nfft + 1 + ngood], 
		 nbad );
		nrem = nrem - nload2;

		}
	else{

		zero( &Buffer[nfft + 1], nfft );

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
	fft( &Buffer[1], &Buffer[nfft + 1], nfft, -1 );

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
	fft( &Buffer[1], &Buffer[nfft + 1], nfft, 1 );

	/*    Load initial conditions from input sequence                                
	 * */
	if( !(nrem <= 0) ){
		copy( (int*)&Input[iptr + 2*ngood - nbad], (int*)&Buffer[ngood + 1], 
		 nbad );
		}

	/*    Save filtered data to output sequence                                      
	 * */
	if( !(nload2 == 0) ){

		for( i = 1; i <= nload1; i++ ){
			i_ = i - 1;
			Buffer[i] = Buffer[i]/scale;
			}

		}

	copy( (int*)&Buffer[1], (int*)&Output[iptr], nload1 );
	iptr = iptr + nload1;

	if( !(nload2 == 0) ){
		copy( (int*)&Buffer[nfft + 1], (int*)&Output[iptr], nload2 );
		iptr = iptr + nload2;
		}

	goto L_6;
L_7:
	;

	return;
} /* end of function */

