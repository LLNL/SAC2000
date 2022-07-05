#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "coda.h"

int unary_op (float* data, int* npts, char* op, float arg, float* delta);
void C_overlap(float* input, int npts, float* output, float* c, int nc, int nfft, float* buffer,  float* cbuff);
void C_zshft(float* signal, int n, int ishft);



/*------------------------------------------------------------------------------ 
 * SUBROUTINE FIRTRN - Calculates Hilbert transform or derivative                
 *                     of a signal with an FIR filter.                           
 *                     Currently uses a 201 point filter constructed             
 *                     by windowing the ideal impulse response                   
 *                     with a hamming window.                                    
 *
 *    Author:  Dave Harris                                                       
 *
 *    Last Modified:  December 20, 1985                                          
 *
 *    Input arguments:                                                           
 *    ----------------                                                           
 *
 *      FTYPE      CHARACTER*(*) variable with type of transform desired         
 *                   'HILBERT' produces Hilbert transform                        
 *                   'DERIVATIVE' produces derivative                            
 *
 *      X          Real array containing unfiltered signal                       
 *
 *      N          Length of x and y                                             
 *
 *    Output arguments:                                                          
 *    -----------------                                                          
 *
 *      Y          Real array containing transformed signal.                     
 *                 May be the same array as x for in-place calculation.          
 *
 *    Ancillary arguments:                                                       
 *    --------------------                                                       
 *
 *      BUFFER     Work array of length at least 4297                            
 *
 *
 *    Linkage:  ZERO, OVERLP, ZSHFT
 *
 * */
void C_firtrn(char* ftype, float* x, int n, float* buffer, float* y)
{
	int i, i_, ihlfsz, iptrb1, iptrb2, iptrf, iqrtsz;
	float c, pi, twopi;

	float *const Buffer = &buffer[0] - 1;
	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;

        int nun;
        float delta = 0.0;


	pi = 3.14159265;
	twopi = 2.*pi;
	iptrf = 1;
	iptrb1 = 201 + 1;
	iptrb2 = iptrb1 + 2*1024;
	ihlfsz = 201/2;
	iqrtsz = 201/4;

	/* Set up filter coefficients                                                    
	 * */
        nun = 201;
	unary_op( &Buffer[iptrf], &nun, "zero", 0.0, &delta );
	if( memcmp(ftype,"HILBERT",7) == 0 ){

		for( i = 1; i <= iqrtsz; i++ ){
			i_ = i - 1;
			c = (2./(pi*(float)( 2*i - 1 )))*(.54 + .46*cos( twopi*
			 (float)( 2*i - 1 )/(float)( 201 ) ));
			Buffer[ihlfsz + 1 + (2*i - 1)] = c;
			Buffer[ihlfsz + 1 - (2*i - 1)] = -c;
			}

		}
	else if( memcmp(ftype,"DERIVATIVE",10) == 0 ){

		for( i = 1; i <= ihlfsz; i++ ){
			i_ = i - 1;
			c = (cos( pi*(float)( i ) )/(float)( i ))*(.54 + .46*cos( pi*
			 (float)( i )/(float)( ihlfsz ) ));
			Buffer[ihlfsz + 1 + i] = c;
			Buffer[ihlfsz + 1 - i] = -c;
			}

		}


	/* Filtering operation with overlap-save to implement transform                  
	 * */
	C_overlap( x, n, y, &Buffer[iptrf], 201, 1024, &Buffer[iptrb1], &Buffer[iptrb2] );

	/* Shift data to account for filtering delay                                     
	 * */
	C_zshft( y, n, -ihlfsz );

	/* Done                                                                          
	 * */
	return;
} /* end of function */

