#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  ZSHFT - Subroutine to shift a signal in-place with zero filling.
 *
 *  Author:  Dave Harris                                                         
 *
 *  Created:  November 17, 1981                                                  
 *
 *
 *  Input Arguments:                                                             
 *  ----- ----------                                                             
 *
 *    SIGNAL                Real array containing signal to be shifted.          
 *
 *    N                     Length of the signal.                                
 *
 *    ISHFT                 Integer number of samples to shift the signal.       
 *                            ISHFT > 0 implies a shift to the right.            
 *                            ISHFT < 0 implies a shift to the left.             
 *
 *  Linkage:  ZERO                                                               
 *
 * */
void /*FUNCTION*/ zshft(signal, n, ishft)
float signal[];
int n, ishft;
{
	int k, nhalt;

	float *const Signal = &signal[0] - 1;




	/*                                       Shift specified too large.               */
	if( abs( ishft ) > n ){

		fprintf( stdout, "*** ZSHFT - SPECIFIED SHIFT TOO LARGE ***\n" );

		/*                                     Left shift.                                */
		}
	else if( ishft < 0 ){

		/*    Shift array entries to the left.                                           
		 * */
		nhalt = n + ishft + 1;
		k = 1;
L_6:
		;
		if( k == nhalt )
			goto L_7;
		Signal[k] = Signal[k - ishft];
		k = k + 1;
		goto L_6;
L_7:
		;

		/*    Zero high end of array                                                     
		 * */
		zero( &Signal[n + ishft + 1], -ishft );

		}
	else if( ishft > 0 ){

		/*    Shift array entries to the right.                                          
		 * */
		k = n;
L_8:
		;
		if( k == ishft )
			goto L_9;
		Signal[k] = Signal[k - ishft];
		k = k - 1;
		goto L_8;
L_9:
		;

		/*    Zero low end of array.                                                     
		 * */
		zero( signal, ishft );

		}

	/*  Bye                                                                          
	 * */
	return;
} /* end of function */

