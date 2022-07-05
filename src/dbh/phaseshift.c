#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*                                                             phaseshift        
 *                                                             90 degree phase sh
 *
 *  Coded by:   Dave Harris                                                      
 *  Based upon ``IIR Discrete-Time Hilbert Transformers'' by Rashid Ansari, ASSP,
 *             35(8), 1116-1119.                                                 
 *
 *
 *  From a single input sequence, produces two output sequences that are an appro
 *    Hilbert transform pair.  Used to form an approximate complex analytic repre
 *    of an allpass filtered version of the input.  Useful in spatial covariance 
 *    calculations.  Set up for continuous, buffered filtering operation.        
 *
 *    input arguments:                                                           
 *    ----------------                                                           
 *
 *          input                 initial (or next) input signal block           
 *
 *          nsamples              number of samples in block                     
 *
 *    output arguments:                                                          
 *    -----------------                                                          
 *
 *          output1               initial (or next) output signal block 1        
 *
 *          output2               initial (or next) output signal block 2        
 *
 *    auxiliary arguments:                                                       
 *    --------------------                                                       
 *
 *          states                internal states that must be saved to          
 *                                  perform buffered continuous filtering        
 *                                  initially set to zero                        
 *                                  must be dimensioned size 11                  
 * */
void /*FUNCTION*/ phaseshift(input, nsamples, output1, output2, states)
float input[];
int nsamples;
float output1[], output2[], states[];
{
	int i, i_;
	float p, p1, p2, q1, q2, w1, w2, x1, x2, x3, y, y1, y2;

	float *const Input = &input[0] - 1;
	float *const Output1 = &output1[0] - 1;
	float *const Output2 = &output2[0] - 1;
	float *const States = &states[0] - 1;




	/*  load internal states                                                         
	 * */
	x1 = States[1];
	x2 = States[2];
	x3 = States[3];
	y1 = States[4];
	y2 = States[5];
	w1 = States[6];
	w2 = States[7];
	p1 = States[8];
	p2 = States[9];
	q1 = States[10];
	q2 = States[11];

	for( i = 1; i <= nsamples; i++ ){
		i_ = i - 1;

		y = 0.94167*(y2 - x1) + x3;
		Output1[i] = 0.53239*(w2 - y) + y2;
		y2 = y1;
		y1 = y;
		w2 = w1;
		w1 = Output1[i];

		p = 0.186540*(p2 - Input[i]) + x2;
		Output2[i] = 0.7902015*(q2 - p) + p2;
		p2 = p1;
		p1 = p;
		q2 = q1;
		q1 = Output2[i];

		x3 = x2;
		x2 = x1;
		x1 = Input[i];

		}

	/*  save internal states                                                         
	 * */
	States[1] = x1;
	States[2] = x2;
	States[3] = x3;
	States[4] = y1;
	States[5] = y2;
	States[6] = w1;
	States[7] = w2;
	States[8] = p1;
	States[9] = p2;
	States[10] = q1;
	States[11] = q2;

	return;
} /* end of function */

