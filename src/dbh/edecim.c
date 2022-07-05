#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  EDECIM -- SUBROUTINE:  EFFICIENT DECIMATOR
 *
 *  PURPOSE:  FILTER AND/OR DECIMATE DATA USING AN FIR FILTER
 *
 *  AUTHOR:  DAVE HARRIS
 *
 *  LAST MODIFIED:  NOVEMBER 17, 1980
 *
 *  ARGUMENTS:
 *  ----------
 *
 *    DATA                REAL ARRAY CONTAINING ORIGINAL SIGNAL
 *
 *    NDATA               NUMBER OF SAMPLES IN DATA
 *
 *    IRATE               INTEGER DECIMATION (RESAMPLING) RATE
 *                        IF IRATE = 1, THEN THE DATA IS FILTERED,
 *                        BUT NOT DECIMATED.
 *
 *    DDATA               REAL ARRAY CONTAINING FILTERED/DECIMATED DATA
 *
 *    NDDATA              NUMBER OF VALID SAMPLES IN DDATA
 *
 *    C                   ARRAY OF FIR FILTER COEFFICIENTS
 *
 *    NC                  NUMBER OF FILTER COEFFICIENTS
 *
 *    ISYM                SET TO 1 FOR AN EVENLY SYMMETRIC FILTER
 *                        SET TO -1 FOR AN ODDLY SYMMETRIC FILTER
 *
 *
 *  NOTE:  THE FILTER COEFFICIENT SEQUENCE IS ASSUMED TO BE SYMMETRIC,
 *    SO ONLY HALF (+1) OF THE COEFFICIENTS ARE ASSUMED TO BE STORED
 *    IN ARRAY C.  THE "SYMMETRY POINT" COEFFICIENT IS STORED IN C(1),
 *    AND THE COEFFICIENTS TO THE RIGHT OF THE SYMMETRY POINT ARE
 *    STORED IN ASCENDING ORDER IN C(2),...,C(NC).
 * */
void /*FUNCTION*/ edecim(data, ndata, irate, ddata, nddata, c, nc, 
	 isym)
float data[];
int ndata, irate;
float ddata[];
int *nddata;
float c[];
int nc, isym;
{
	int in, j, j_, k, ncm1, out;
	float add, temp;

	float *const C = &c[0] - 1;
	float *const Data = &data[0] - 1;
	float *const Ddata = &ddata[0] - 1;



	/*  CALCULATING NEW DATA LENGTH
	 * */
	*nddata = ndata/irate;

	/*  LOOP TO FILTER/DECIMATE DATA
	 *
	 *    INITIALIZE INPUT AND OUTPUT POINTERS
	 * */
	in = 1;
	out = 1;

	/*    CALCULATE OUTPUT POINTS UNTIL OUT EXCEEDS THE NUMBER OF NEW POINTS
	 * */
	ncm1 = nc - 1;
L_1:
	;
	if( out > *nddata )
		goto L_2;
	temp = C[1]*Data[in];
	for( j = 1; j <= ncm1; j++ ){
		j_ = j - 1;
		add = 0.;
		k = in + j;
		if( k <= ndata ){
			add = Data[k];
			}
		k = in - j;
		if( k > 0 ){
			add = add + Data[k]*isym;
			}
		temp = temp + C[j + 1]*add;
		}
L_4:
	;
	Ddata[out] = temp;

	/*    UPDATE DATA POINTERS
	 * */
	in = in + irate;
	out = out + 1;

	/*  DONE LOOP
	 * */
	goto L_1;
L_2:
	;

	/*  BYE
	 * */
	return;
} /* end of function */

