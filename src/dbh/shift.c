#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  SHIFT - Subroutine to shift a sequence.
 *          Circular and linear (with zero filling) shifts
 *          are implemented.
 *
 *  Author:  Dave Harris
 *
 *  Last Modified:  January 3, 1985
 *
 *
 *  Arguments:
 *  ----------
 *
 *    X            Array containing the sequence to be shifted.
 *
 *    N            Length of the sequence.
 *
 *    ISHFT        Number of samples to shift.
 *
 *                   A negative number indicates a shift left.
 *                   A positive number indicates a shift right.
 *                   A zero indicates no shift.
 *
 *    TYPE         Character*8 variable indicating which type
 *                   of shift.
 *
 *                   'C.......'    Circular shift
 *                   'L.......'    Linear shift with zero filling.
 *
 *  Output Arguments
 *  -----------------
 *
 *    Y            Array containing shifted sequence.
 *                   May not be the same array as X.
 *
 *  ERROR_MESSAGE  Character*130 variable containing error message
 *                 when error detected, ' ' when no error.
 *
 *  Linkage: (none)
 *
 * */
void /*FUNCTION*/ shift(x, n, ishft, type, y, errmsg, errmsg_s)
float x[];
int n, ishft;
char *type;
float y[];
char *errmsg;   int errmsg_s;
{
	int i, i_, is, m;

	float *const X = &x[0] - 1;
	float *const Y = &y[0] - 1;




	/*  Initializations
	 * */
        fstrncpy(errmsg, errmsg_s-1, " ", 1);

	/*  Error checking
	 *
	 *                                         Shift too large */
	if( abs( ishft ) >= n ){

                fstrncpy(errmsg, errmsg_s-1,
                  " SHIFT *** shift larger than data record *** ", 45);

		}
	else if( type[0] != 'C' && type[0] != 'L' ){

                fstrncpy(errmsg, errmsg_s-1,
                  " SHIFT *** illegal shift type *** ", 34);

		/*                                         Everything OK */
		}
	else{

		/*                                          Shift right */
		if( ishft >= 0 ){

			m = n - ishft;
			for( i = 1; i <= m; i++ ){
				i_ = i - 1;
				Y[i + ishft] = X[i];
				}
			/*                                      Circular shift */
			if( type[0] == 'C' ){
				for( i = 1; i <= ishft; i++ ){
					i_ = i - 1;
					Y[i] = X[n - ishft + i];
					}
				/*                                       Linear shift (zero filling) */
				}
			else{
				for( i = 1; i <= ishft; i++ ){
					i_ = i - 1;
					Y[i] = 0.;
					}
				}

			/*                                         Shift left */
			}
		else if( ishft < 0 ){

			is = -ishft;
			m = n - is;
			for( i = 1; i <= m; i++ ){
				i_ = i - 1;
				Y[i] = X[i + is];
				}
			/*                                       Circular shift */
			if( type[0] == 'C' ){
				for( i = 1; i <= is; i++ ){
					i_ = i - 1;
					Y[n - is + i] = X[i];
					}
				/*                                        Linear shift (zero filling) */
				}
			else{
				for( i = 1; i <= is; i++ ){
					i_ = i - 1;
					Y[n - is + i] = 0.;
					}
				}

			}

		}

	/*  Bye
	 * */
	return;
} /* end of function */

