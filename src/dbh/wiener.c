#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*  SUBROUTINE WIENER - PROGRAM TO DESIGN AND APPLY A PREDICTION
 *    ERROR FILTER TO A GIVEN SIGNAL
 *
 *  AUTHOR:  DAVE HARRIS
 *
 *  LAST MODIFIED:	APRIL 1, 1997
 *			MARCH 6, 1997
 *		  	JULY 23, 1996
 *			FEBRUARY 5, 1981
 *
 *
 *  INPUT ARGUMENTS:
 *  ----- ----------
 *
 *    DATA                   ARRAY CONTAINING INPUT DATA SEQUENCE
 *
 *    NSAMPS                 LENGTH OF SEQUENCE IN DATA, WHICH IS
 *                             ASSUMED TO START AT DATA(1)
 *
 *    START                  INTEGER VARIABLE CONTAINING THE STARTING
 *                             INDEX DEFINING THE WINDOW USED TO
 *                             ESTIMATE THE NOISE AUTOCORRELATION.
 *                             START MUST BE GREATER THAN ZERO
 *
 *    WLEN                   INTEGER VARIABLE CONTAINING THE LENGTH
 *                             OF THE WINDOW IN SAMPLES
 *
 *    NC                     THE NUMBER OF FILTER COEFFICIENTS
 *                             MAXIMUM:  100
 *
 *    LMU                    1 IF MU IS SET, ELSE 0
 *
 *    MU                     REAL*4 VARIABLE CONTAINING ADAPTATION
 *                             PARAMETER
 *
 *    LEPSILON               1 IF EPSILON IS SET, ELSE 0
 *
 *    EPSILON                LMU, LEPSILON, AND EPSILON ADDED 960723, maf
 *
 *
 *  OUTPUT ARGUMENTS:
 *  ------ ----------
 *
 *    FDATA                  ARRAY CONTAINING FILTERED OUTPUT
 *                             SEQUENCE, WHICH MAY BE THE SAME ARRAY
 *                             AS DATA.
 *
 *    IERR                   INTEGER ERROR CONDITION CODE
 *                             = 0    NO ERROR
 *                             = 1    NUMERICAL INSTABILITY IN
 *                                    LEVINSON RECURSION
 *
 *
 *  SUBPROGRAMS CALLED:  LEVIN, ZERO
 * */

/*FUNCTION*/	/* lmu, lepsilon, and epsilon added 960723, maf */
void wiener(data, nsamps, start, wlen, nc, lmu, mu, lepsilon, epsilon, fdata, ierr)
float data[];
int nsamps, start, wlen, nc, lmu, lepsilon;
double mu , epsilon ;
float fdata[];
int *ierr;
{
	int i, j, k, point;
	float	a[100],
		buffer[100],
		reflct[100],
		rho[100],
		originalRho;	/* added to allow variable epsilon.  maf 970306 */
	double e1, err ;	/* changed err from float to double, maf 960723 */

	float *const A = &a[0] - 1;
	float *const Buffer = &buffer[0] - 1;
	float *const Data = &data[0] - 1;
	float *const Fdata = &fdata[0] - 1;
	float *const Reflct = &reflct[0] - 1;
	float *const Rho = &rho[0] - 1;




	/*  ESTIMATE AUTOCORRELATION FUNCTION
	 * */
	zero( rho, nc );
	for( i = 1; i <= nc; i++ ){
	    for( j = 0; j <= (wlen - i); j++ ){
		k = j + start;

		if ( k > 0 && k + i - 1 < nsamps ) {	/* check array limits. maf 970401 */
			/* added casting operators, maf 960723 */
		    Rho[i] +=  (double) ( Data[k] ) * (double) ( Data[k + i - 1] ) ;
		}
	    } /* end for ( j ) */
	} /* end for ( i ) */

	/* IF EPSILON IS NOT SET, SET IT TO DEFAULT maf 960723 */
	if ( !lepsilon )
	    epsilon = 0.0 ;

	originalRho = Rho[1] ;	/* added to allow variable epsilon.  maf 970306 */

	do {	/* This do loop allows epsilon to vary from zero to 0.01,  maf 970306 */
	    /*  REGULARIZE DIAGONAL ELEMENT, 
	        RHO[1]==> RHO[1] * (1.0 + EPSILON) maf 960723 */
	    Rho[1] = (double) ( originalRho ) * ( 1.0 + epsilon ) ;

	    /*  CALCULATE PREDICTION FILTER COEFFICIENTS
	     * */
	    levin( rho, a, reflct, nc );

	    /*  CHECK NUMERICAL STABILITY OF LEVINSON RECURSION
	     * */
	    *ierr = 0;
	    for( i = 1; i <= (nc - 1); i++ ){
		if( fabs( Reflct[i] ) > .999 ){
		    *ierr = 1;
		    break ;
		}
	    }

	    if ( *ierr && !lepsilon ) {
		if ( epsilon == 0.0 )
		    epsilon = 0.00001 ;
		else
		    epsilon *= 10. ;

		/* Send a message about increasing epsilon unless epsilon is 
		   too big */
		if ( epsilon < 0.1 ) {
		    setmsg ( "WARNING", 1614 ) ;
		    apfmsg ( epsilon ) ;
		    outmsg () ;
		    clrmsg () ;
		}
	    }
	}while ( !lepsilon && *ierr && epsilon < 0.1 ) ;

	/*  FILTER DATA
	 *
	 *    INITIALIZE BUFFER
	 * */
	zero( buffer, nc );

	/*    INITIALIZE POINTER
	 * */
	point = 1;

	/*    LOOP
	 * */

	while( point <= nsamps ) {
	    /*    FETCH INPUT DATUM
	     * */
	    Buffer[1] = Data[point];

	    /*    CALCULATE NEW ERROR POINT
	     * */
	    e1 = err = Buffer[1];
	    for( i = 2; i <= nc; i++ ){ /* casting operators added. maf 960723 */
		e1 = e1 + (double) ( Buffer[i] ) * (double) ( A[i] ) ;
	    }

	    Fdata[point] = e1;

	    /*    UPDATE FILTER COEFFICIENTS
	     * */		 /* replaced !lmu with lmu maf 960801 */
	    if ( lmu ) {	/* if the user wants mu calculated */
		mu = 0.0 ;  /* figure it out. maf 960723 */
		for ( i = 1 ; i <= nc ; i++ ){
		    mu += fabs ( (double) Rho[i] ) ;
		} /* end for */

		mu = fabs( 1.95 / mu ) ;
	    } /* end if */
 
	    for( i = 2; i <= nc; i++ ){ /* casting operators added. maf 960723 */
		A[i] = (double) ( A[i] ) - mu * err * (double) ( Buffer[i] ) ;
	    }

	    /*    SHIFT BUFFER
	     * */
	    for( i = 2; i <= nc; i++ ){
		k = nc + 2 - i;
		Buffer[k] = Buffer[k - 1];
	    }

	    /*    UPDATE POINTER
	     * */
	    point = point + 1;
	} /* end while */

	/*  DONE
	 * */
	return;
} /* end of function */

