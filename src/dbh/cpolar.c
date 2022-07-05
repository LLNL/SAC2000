#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

/*-------------------------------------------------------------------------------
 *                                                             CPOLAR            
 * CPOLAR - contour plotting program for data in polar coordinates               
 *
 * Author:  Dave Harris                                                          
 *
 * Last Modified:  March 28, 1986                                                
 *
 *
 * Usage:                                                                        
 *               CALL BEGINFRAME(NERR)                                           
 *               CALL SETVP( BEST_FIT )            BEST_FIT = 2                  
 *               CALL SETVPC( DESIRED SQUARE VIEWPORT )                          
 *                    .                                                          
 *                    .                                                          
 *                    .                                                          
 *               CALL CPOLAR(X, NRAD, NAZ, LEV, ZMIN, ZMAX)                      
 *                    .                                                          
 *                    .                                                          
 *                    .                                                          
 *
 *                 supplementary plotting code                                   
 *                    .                                                          
 *                    .                                                          
 *                    .                                                          
 *
 *               CALL ENDFRAME(NERR)                                             
 *
 *
 * Arguments:                                                                    
 * ----------                                                                    
 *
 *     X             2-D array of real data, stored radial sequential            
 *                     starting at zero azimuth, and zero radius.  The           
 *                     first radial line must be duplicated at the end:          
 *                     that is, there must be samples at 0 and 360               
 *     NRAD          Number of radial samples                                    
 *     NAZ           Number of azimuth samples                                   
 *     LEV           Number of contour levels                                    
 *     ZMIN          Minimum contour height                                      
 *     ZMAX          Maximum contour height                                      
 *
 *;;                                                                             
 *;;                                                                             
 * */
void /*FUNCTION*/ cpolar(x, nrad, naz, lev, zmin, zmax)
float x[];
int nrad, naz, lev;
double zmin, zmax;
{
	int swap;
	int i, i_, ihigh, ilow, inum, j, j_, k, k_, nazm1, nradm1;
	float angle, crners[2][4], dela, delr, delz, dmax, dmin, inter[2][4], 
	 lambda, pi, rho, temp, twopi, z[20], zf[4];


	float *const X = &x[0] - 1;
	float *const Z = &z[0] - 1;
	float *const Zf = &zf[0] - 1;



	pi = 3.14159265;
	twopi = 2.*pi;

	/* Initialize local world coordinates                                            
	 * */
	setworld( -1., 1., -1., 1. );

	/* Define contour interval and levels                                            
	 * */
	delz = (zmax - zmin)/(float)( lev - 1 );
	for( i = 1; i <= lev; i++ ){
		i_ = i - 1;
		Z[i] = zmin + (float)( i - 1 )*delz;
		/*              I                                                                 */
		}

	/* Set up scaling and offsets for plotting                                       
	 * */
	nradm1 = nrad - 1;
	nazm1 = naz - 1;
	delr = 0.8/(float)( nradm1 );
	dela = twopi/(float)( nazm1 );

	/* Draw a circle around plot                                                      */
	setcolor( 3 );

	worldsector( 0.0, 0.0, 0.8, 0., 360., 1. );

	/* Main plotting loop                                                            
	 * */
	setcolor( 1 );
	for( i = 1; i <= nazm1; i++ ){
		i_ = i - 1;

		for( j = 1; j <= nradm1; j++ ){
			j_ = j - 1;

			/* Define corner amplitudes of current plotting wedge                            
			 * */
			Zf[1] = X[j + (i - 1)*nrad];
			Zf[2] = X[j + 1 + (i - 1)*nrad];
			Zf[3] = X[j + 1 + i*nrad];
			Zf[4] = X[j + i*nrad];

			/* Determine if any contour intersects the wedge                                 
			 * */
                        dmax = fmax( Zf[1], Zf[2]);
                        dmax = fmax( dmax,  Zf[3]);
                        dmax = fmax( dmax,  Zf[4]);

                        dmin = fmin( Zf[1], Zf[2]);
                        dmin = fmin( dmin,  Zf[3]);
                        dmin = fmin( dmin,  Zf[4]);


			/* If no intersection continue on to next wedge                                  
			 * */
			if( (dmax < zmin) || (dmin > zmax) ){
				goto L_9999;
				}

			/* If all four corners of wedge have same value skip this wedge                  
			 * */
			if( dmax == dmin ){
				goto L_9999;
				}

			/* Determine number of intersecting contours                                     
			 * */
			dmin = fmax( zmin, dmin );
			dmax = fmin( zmax, dmax );
			ilow = (int)( (dmin - zmin)/delz ) + 1;
			ihigh = (int)( (dmax - zmin)/delz ) + 1;
			if( Z[ilow] < dmin ){
				ilow = ilow + 1;
				}
			if( ilow > ihigh ){
				goto L_9999;
				}

			/* Locate intersections of contours with wedge boundaries and plot them          
			 * */
			for( k = ilow; k <= ihigh; k++ ){
				k_ = k - 1;

				inum = 0;

				/*    Locate corners of wedge in cartesian coordinate system                     
				 * */
				rho = delr*(float)( j - 1 );
				angle = dela*(float)( i - 1 );
				/*                                                            X position          */
				crners[0][0] = rho*sin( angle );
				/*                                                            Y position          */
				crners[1][0] = rho*cos( angle );

				rho = delr*(float)( j );
				angle = dela*(float)( i - 1 );
				/*                                                            X position          */
				crners[0][1] = rho*sin( angle );
				/*                                                            Y position          */
				crners[1][1] = rho*cos( angle );

				rho = delr*(float)( j );
				angle = dela*(float)( i );
				/*                                                            X position          */
				crners[0][2] = rho*sin( angle );
				/*                                                            Y position          */
				crners[1][2] = rho*cos( angle );

				rho = delr*(float)( j - 1 );
				angle = dela*(float)( i );
				/*                                                            X position          */
				crners[0][3] = rho*sin( angle );
				/*                                                            Y position          */
				crners[1][3] = rho*cos( angle );

				/*    Find intersections, if any                                                 
				 * */
				inum = 0;

				/*      Left segment                                                             
				 * */
				if( !(Zf[1] == Zf[2]) ){
					lambda = (Z[k] - Zf[2])/(Zf[1] - Zf[2]);
					if( fabs( lambda - 0.5 ) <= 0.5 ){
						inum = 1;
						inter[0][0] = lambda*crners[0][0] + (1. - 
						 lambda)*crners[0][1];
						inter[1][0] = lambda*crners[1][0] + (1. - 
						 lambda)*crners[1][1];
						}
					}

				/*      Top segment                                                              
				 * */
				if( !(Zf[2] == Zf[3]) ){
					lambda = (Z[k] - Zf[3])/(Zf[2] - Zf[3]);
					if( fabs( lambda - 0.5 ) <= 0.5 ){
						inum = inum + 1;
						inter[0][inum - 1] = lambda*crners[0][1] + 
						 (1. - lambda)*crners[0][2];
						inter[1][inum - 1] = lambda*crners[1][1] + 
						 (1. - lambda)*crners[1][2];

						}
					}

				/*      Right segment                                                            
				 * */
				if( !(Zf[3] == Zf[4]) ){
					lambda = (Z[k] - Zf[4])/(Zf[3] - Zf[4]);
					if( fabs( lambda - 0.5 ) <= 0.5 ){
						inum = inum + 1;
						inter[0][inum - 1] = lambda*crners[0][2] + 
						 (1. - lambda)*crners[0][3];
						inter[1][inum - 1] = lambda*crners[1][2] + 
						 (1. - lambda)*crners[1][3];

						}
					}

				/*      Bottom segment                                                           
				 * */
				if( !(Zf[4] == Zf[1]) ){
					lambda = (Z[k] - Zf[1])/(Zf[4] - Zf[1]);
					if( fabs( lambda - 0.5 ) <= 0.5 ){
						inum = inum + 1;
						inter[0][inum - 1] = lambda*crners[0][3] + 
						 (1. - lambda)*crners[0][0];
						inter[1][inum - 1] = lambda*crners[1][3] + 
						 (1. - lambda)*crners[1][0];

						}
					}

				/* Plot contours                                                                 
				 * */
				if( inum == 2 ){

					worldmove( inter[0][0], inter[1][0] );
					worlddraw( inter[0][1], inter[1][1] );

					}
				else if( inum == 4 ){

					/*    Test to see if left intercept goes with top intercept.  If not, then it    
					 *      must go with bottom intercept.                                           
					 * */
					swap = TRUE;
					if( Z[k] < Zf[1] && Z[k] < Zf[3] ){
						swap = FALSE;
						}
					if( Z[k] > Zf[1] && Z[k] > Zf[3] ){
						swap = FALSE;
						}
					if( swap ){
						temp = inter[0][1];
						inter[0][1] = inter[0][3];
						inter[0][3] = temp;
						}

					worldmove( inter[0][0], inter[1][0] );
					worlddraw( inter[0][1], inter[1][1] );
					worldmove( inter[0][2], inter[1][2] );
					worlddraw( inter[0][3], inter[1][3] );

					}

				/*                K                                                               */
				}

L_9999:
			;

			/*              J                                                                 */
			}

		/*            I                                                                   */
		}

	/* Bye                                                                           
	 * */
	return;
} /* end of function */

