#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "coda.h"
/* XAPIIR -- SUBROUTINE:   IIR FILTER DESIGN AND IMPLEMENTATION                  
 *
 *  AUTHOR:  Dave Harris                                                         
 *
 *  LAST MODIFIED:  September 12, 1990                                           
 *
 *  ARGUMENTS:                                                                   
 *  ----------                                                                   
 *
 *    DATA           REAL ARRAY CONTAINING SEQUENCE TO BE FILTERED               
 *                     ORIGINAL DATA DESTROYED, REPLACED BY FILTERED DATA        
 *
 *    NSAMPS         NUMBER OF SAMPLES IN DATA                                   
 *
 *
 *    APROTO         CHARACTER*8 VARIABLE, CONTAINS TYPE OF ANALOG               
 *                     PROTOTYPE FILTER                                          
 *                     '(BU)TTER  ' -- BUTTERWORTH FILTER                        
 *                     '(BE)SSEL  ' -- BESSEL FILTER                             
 *                     'C1      ' -- CHEBYSHEV TYPE I                            
 *                     'C2      ' -- CHEBYSHEV TYPE II                           
 *
 *    TRBNDW         TRANSITION BANDWIDTH AS FRACTION OF LOWPASS                 
 *                   PROTOTYPE FILTER CUTOFF FREQUENCY.  USED                    
 *                   ONLY BY CHEBYSHEV FILTERS.                                  
 *
 *    A              ATTENUATION FACTOR.  EQUALS AMPLITUDE                       
 *                   REACHED AT STOPBAND EDGE.  USED ONLY BY                     
 *                   CHEBYSHEV FILTERS.                                          
 *
 *    IORD           ORDER (#POLES) OF ANALOG PROTOTYPE                          
 *                   NOT TO EXCEED 10 IN THIS CONFIGURATION.  4 - 5              
 *                   SHOULD BE AMPLE.                                            
 *
 *    TYPE           CHARACTER*8 VARIABLE CONTAINING FILTER TYPE                 
 *                     'LP' -- LOW PASS                                          
 *                     'HP' -- HIGH PASS                                         
 *                     'BP' -- BAND PASS                                         
 *                     'BR' -- BAND REJECT                                       
 *
 *    FLO            LOW FREQUENCY CUTOFF OF FILTER (HERTZ)                      
 *                   IGNORED IF TYPE = 'LP'                                      
 *
 *    FHI            HIGH FREQUENCY CUTOFF OF FILTER (HERTZ)                     
 *                   IGNORED IF TYPE = 'HP'                                      
 *
 *    TS             SAMPLING INTERVAL (SECONDS)                                 
 *
 *    PASSES           INTEGER VARIABLE CONTAINING THE NUMBER OF PASSES          
 *                   1 -- FORWARD FILTERING ONLY                                 
 *                   2 -- FORWARD AND REVERSE (I.E. ZERO PHASE) FILTERING        
 *
 *
 *  SUBPROGRAMS REFERENCED:  BILIN2, BUROOTS, WARP, CUTOFFS, LPTHP, LPTBP,       
 *    LP, LPTBR, BEROOTS, C1ROOTS, C2ROOTS, CHEBPARM, DESIGN, APPLY              
 * */
#define FALSE 0
#define TRUE  1

void C_design(int iord, char* type, char* aproto, double a, double trbndw, double fl, double fh, double ts,  float* sn, float* sd, int* nsects);
void C_apply(float* data, int nsamps, int zp, float* sn, float* sd, int nsects);


void C_xapiir(float* data, int nsamps, char* aproto, double trbndw, double a, int iord, char* type,  double flo, double fhi, double ts, int passes)
{
	int zp;
	int nsects;
	float sd[30], sn[30];
        char strtemp1[3], strtemp2[3];


	float *const Data = &data[0] - 1;
	float *const Sd = &sd[0] - 1;
	float *const Sn = &sn[0] - 1;





	/*  Filter designed                                                              
	 * */
        strncpy(strtemp1,type,2);
        strtemp1[2] = '\0';
        strncpy(strtemp2,aproto,2);
        strtemp2[2] = '\0';

	C_design( iord, strtemp1, strtemp2, a, trbndw, flo, 
	 fhi, ts, sn, sd, &nsects );

	/*  Filter data                                                                  
	 * */
	if( passes == 1 ){
		zp = FALSE;
		}
	else{
		zp = TRUE;
		}
	C_apply( data, nsamps, zp, sn, sd, nsects );

	return;
} /* end of function */

