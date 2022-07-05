#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"

/*  Copyright 1990  Regents of the University of California                      
 *
 *
 *  Author:  Dave Harris                                                         
 *
 *           Lawrence Livermore National Laboratory                              
 *           L-205                                                               
 *           P.O. Box 808                                                        
 *           Livermore, CA  94550                                                
 *           USA                                                                 
 *
 *           (415) 423-0617                                                      
 *                                                                  INSPECT      
 *
 *  Subroutine to calculate frequency responses of IIR analog and digital        
 *    filters. Choice of analog frequency interval for response calculation      
 *    is up to the user.  For digital filters, the interval is fixed:            
 *    0 to the folding frequency.                                                
 *
 *  Input Arguments:                                                             
 *  ----------------                                                             
 *
 *    IORD                Filter order (10 MAXIMUM)                              
 *
 *    TYPE                Character*2 variable containing filter type            
 *                          LOWPASS (LP)                                         
 *                          HIGHPASS (HP)                                        
 *                          BANDPASS (BP)                                        
 *                          BANDREJECT (BR)                                      
 *
 *    APROTO              Character*2 variable designating analog prototype      
 *                          Butterworth (BU)                                     
 *                          Bessel (BE)                                          
 *                          Chebyshev Type I (C1)                                
 *                          Chebyshev Type II (C2)                               
 *
 *    ATT                 Chebyshev II stopband attenuation factor               
 *
 *    TRBNDW              Transition bandwidth (fraction of passband width)      
 *
 *    FL                  Low-frequency cutoff                                   
 *
 *    FH                  High-frequency cutoff                                  
 *
 *    TS                  Sampling interval (in seconds)                         
 *
 *    RTYPE               Response type:                                         
 *                          'DAM' Digital amplitude                              
 *                          'DPH' Digital phase                                  
 *                          'DGD' Digital group delay                            
 *                          'AM'  Analog amplitude                               
 *                          'PH'  Analog phase                                   
 *                          'GD'  Analog group delay                             
 *
 *    NFREQS              Number of frequency samples                            
 *
 *    RFL                 Low sampling frequency, an input argument              
 *                          if RTYPE is analog                                   
 *
 *    RFH                 High sampling frequency, an input argument             
 *                          if RTYPE is analog                                   
 *
 *    SAMPLING           Frequency domain sampling (CHARACTER*8):                
 *                         'LINEAR'                                              
 *                         'LOG'                                                 
 *
 *
 *  Output Arguments:                                                            
 *  -----------------                                                            
 *
 *    RESPONSE            Array of frequency response samples                    
 *
 *    FREQS               Array of sampling frequencies                          
 *
 *
 *    RFL                 Low sampling frequency, an output argument             
 *                          if RTYPE is digital                                  
 *
 *    RFH                 High sampling frequency, an output argument            
 *                          if RTYPE is digital                                  
 *
 * */
void /*FUNCTION*/ inspect(iord, type, aproto, att, trbndw, fl, fh, ts, 
	 rtype, nfreqs, rfl, rfh, sampling, response, freqs)
int iord;
char *type, *aproto;
double att, trbndw, fl, fh, ts;
char *rtype;
int nfreqs;
float *rfl, *rfh;
char *sampling;
float response[], freqs[];
{
	char stype[10][4];
	int digital;
	int nsects;
	float dcvalue, eps, fhw, flw, omegar, ripple, sd[30], sn[30];
	complexf poles[10], zeros[10];
        char *strtemp;


	if( rtype[0] == 'D' )
	     digital = TRUE;
	else
	     digital = FALSE;

	/*  Analog prototype selection                                                   
	 * */
	if( memcmp(aproto,"BU",2) == 0 ){
	     buroots( poles, (char*)stype,4, &dcvalue, &nsects, iord );
	}
	else if( memcmp(aproto,"BE",2) == 0 ){
	     beroots( poles, (char*)stype,4, &dcvalue, &nsects, iord );
	}
	else if( memcmp(aproto,"C1",2) == 0 ){
	     chebparm( att, trbndw, iord, &eps, &ripple );
	     c1roots( poles, (char*)stype,4, &dcvalue, &nsects, iord, eps );
	}
	else if( memcmp(aproto,"C2",2) == 0 ){
	     omegar = 1. + trbndw;
	     c2roots( poles, zeros, (char*)stype,4, &dcvalue, &nsects, iord,
			att, omegar);
	}

	/*  Analog mapping selection
	 * */
	if( memcmp(type,"BP",2) == 0 ){
	    flw = warp( fl*ts/2., 2. );
	    fhw = warp( fh*ts/2., 2. );

	    if( digital ){
		lptbp( poles, zeros, (char*)stype,4, dcvalue, &nsects,
			flw, fhw, sn,sd);
	    }
	    else{
		lptbp( poles, zeros, (char*)stype,4, dcvalue, &nsects,
			fl, fh, sn, sd );
	    }

	}
	else if( memcmp(type,"BR",2) == 0 ){

	    flw = warp( fl*ts/2., 2. );
	    fhw = warp( fh*ts/2., 2. );

	    if( digital ){
		lptbr( poles, zeros, (char*)stype,4, dcvalue, &nsects,
			flw, fhw, sn,sd);
	    }
	    else{
		lptbr( poles, zeros, (char*)stype,4, dcvalue, &nsects,
			fl, fh, sn, sd );
	    }

	}
	else if( memcmp(type,"LP",2) == 0 ){

	    if( digital ){
		fhw = warp( fh*ts/2., 2. );
		lp( poles, zeros, (char*)stype,4, dcvalue, nsects, sn, sd );
		cutoffs( sn, sd, nsects, fhw );
	    }
	    else{
		lp( poles, zeros, (char*)stype,4, dcvalue, nsects, sn, sd );
		cutoffs( sn, sd, nsects, fh );
	    }

	}
	else if( memcmp(type,"HP",2) == 0 ){

	    if( digital ){
		flw = warp( fl*ts/2., 2. );
		lpthp( poles, zeros, (char*)stype,4, dcvalue, nsects, sn, sd );
		cutoffs( sn, sd, nsects, flw );
	    }
	    else{
		lpthp( poles, zeros, (char*)stype,4, dcvalue, nsects, sn, sd );
		cutoffs( sn, sd, nsects, fl );
	    }

	}

	/*  Response calculation                                                         
	 * */
	if( digital ){
	    bilin2( sn, sd, nsects );

            strtemp = malloc(3);
            strncpy(strtemp,rtype+1,2);
            strtemp[2] = '\0';

	    dfr( sn, sd, nsects, strtemp, nfreqs, ts, response );

            free(strtemp);

	    *rfl = 0.0;
	    *rfh = 1/(2.*ts);
	}
	else{
            strtemp = malloc(3);
            strncpy(strtemp,rtype,2);
            strtemp[2] = '\0';

	    afr( sn, sd, nsects, strtemp, sampling, *rfl, *rfh, 
	     nfreqs, response, freqs );

            free(strtemp);
	}

	return;
} /* end of function */

