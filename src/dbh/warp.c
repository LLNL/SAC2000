#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

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
 *
 * WARP -- FUNCTION, APPLIES TANGENT FREQUENCY WARPING TO COMPENSATE             
 *         FOR BILINEAR ANALOG -> DIGITAL TRANSFORMATION                         
 *
 * ARGUMENTS:                                                                    
 * ----------                                                                    
 *
 *      F       ORIGINAL DESIGN FREQUENCY SPECIFICATION (HERTZ)                  
 *      TS      SAMPLING INTERVAL (SECONDS)                                      
 *
 *  LAST MODIFIED:  SEPTEMBER 20, 1990                                           
 * */
double /*FUNCTION*/ warp(f, ts)
double f, ts;
{
	float angle, twopi, warp_v;


	twopi = 6.2831853;
	angle = twopi*f*ts/2.;
	warp_v = 2.*tan( angle )/ts;
	warp_v = warp_v/twopi;

	return( warp_v );
} /* end of function */

