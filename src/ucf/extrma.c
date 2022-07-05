#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"

#define MAX_DATA_VALUE 1.0e+36
void /*FUNCTION*/ extrma(array, incrmt, number, aminm, amaxm, amean)
float array[];
int incrmt, number;
float *aminm, *amaxm, *amean; {
    int _d_l, _d_m, _do0, _do1, j, j1, j2, j_;
    float aj;
    
    float *const Array = &array[0] - 1;
    
    
    /*=====================================================================
     * PURPOSE:  To calculate the extrema of an array.
     *           The minimum, maximum, and mean values are calculated.
     *=====================================================================
     * INPUT ARGUMENTS:
     *    ARRAY:   Data array. [r]
     *    INCRMT:  Increment in data array. [i]
     *             "2" means every other point is examined.
     *    NUMBER:  Number of elements in ARRAY to examine. [i]
     *=====================================================================
     * OUTPUT ARGUMENTS:
     *    AMINM:   Minimum value of ARRAY. [r]
     *    AMAXM:   Maximum value of ARRAY. [r]
     *    AMEAN:   Mean value of ARRAY. [r]
     *=====================================================================
     * GLOBAL INPUT:
     *    MACH:  VLARGE. [rp]
     *=====================================================================
     * MODULE/LEVEL:  SERVICE/4
     *=====================================================================
     * MODIFICATION HISTORY:
     *    920521:  Added test for data values outside storage range.
     *             This should be replaced by the fortran ieee err handler.
     *             Modified VLARGE in inc/mach to closer value for SUN SPARC1+
     *    830810:  Cleaned up and converted to independent subroutine.
     *    810000:  Original version.
     *===================================================================== */
    /* PROCEDURE: */
    /* - Initialize output values. */
    j = 1;
    *aminm = Array[j];
    *amaxm = Array[j];
    *amean = Array[j];
    
    /* - Loop through array looking for extrema. */
    
    j1 = incrmt + 1;
    j2 = incrmt*(number - 1) + 1;
    for( j = j1; j <= j2; j += incrmt ){
        j_ = j - 1;
        aj = Array[j];
        if( aj > MAX_DATA_VALUE){
            Array[j] = MAX_DATA_VALUE;
            aj = Array[j];
            printf( "Warning: Data value exceeded %e and was reset to %e\n", MAX_DATA_VALUE, MAX_DATA_VALUE );
        }
        if( aj < -MAX_DATA_VALUE ){
            Array[j] = -MAX_DATA_VALUE;
            aj = Array[j];
            printf( "Warning: Data value was less than %e and was reset to %e\n", -MAX_DATA_VALUE, -MAX_DATA_VALUE );
        }
        *amean = *amean + aj;
        
        if((aj - *aminm) < 0.0 ) goto L_600;
        if((aj - *amaxm) <= 0.0) goto L_1000;
        else  goto L_800;
        
        L_600:
            *aminm = aj;
            goto L_1000;
            L_800:
                *amaxm = aj;
                L_1000:
                    ;
    }
    
    /* - Compute mean value. */
    
    *amean = *amean/(max( number, 1 ));
    
    
    /* - Test to see if anything strange happened , like RQ command
     *   would do with inappropriate Q or C values.
     * */
    if( *aminm < -VLARGE || *amaxm > VLARGE ){
        setmsg( "WARNING", 0 );
        apcmsg( "Data value outside system storage bounds", 41 );
        aplmsg( "Maxvalue = ", 12 );
        apfmsg( *amaxm );
        apcmsg( " Minvalue = ", 13 );
        apfmsg( *aminm );
        outmsg();
        clrmsg();
    }
    
    L_8888:
        return;
        
} /* end of function */

