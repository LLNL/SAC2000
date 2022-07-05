#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
void /*FUNCTION*/ synch ( ndttmi , offsti , num , ndttmo , offsto , lbegin )
int ndttmi[][6];
float offsti[];
int num, ndttmo[][6];
float offsto[];
int lbegin ;
{
	int j, j_, jmx;
	float offstm;

	float *const Offsti = &offsti[0] - 1;

	float *const Offsto = &offsto[0] - 1;


	/*=====================================================================
	 * PURPOSE: To synchronize a set of times to the latest time.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    NDTTMI:  Current reference times. [fa: (6,NUM)]
	 *    OFFSTI:  Current beginning offsets. [fa: (NUM)]
	 *    NUM:     Number of entries. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NDTTMO:  New reference times. [f array: (6,NUM)]
	 *    OFFSTO:  New begining offsets. [fa: (NUM)]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  IDTTM, DDTTM
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Adjust input date time arrays (DTA) for input offsets. */
	for( j = 1; j <= num; j++ ){
	    j_ = j - 1;
	    idttm( &ndttmi[j_][0], Offsti[j], &ndttmo[j_][0] );
	}

	/* - Calculate the differences in DTA between the first and subsequent entries.
	 *   Store differences in output offset array. */
	/*    modified to allow BEGIN option.  maf 970908 */
	Offsto[1] = 0.;
	if ( lbegin ) {
	    for( j = 2; j <= num; j++ ){
		Offsto[j] = 0. ;
	    }
	}
	else {
	    for( j = 2; j <= num; j++ ){
		j_ = j - 1;
		ddttm( &ndttmo[j_][0], &ndttmo[0][0], &Offsto[j] );
	    }
	}
	/* end code modified maf 970908. */

	/* - Determine the largest difference. */

	jmx = 1;
	for( j = 2; j <= num; j++ ){
	    if( Offsto[j] > Offsto[jmx] )
		jmx = j;
	}
	offstm = Offsto[jmx];

	/* - All output DTA's have the DTA of the entry with the largest difference. */

	for( j = 1; j <= num; j++ ){
	    j_ = j - 1;
	    if( j != jmx )
		copyi( &ndttmo[jmx - 1][0], &ndttmo[j_][0], 6 );
	}

	/* - All output offsets are relative to this new output DTA. */

	for( j = 1; j <= num; j++ ){
	    Offsto[j] = Offsto[j] - offstm;
	}

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810624:  Original version.
	 *===================================================================== */

} /* end of function */

