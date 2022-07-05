#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

void apcmsg2(char* kalpha, int kalpha_s);



void cnvfmt(char* kcard, int kcard_s, char* kfmt, int kfmt_s, int nentry, float* fentry, int* nerr)
{
	int j, nc;
	void *_p0;

	float *const Fentry = &fentry[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To convert a formatted alphanumeric data card into
	 *           a floating point array.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCARD:    Alphanumeric data card. [c]
	 *    KFMT:     Format statement to use in conversion. [c]
	 *    NENTRY:   Number of floating point fields to convert. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    FENTRY:  Array of floating point fields. [ra]
	 *    NERR:    Error return flag.  Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  SETMSG, APCMSG, APLMSG
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860910:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  860910
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine length of input card. */

	nc = indexb( kcard,kcard_s );

	/* - Decode card using format statement. */
/*
	ird_seqbeg(kcard, kcard_s - 1, kfmt);
	for( j = 1; j <= nentry; j++ ){
		readfmt( INTERNAL_UNIT, "%f ", &Fentry[j] );
		}
	if( UERR(INTERNAL_UNIT) > 0 )	goto L_2000;
*/
	/* - Decode card using FORTRAN style format statement. */
        /* - The assumption here is that a fairly limited formatting */
        /* - capability exists.  In this case we are only reading */
        /* - floating point numbers.  So format spec.s would be limited */
        /* - to things such as (4x,f9.5,g11.2,4e11.2).  On read, there */
        /* - should be no need to have literal strings in the format */
        /* - spec., and the x specifier may be used to space over entries */
        /* - to get fields of interest.                            */

/*
        readformat(kcard,nc,kfmt,fentry,nentry,nerr);
        if(*nerr != 0) goto L_2000;
        else goto L_8888;

*/
        printf("formatted reads are not supported in SAC2000 yet\n");
	goto L_8888;

	/* - Come to here if an error occurs during decode. */

L_2000:
	*nerr = 1357;
	setmsg( "ERROR", *nerr );
	aplmsg( "Format statement =",19 );
	apcmsg( kfmt,kfmt_s );
	aplmsg( "Data card =",12 );
	apcmsg( "\"",2 );
        apcmsg2(kcard,nc);
	apcmsg( "\"",2 );

L_8888:

	return;

} /* end of function */

