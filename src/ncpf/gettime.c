#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"

void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ gettime(kfunction, nc, ic, kvalue, kvalue_s, nerr)
char *kfunction;
int *nc, ic;
char *kvalue;   int kvalue_s;
int *nerr;
{
	int lexist, lmax, lmin, lvalue;
	int j, ndx1, ndx2, nlen, npt;
	float tvalue, value;

        char *s1;

        float *Sacmem1, *Sacmem2;
	/*=====================================================================
	 * PURPOSE: Returns the time offset in the file for the first occurence
	 *          of a given value, or the offset coresponding the the first
	 *          MAXIMUM or MINUMUM data value in the file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kfunction:  The full gettime command string [c].
	 *    nc:         Length of command line string [i].
	 *    ic:         Pointer to next token in command [i].
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kvalue:     Returned time offset, as a string.
	 *    nerr:       Nonzero if error occurs.
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    lmax:       True if looking for first value greater or equal [l].
	 *    lmin:       True if looking for first value less than or equal [l].
	 *    lvalue:     True if a target value is supplied [l].
	 *    tvalue:     Target value to search for.
	 *    value:      Located data point offset relative to begin time [f].
	 *    npt:        Point number where target value was located [n].
	 *    nlen:       Length of data file [i].
	 *    ndx1:       Pointer to first component [i].
	 *    ndx2:       Pointer to second component (unevenly spaced file) [i].
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    920325:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	*nerr = 0;
	npt = 0;
	lmax = FALSE;
	lmin = FALSE;
	lvalue = FALSE;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* Put the command onto the command stack, prepare for parsing. */
	modcase( TRUE, kfunction, *nc, kfunction);

        strncpy((s1=malloc(*nc-ic+2)),kfunction+ic - 1,*nc - ic + 1);
        s1[*nc - ic + 1] = '\0';
	pcmsg( s1, *nc - ic + 2, nerr );
	free(s1);

	if( *nerr != 0 )
	    goto L_8888;
	gc( &lexist, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* PARSING PHASE
	 * While more tokens in the line */

L_900:
	if( lcmore( nerr ) ){

	    /* - "MAX":  signifies getting the DEPMAX or first value greater than 
	     *            the specified value. */
	    if( lckey( "MAX$",5 ) && (!lmin) ){
		lmax = TRUE;
		goto L_900;
	    }

	    /* - "MIN":  signifies getting the DEPMIN or first value greater than 
	     *            the specified value. */
	    else if( lckey( "MIN$",5 ) && (!lmax) ){
		lmin = TRUE;
		goto L_900;
	    }

	    /* - Target value to search for. */
	    else if( lcchar( 32, kvalue,kvalue_s, nc ) ){
		lvalue = TRUE;
		cnvatf( kvalue,kvalue_s, &tvalue, 0, nerr );
					/* add 0. maf 970129 */
		*nc = indexb( kvalue,kvalue_s );
		if( *nerr != 0 )
		    goto L_8888;
	    }

	    /* - Parsing is complete, or something unexpected happened. */
	    else{
		*nerr = 1012;
		setmsg( "ERROR", *nerr );
                apcmsg2(kfunction,*nc);
		goto L_8888;
	    }
	} /* end if ( lcmore ( nerr ) ) */

	/* PROCEDURE: */

	/* - Get the first file from the memory manager */

	getfil( 1, TRUE, &nlen, &ndx1, &ndx2, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Looking for a specific value. */

	if( lvalue ){

	    /* -- Locate First MAX value greater than or equal to given value. */
	    if( lmax ){
		value = *depmin;
                Sacmem1 = cmmem.sacmem[ndx1];
                if (!*leven) Sacmem2 = cmmem.sacmem[ndx2];

		for( j = ndx1; j <= (ndx1 + nlen - 1); j++ ){
		    npt = npt + 1;
		    if( *(Sacmem1++) >= tvalue ){
			if( *leven ){
			    value = *b + (*delta*(npt - 1));
			}
			else{
			    value = *(Sacmem2+npt-1);
			}
			cnvfta( value, 16, 5, kvalue,kvalue_s );
			goto L_8888;
		    }
		} /* end for */
	    } /* end if ( lmax ) */

	    /* -- Locate First value less than or equal to given value. */
	    else{
		value = *depmax;
                Sacmem1 = cmmem.sacmem[ndx1];
                if (!*leven) Sacmem2 = cmmem.sacmem[ndx2];

		for( j = ndx1; j <= (ndx1 + nlen - 1); j++ ){
		    npt = npt + 1;
		    if( *(Sacmem1++) <= tvalue ){
			if( *leven ){
			    value = *b + (*delta*(npt - 1));
			}
			else{
			    value = *(Sacmem2+npt-1);
			}
			cnvfta( value, 16, 5, kvalue,kvalue_s );
			goto L_8888;
		    }
		} /* end for */
	    } /* end else associated with if ( lmax ) */
	} /* end if ( lvalue ) */

	/* - No supplied target value. */
	else{

	    /* -- Locate first DEPMAX point in file. */

	    if( lmax ){
		value = *depmin;
                Sacmem1 = cmmem.sacmem[ndx1];
                if (!*leven) Sacmem2 = cmmem.sacmem[ndx2];

		for( j = ndx1; j <= (ndx1 + nlen - 1); j++ ){
		    npt = npt + 1;
		    if( *(Sacmem1++) >= *depmax ){
			if( *leven ){
			    value = *b + (*delta*(npt - 1));
			}
			else{
			    value = *(Sacmem2+npt-1);
			}
			cnvfta( value, 16, 5, kvalue,kvalue_s );
			goto L_8888;
		    }
		} /* end for */
	    } /* end if ( lmax ) */

	    /* -- Locate first DEPMIN point in file. */
	    else{
		value = *depmax;
                Sacmem1 = cmmem.sacmem[ndx1];
                if (!*leven) Sacmem2 = cmmem.sacmem[ndx2];

		for( j = ndx1; j <= (ndx1 + nlen - 1); j++ ){
		    npt = npt + 1;
		    if( *(Sacmem1++) <= *depmin ){
			if( *leven ){
			    value = *b + (*delta*(npt - 1));
			}
			else{
			    value = *(Sacmem2+npt-1);
			}
			cnvfta( value, 16, 5, kvalue,kvalue_s );
			goto L_8888;
		    }
		} /* end for */
	    } /* end else associated with if ( lmax ) */
	} /* end else associated with if ( lvalue ) */

L_8888:
	return;

} /* end of function */

