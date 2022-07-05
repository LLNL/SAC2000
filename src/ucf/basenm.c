#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ basenm(kbase, kbase_s, n1, n2, kdfcl, kdfcl_s, nerr)
char *kbase;   int kbase_s;
int n1, n2;
char *kdfcl;   int kdfcl_s;
int *nerr;
{
	char _c0[2], _c1[2], kname[MCPFN+1];
	int j, j_, jdig, jten, nbase;
	static byte kint[10]={'0','1','2','3','4','5','6','7','8','9'};

	byte *const Kint = &kint[0] - 1;



	/*=====================================================================
	 * PURPOSE:  To generate a family of file names from a given base name.
	 *           A two digit integer is appended to the base name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     KBASE:  Base name to use. [c]
	 *        N1:  Starting integer to append to base name [i: 0-99].
	 *        N2:  Ending integer to append to base name [i: N1-99].
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KDFCL:   Array to store output file names. [cl]
	 *      NERR:  Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPFN
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	if( memcmp(kbase," ",1) == 0 ){
		*nerr = 914;
		setmsg( "ERROR", *nerr );
		apcmsg( kbase,kbase_s );
		goto L_8888;
		}
	else if( (n1 > n2 || n1 < 0) || n2 > 99 ){
		*nerr = 915;
		setmsg( "ERROR", *nerr );
		apimsg( n1 );
		apimsg( n2 );
		goto L_8888;
		}
	else{
		nbase = indexb( kbase,kbase_s );
		if( nbase <= 0 ){
			nbase = MCPFN - 2;
			}
		else if( nbase > MCPFN - 2 ){
			nbase = MCPFN - 2;
			}
		jten = n1/10 + 1;
		jdig = n1 - 10*(n1/10) + 1;
		for( j = 1; j <= (n2 - n1 + 1); j++ ){
			j_ = j - 1;
                        fstrncpy( kname, MCPFN, kbase, nbase);
                        *(kname+nbase) = Kint[jten];
                        *(kname+nbase+1) = Kint[jdig];
			putcl( kdfcl,kdfcl_s, kname,MCPFN+1, nerr );
			if( *nerr != 0 )
				goto L_8888;
			if( jdig < 10 ){
				jdig = jdig + 1;
				}
			else{
				jten = jten + 1;
				jdig = 1;
				}
			}
		}

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    860917:  Changed output file storage to a character list.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800906:  Fixed bug in determining correct base name.
	 *    800823:  Added tree name capability [Prime].
	 *             Allowed a lower as well as upper range to be specified.
	 *    800103:  Original version.
	 *===================================================================== */

} /* end of function */

