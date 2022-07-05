#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ zdestf(kbase, kbase_s, nerr)
char *kbase;   int kbase_s;
int *nerr;
{
	char _c0[2], _c1[2], kname[MCPFN+1];
	int j, j_, jdig, jten, nbase;
	static byte kint[10]={'0','1','2','3','4','5','6','7','8','9'};

	byte *const Kint = &kint[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To destroy a family of file names given the base name.
	 *           A two digit integer is appended to the base name.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *     kbase:  Base name to use. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      nerr:  Error return flag. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  co/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    mcpfn
	 *=====================================================================
	 * NATURE OF DEPENDENT CODING:  None.  Here because it uses ZDEST.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860818:  Changed to new message system.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800906:  Fixed bug in determining correct base name.
	 *    800823:  Added tree name capability [Prime].
	 *             Allowed a lower as well as upper range to be specified.
	 *    800103:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860818
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Make sure basename is legitimate. */

	if( memcmp(kbase," ",1) == 0 ){
		*nerr = 914;
		setmsg( "ERROR", *nerr );
		apcmsg( kname,MCPFN+1 );
		goto L_8888;
		}
	else{

		/* - Determine number of characters in base name. */

		nbase = indexb( kbase,kbase_s );
		if( nbase <= 0 ){
			nbase = MCPFN - 2;
			}
		else if( nbase > MCPFN - 2 ){
			nbase = MCPFN - 2;
			}

		/* - Destroy files with basename staring at "01" until an error occurs.
		 *   ASSUME this error means that there are no more files with basename. */

		jten = 1;
		jdig = 2;
		for( j = 1; j <= 99; j++ ){
			j_ = j - 1;

			/* -- Create file name. */

                        memset(kname,(int)' ',MCPFN);
                        kname[MCPFN] = '\0';
                        memcpy(kname,kbase,nbase);
                        kname[nbase] = Kint[jten];
                        kname[nbase + 1] = Kint[jdig];

			/* -- Try to destroy it.  Return on error.
			 *    This should normally be "File does not exist." */
			zdest( kname,MCPFN+1, nerr );
			if( *nerr != 0 )
				goto L_8888;

			/* -- Increment numbers appended to basename. */
			if( jdig < 10 ){
				jdig = jdig + 1;
				}
			else{
				jten = jten + 1;
				jdig = 1;
				}
			}
		}

	/* - Clear error condition for "File does not exist." */

L_8888:
	if( *nerr == 108 ){
		*nerr = 0;
		clrmsg();
		}


	return;

} /* end of function */

