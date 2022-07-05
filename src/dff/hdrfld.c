#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ hdrfld(kname, kname_s, icat, item, lfound)
char *kname;   int kname_s;
int *icat, *item;
int *lfound;
{
	char ktemp[9];
	int nc;



	/*=====================================================================
	 * PURPOSE:  To determine the category (type) and index number of
	 *           a header variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of header field. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ICAT:    Category number of field. [i]
	 *             For example 1 means floating (FHDR array).
	 *    ITEM:    Index number within category.
	 *             For example 12 means variable stored in FHDR(12).
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870514:  Added convertion to upper case.
	 *    810000:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870514
	 *===================================================================== */
	/* PROCEDURE: */

        memset(ktemp,' ',8);
        ktemp[8] = '\0';

	/* - Convert input name to uppercase for testing. */
	nc = indexb( kname,kname_s );
	modcase( TRUE, kname, nc, ktemp );
	if( nc < MCPW )
		subscpy( ktemp, nc, MCPW - 1, 8, " " );

	/* - Assume the worst. */

	*lfound = FALSE;
	*icat = 0;

	/* - Check name versus list of variables in each category. */

	/* -- Floating point fields. */
	*item = nequal( ktemp, (char*)kmlhf.kfhdr,9, MFHDR );
	if( *item > 0 ){
		*lfound = TRUE;
		*icat = cmlhf.icatf;
		goto L_8888;
		}

	/* -- Integer fields. */
	*item = nequal( ktemp, (char*)kmlhf.knhdr,9, MNHDR );
	if( *item > 0 ){
		*lfound = TRUE;
		*icat = cmlhf.icatn;
		goto L_8888;
		}

	/* -- Enumerated fields. */
	*item = nequal( ktemp, (char*)kmlhf.kihdr,9, MIHDR );
	if( *item > 0 ){
		*lfound = TRUE;
		*icat = cmlhf.icati;
		goto L_8888;
		}

	/* -- Logical fields. */
	*item = nequal( ktemp, (char*)kmlhf.klhdr,9, MLHDR );
	if( *item > 0 ){
		*lfound = TRUE;
		*icat = cmlhf.icatl;
		goto L_8888;
		}

	/* -- Character fields. */
	*item = nequal( ktemp, (char*)kmlhf.kkhdr,9, MKHDR );
	if( *item > 0 ){
		*lfound = TRUE;
		*icat = cmlhf.icatk;
		goto L_8888;
		}

	/* -- Auxiliary fields. */
	*item = nequal( ktemp, (char*)kmlhf.kahdr,9, MAHDR );
	if( *item > 0 ){
		*lfound = TRUE;
		*icat = cmlhf.icata;
		goto L_8888;
		}

L_8888:
	return;

} /* end of function */

