#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/wild.h"
int /*FUNCTION*/ lfilec(kentry, ndflou, kdflou, nerr)
char *kentry;
int ndflou;
char *kdflou;
int nerr;
{
	int lfilec_v;

	/*=====================================================================
	 * PURPOSE:  To check for and process a previously defined dataset.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KENTRY:  Entry to check. [c]
	 *    NDFLOU:  Number of entries in output file list. [i]
	 *    KDFLOU:  Output file list. [cl]
	 *=====================================================================
	 *    NDFLOU:  Number of entries in output file list. [i]
	 *    KDFLOU:  Output file list. [cl]
	 *=====================================================================
	 * FUNCTION VALUE:  .TRUE. if there is a dataset character
	 *                  in the entry, .FALSE. otherwise.
	 *=====================================================================
	 * MODULE/LEVEL:  WILD/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    86xxxx:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  86xxxx
	 *===================================================================== */
	/* PROCEDURE: */
	lfilec_v = FALSE;

L_8888:
	return( lfilec_v );

} /* end of function */

