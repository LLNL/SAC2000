#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ inihdr()
{
	int j, j_;



	/* ind
	 *=====================================================================
	 * PURPOSE: Variable initialization of common block CMHDR.
	 *=====================================================================
	 * MODULE/LEVEL:  HDR/4
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *    NVHDRC:  Current header version number.
	 *    FUNDEF:  Value used to designate an "undefined" floating field.
	 *    IUNDEF:  Same for integer fields.
	 *    NUNDEF:  Same for fixed value fields.
	 *    KUNDEF:  Same for character variable fields.
	 *===================================================================== */
	/* PROCEDURE: */
	cmhdr.nvhdrc = MVHDRC;

	cmhdr.fundef = -12345.;
	cmhdr.iundef = -12345;
	cmhdr.nundef = -12345;
	strcpy( kmhdr.kundef, "-12345  " );

	for( j = 1; j <= MIV; j++ ){
		j_ = j - 1;
		Niv[j] = j;
		}

	cmhdr.linc = FALSE ;	/* lh starts without the INC option.  maf 961212 */
	cmhdr.llh  = FALSE ;	/* not currently executing xlh().  maf 961212 */

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    961212:  Added linc and llh as part off adding the INCLUSIVE 
	 *             option to the LISTHDR command.  maf
	 *    910820:  Added include file dfm and subscripted nlnhdr.
	 *    810414:  Original version.
	 *===================================================================== */

} /* end of function */


void /*FUNCTION*/ inihdr_()
{
	inihdr() ;
}
