#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/bom.h"
void /*FUNCTION*/ inibom()
{



	/*=====================================================================
	 * PURPOSE: Variable initialization of common block CMBOM.
	 *=====================================================================
	 * PARAMETERS:
	 *   MBFL    [MDFL]     Maximum number of files in binop file list (bfl).
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *   NBFL    [0]        Number of files currently in bfl.
	 *   KBFL               Names of files in bfl.
	 *                      [char*32 array of length MBFL.]
	 *===================================================================== */
	/* PROCEDURE: */
	cmbom.nbfl = 0;

	strcpy( kmbom.kecnpt, "FATAL   " );
	strcpy( kmbom.kecdel, "FATAL   " );

	cmbom.ibflc = 0;
	cmbom.ndxhbf = 0;
	cmbom.ndx1bf = 0;
	cmbom.ndx2bf = 0;

	/* by default, take header info from original file. maf 990526 */
	cmbom.lnewhdr = FALSE ;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    850730:  Added initialization for IBFLC, NDXHBF, NDX1BF, NDX2BF.
	 *    810811:  Added initializaton for KECDEL.
	 *    810415:  Original version.
	 *===================================================================== */

} /* end of function */
/* MODULE/LEVEL:  BOM/4
 *=====================================================================
 * GLOBAL COUPLING:
 *===================================================================== */
