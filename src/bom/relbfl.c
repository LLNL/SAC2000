#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/bom.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ relbfl(nerr)
int *nerr;
{
	int jcomp, jcomp_;



	/*=====================================================================
	 * PURPOSE:  To release the current binop file from memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  BOM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    BOM:     IBFLC, NDXHBF, NDX1BF, NDX2BF
	 *    DFM:     NDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    BOM:     IBFLC, NDXHBF, NDX1BF, NDX2BF
	 *    MEM:     SACMEM
	 *    DFM:     NDXHDR, NDXDTA, NLNDTA, NCOMP
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  RELAMB
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880308:  Was not clearing the DFM array variables properly.
	 *    880306:  Fixed bug that was not releasing header block.
	 *    850730:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850730
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Release blocks if they are in memory: */

	/* -- Header block. */
	if( cmbom.ndxhbf > 0 )
		relamb( cmmem.sacmem, cmbom.ndxhbf, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* -- First data component. */
	if( cmbom.ndx1bf > 0 )
		relamb( cmmem.sacmem, cmbom.ndx1bf, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* -- Second data component if any. */
	if( cmbom.ndx2bf > 0 )
		relamb( cmmem.sacmem, cmbom.ndx2bf, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Zero DFM pointers. (Binary file is stored at end of data file list.) */

	Ndxhdr[cmdfm.ndfl + 1] = 0;
	Nlndta[cmdfm.ndfl + 1] = 0;
	for( jcomp = 1; jcomp <= Ncomp[cmdfm.ndfl + 1]; jcomp++ ){
		jcomp_ = jcomp - 1;
		cmdfm.ndxdta[cmdfm.ndfl][jcomp_] = 0;
		}
	Ncomp[cmdfm.ndfl + 1] = 0;

	/* - Zero BOM pointers. */

	cmbom.ibflc = 0;
	cmbom.ndxhbf = 0;
	cmbom.ndx1bf = 0;
	cmbom.ndx2bf = 0;

L_8888:
	return;

} /* end of function */

