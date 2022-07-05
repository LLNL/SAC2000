#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/bom.h"
void /*FUNCTION*/ getcfl(wfdiscFileName, wFileName, ibfl, nlen, ndx1, ndx2, nerr)
char * wfdiscFileName , wFileName ;
int ibfl ;
int *nlen, *ndx1, *ndx2, *nerr;
{
	int ic1, ic2;
        char *strtemp;



	/*=====================================================================
	 * PURPOSE:  To get a CSS flat file from disk for merge.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IBFL:    Binop file index number. [i]
	 *    LDTA:    Set to .TRUE. if header and data is requested. [l]
	 *             Set to .FALSE. if only header is requested.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NLEN:    Length of each data component. [i]
	 *    NDX1:    Index in SACMEM array of first data component. [i]
	 *    NDX2:    Index of second data component, if any. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1804
	 *=====================================================================
	 * NOTE:       NLEN, NDX1, and NDX2 are set to zero if LDTA if .FALSE.
	 *             or NERR is nonzero.
	 *=====================================================================
	 * MODULE/LEVEL:  BOM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    BOM:     IBFLC, KBFL
	 *    DFM:     NDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    BOM:     IBFLC, NLENBF, NDXHBF, NDX1BF, NDX2BF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  RELBFL, RDSAC, GTOUTM
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970616:  Original version. plagerized from getbfl  maf
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Make sure a valid binop file entry has been requested. */

	if( ibfl <= 0 || ibfl > cmbom.nbfl ){
		*nerr = 1804;
		setmsg( "ERROR", *nerr );
		apimsg( ibfl );
		*nlen = 0;
		*ndx1 = 0;
		*ndx2 = 0;
	}
 
	/* - Only read if binop file if it is different from current one. */
	else if( ibfl != cmbom.ibflc ){

		/* -- Release memory blocks for previous file. */
		relbfl( nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Read data file into memory.
		 *    (Use DFM index NDFL+1 to temporarily store pointers during read.) */
		rdcss( cmdfm.ndfl + 1, wfdiscFilename, wFileName, &cmbom.nlenbf, &cmbom.ndxhbf, 
		 &cmbom.ndx1bf, &cmbom.ndx2bf, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Save current binary file list number. */
		cmbom.ibflc = ibfl;

		/* -- Return pointers to header and data. */
		*nlen = cmbom.nlenbf;
		*ndx1 = cmbom.ndx1bf;
		*ndx2 = cmbom.ndx2bf;
	}

	/* - If same file as last time, simply return the pointers. */
	else{
		*nlen = cmbom.nlenbf;
		*ndx1 = cmbom.ndx1bf;
		*ndx2 = cmbom.ndx2bf;
	}

L_8888:
	return;

} /* end of function */

