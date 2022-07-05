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
#include "../../inc/bom.h"
void /*FUNCTION*/ getbfl(ibfl, ldta, nlen, ndx1, ndx2, nerr)
int ibfl;
int ldta;
int *nlen, *ndx1, *ndx2, *nerr;
{
	int ic1, ic2;
        char *strtemp;



	/*=====================================================================
	 * PURPOSE:  To get a binop file from the memory manager.
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
	 *    980922:  Added parameter to call to rdsac to inhibit rdsac from
	 *             adding the filename to the list of filenames.  maf
	 *    880306:  Fixed bug when only headers were requested.
	 *    870811:  Fixed bug when only one file was in the bfl.
	 *    850730:  Changes due to new memory manager.
	 *             Changes in argument lists for RDSAC.
	 *    810224:  Fixed bug in computing memory index for binop file.
	 *    810130:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850730
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
		lnumcl( kmbom.kbfl,MAXCHARS, ibfl, &ic1, &ic2 );
                strtemp = malloc(ic2-ic1+2);
                strncpy(strtemp,kmbom.kbfl+ic1 - 1,ic2-ic1+1);
                strtemp[ic2-ic1+1] = '\0';
		rdsac( cmdfm.ndfl + 1, strtemp, ic2-ic1+2, FALSE, ldta, &cmbom.nlenbf,
		 &cmbom.ndxhbf, &cmbom.ndx1bf, &cmbom.ndx2bf, nerr );
                free(strtemp);
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

