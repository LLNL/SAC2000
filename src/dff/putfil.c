#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void putfil(int idfl, int* nerr)
{
	int nlcmem;
	void zputc();


	/*=====================================================================
	 * PURPOSE:  To move current header from HDR common to working memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1310
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *     MACH: MCPW
	 *     DFM:  NDXHDR
	 *     HDR:  MCMHDR, MKHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *     MEM:  SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *     SACLIB: COPY, ZPUTC, SETMSG
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Define memory location for copy. */

	nlcmem = Ndxhdr[idfl];

	/* - Move header.  COPY copies all non-character variables and
	 *   ZPUTC copies the character variables. */

	if( nlcmem > 0 ){
		copy( (int*)&Fhdr[1], (int*)cmmem.sacmem[nlcmem], MCMHDR );
		zputc( kmhdr.khdr[0],9, cmmem.sacmem[nlcmem]+MCMHDR, (MCPW+1)* MKHDR );
	}
	else{
		*nerr = 1310;
		setmsg( "ERROR", *nerr );
		apimsg( idfl );
	}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    990416:  Removed existing putfil.c and renamed mvhdr.c to putfil.c
	 *             This putfil used to be mvhdr.  There is now no mvhdr.c maf
	 *    850415:  Changes due to restructuring of DFM common block.
	 *    820917:  Modification due to change in HDR common blocks.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850415
	 *===================================================================== */

} /* end of function */

