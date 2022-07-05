#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/hdr.h"
void /*FUNCTION*/ newhdr()
{
	int jhdr ;
	/* - Initialize some common blocks if not already done. */
	if( cmhdr.fundef != -12345. ){
	    /*initsac();*/
	    initblkdata();
	    inihdr();
	    inilhf();
	    inimsg();
	}	

	ka[8]='\0';
	kcmpnm[8]='\0';
	kdatrd[8]='\0';
	kevnm[17]='\0';
	kf[8]='\0';
	khole[8]='\0';
	kinst[8]='\0';
	knetwk[8]='\0';
	ko[8]='\0';
	kstnm[8]='\0';
	kt0[8]='\0';
	kt1[8]='\0';
	kt2[8]='\0';
	kt3[8]='\0';
	kt4[8]='\0';
	kt5[8]='\0';
	kt6[8]='\0';
	kt7[8]='\0';
	kt8[8]='\0';
	kt9[8]='\0';
	kuser0[8]='\0';
	kuser1[8]='\0';
	kuser2[8]='\0';

	/*=====================================================================
	 * PURPOSE:  To prepare a new (default) header.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     FUNDEF, IUNDEF, NUNDEF, KUNDEF, NVHDRC, ITIME
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     All
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    961031:  ninf and nhst were changed to norid and nevid for
	 *             compatability with the CSS format.  maf 961031
	 *    870902:  Added calls to INILHF and INIMSG as part of initialization.
	 *    821001:  Added initialization of LCALDA.
	 *    811118:  Replaced FMTSAC with literal "2."
	 *             Replaced DFM insert with HDR insert.
	 *             Deleted call to INIHDR.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870902
	 *===================================================================== */
	/* PROCEDURE: */

	/* - Set ALL header fields to their "undefined" values. */

	/* -- Floating fields: */
	for( jhdr = 1; jhdr <= MFHDR; jhdr++ ){
	    Fhdr[jhdr] = cmhdr.fundef;
	}

	/* -- Integer fields: */
	for( jhdr = 1; jhdr <= MNHDR; jhdr++ ){
	    Nhdr[jhdr] = cmhdr.nundef;
	}

	/* -- Fixed value fields: */
	for( jhdr = 1; jhdr <= MIHDR; jhdr++ ){
	    Ihdr[jhdr] = cmhdr.iundef;
	}

	/* -- Character variable fields: */
	for( jhdr = 1; jhdr <= MKHDR; jhdr++ ){
	    strcpy( kmhdr.khdr[jhdr - 1], kmhdr.kundef );
	}

	/* -- Logical fields: */
	for( jhdr = 1; jhdr <= MLHDR; jhdr++ ){
	    Lhdr[jhdr] = FALSE;
	}

	/* - Now set specific fields to their default values. */

	*nvhdr = cmhdr.nvhdrc;
	*leven = TRUE;
	*lovrok = TRUE;
	*lcalda = TRUE;
	*iftype = *itime;

	return;

} /* end of function */



/* Wrapper to make the function more convenient for FORTRAN programmers. */

void /*FUNCTION*/ newhdr_ ()
{
	newhdr () ;
}
