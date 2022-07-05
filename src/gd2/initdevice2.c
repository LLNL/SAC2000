#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ initdevice2()
{



	/*=====================================================================
	 * PURPOSE:  To initialize the graphics device 2 (SGF) common block.
	 *=====================================================================
	 * MODULE/LEVEL:  GD2/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    GD2:     All.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900310:  Changed plot size variables.
	 *    870929:  Deleted initialization of xw and yw.
	 *    861010:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870929
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Initialize common block. */
	/* -- Device name. */
	strcpy( kmgd2.kname2, "SGF     " );
	cmgd2.itype2 = 1;

	/* -- Frame directory, id, and number. */
	fstrncpy( kmgd2.kfdir, MCPFN, " ", 1);
	fstrncpy( kmgd2.kfdirStore, MCPFN, " ", 1);
	cmgd2.nfdir = 0;
	cmgd2.nfdirStore = 0 ;
	fstrncpy( kmgd2.kfnamb, MCPFN, "f", 1);
	cmgd2.nfnamb = 1;
	cmgd2.nfnum = 1;
	cmgd2.lfnum = FALSE;

	/* - Fixed plot size attributes. */

	strcpy( kmgd2.sizetype, "NORMAL  " );
	cmgd2.sizevalue = 10.0;
	cmgd2.encodesize = FALSE;

	cmgd2.lover = FALSE ;  /* TRUE overwrite SGF files */

	/* set print filename to "" */
	kmgd2.kfilename[ 0 ] = '\0' ;

L_8888:
	return;

} /* end of function */

