#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/snf.h"
void /*FUNCTION*/ inisnf()
{

	/*=====================================================================
	 * PURPOSE: Variable initialization of common block CMSNF.
	 *=====================================================================
	 * MODULE/LEVEL:  SNF/4
	 *=====================================================================
	 * PARAMETERS:
	 *   MSN     [15]       Number of station names.
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *   KSNOLD  [below]    Old station names [char*4].
	 *   KSNNEW  [below]    New station names [char*4].
	 *===================================================================== */
	/* PROCEDURE: */
	strcpy( kmsnf.ksnold[0], "CL00" );
	strcpy( kmsnf.ksnold[1], "CL01" );
	strcpy( kmsnf.ksnold[2], "CL02" );
	strcpy( kmsnf.ksnold[3], "CL03" );
	strcpy( kmsnf.ksnold[4], "CL04" );
	strcpy( kmsnf.ksnold[5], "CL05" );
	strcpy( kmsnf.ksnold[6], "CL06" );
	strcpy( kmsnf.ksnold[7], "PIGS" );
	strcpy( kmsnf.ksnold[8], "LTFT" );
	strcpy( kmsnf.ksnold[9], "LWFT" );
	strcpy( kmsnf.ksnold[10], "LFRT" );
	strcpy( kmsnf.ksnold[11], "ELKO" );
	strcpy( kmsnf.ksnold[12], "KANA" );
	strcpy( kmsnf.ksnold[13], "LAND" );
	strcpy( kmsnf.ksnold[14], "MINA" );

	strcpy( kmsnf.ksnnew[0], "CDV " );
	strcpy( kmsnf.ksnnew[1], "CSA " );
	strcpy( kmsnf.ksnnew[2], "CMN " );
	strcpy( kmsnf.ksnnew[3], "CDA " );
	strcpy( kmsnf.ksnnew[4], "CPN " );
	strcpy( kmsnf.ksnnew[5], "CVL " );
	strcpy( kmsnf.ksnnew[6], "CPS " );
	strcpy( kmsnf.ksnnew[7], "CPS " );
	strcpy( kmsnf.ksnnew[8], "CTF " );
	strcpy( kmsnf.ksnnew[9], "CWG " );
	strcpy( kmsnf.ksnnew[10], "CFR " );
	strcpy( kmsnf.ksnnew[11], "ELK " );
	strcpy( kmsnf.ksnnew[12], "KNB " );
	strcpy( kmsnf.ksnnew[13], "LAC " );
	strcpy( kmsnf.ksnnew[14], "MNV " );

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810514:  Original version.
	 *===================================================================== */

} /* end of function */

