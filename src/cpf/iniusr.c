#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/usr.h"
void /*FUNCTION*/ iniusr()
{
	int j, j_;



	/*=====================================================================
	 * PURPOSE: Variable initialization of common block CMUSR.
	 *=====================================================================
	 * PARAMETERS:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *=====================================================================
	 * VARIABLE DEFINITIONS FOR:
	 *===================================================================== */
	/* PROCEDURE: */
	cmusr.nusr = 0;
	strcpy( kmusr.kusr[0], "V1      " );
	strcpy( kmusr.kusr[1], "V2      " );
	strcpy( kmusr.kusr[2], "V3      " );
	strcpy( kmusr.kusr[3], "V4      " );
	strcpy( kmusr.kusr[4], "V5      " );
	strcpy( kmusr.kusr[5], "V6      " );
	strcpy( kmusr.kusr[6], "V7      " );
	strcpy( kmusr.kusr[7], "V8      " );
	strcpy( kmusr.kusr[8], "V9      " );
	strcpy( kmusr.kusr[9], "V10     " );
	strcpy( kmusr.kusr[10], "V11     " );
	strcpy( kmusr.kusr[11], "V12     " );
	strcpy( kmusr.kusr[12], "V13     " );
	strcpy( kmusr.kusr[13], "V14     " );
	strcpy( kmusr.kusr[14], "V15     " );
	strcpy( kmusr.kusr[15], "V16     " );
	strcpy( kmusr.kusr[16], "V17     " );
	strcpy( kmusr.kusr[17], "V18     " );
	strcpy( kmusr.kusr[18], "V19     " );
	strcpy( kmusr.kusr[19], "V20     " );
	strcpy( kmusr.kusr[20], "V21     " );
	strcpy( kmusr.kusr[21], "V22     " );
	strcpy( kmusr.kusr[22], "V23     " );
	strcpy( kmusr.kusr[23], "V24     " );
	strcpy( kmusr.kusr[24], "V25     " );
	strcpy( kmusr.kusr[25], "V26     " );
	strcpy( kmusr.kusr[26], "V27     " );
	strcpy( kmusr.kusr[27], "V28     " );
	strcpy( kmusr.kusr[28], "V29     " );
	strcpy( kmusr.kusr[29], "V30     " );
	for( j = 1; j <= MUSR; j++ ){
		j_ = j - 1;
		Numusr[j] = 1;
		Ndxusr[j] = j;
		}
	for( j = 1; j <= MVUSR; j++ ){
		j_ = j - 1;
		Vusr[j] = 0.;
		}
	cmusr.nlusr = 0;
	strcpy( kmusr.klusr[0], "L1      " );
	strcpy( kmusr.klusr[1], "L2      " );
	strcpy( kmusr.klusr[2], "L3      " );
	strcpy( kmusr.klusr[3], "L4      " );
	strcpy( kmusr.klusr[4], "L5      " );
	strcpy( kmusr.klusr[5], "L6      " );
	strcpy( kmusr.klusr[6], "L7      " );
	strcpy( kmusr.klusr[7], "L8      " );
	strcpy( kmusr.klusr[8], "L9      " );
	strcpy( kmusr.klusr[9], "L10     " );
	for( j = 1; j <= MLUSR; j++ ){
		j_ = j - 1;
		Lusr[j] = FALSE;
		}

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    810414:  Original version.
	 *===================================================================== */

} /* end of function */
/* MODULE/LEVEL:
 *=====================================================================
 * GLOBAL COUPLING:
 *===================================================================== */
