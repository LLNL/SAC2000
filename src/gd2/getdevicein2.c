#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
void /*FUNCTION*/ getdeviceinfo2(kdname, kdname_s, idtype)
char *kdname;   int kdname_s;
int *idtype;
{



	/*=====================================================================
	 * PURPOSE:  To inquire about certain attributes of graphics device 2.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KDNAME:  The name of the graphics device. [c]
	 *    IDTYPE:  The type of the graphics device. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  GD2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GD2:     INAME2, ITYPE2
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    862020:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  862020
	 *===================================================================== */
	/* PROCEDURE: */
	fstrncpy( kdname, kdname_s-1, kmgd2.kname2, strlen(kmgd2.kname2));
	*idtype = cmgd2.itype2;

L_8888:
	return;

} /* end of function */

