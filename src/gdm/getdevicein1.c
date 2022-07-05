#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
void /*FUNCTION*/ getdeviceinfo1(kdname, kdname_s, idtype)
char *kdname;   int kdname_s;
int *idtype;
{



	/*=====================================================================
	 * PURPOSE:  To get the name and type of graphics device 1 
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KDNAME:  The name of the graphics device. [c]
	 *    IDTYPE:  The type of the graphics device. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  GD1/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GD1:     INAME1, ITYPE1
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    981112:  Device 1 was originally the terminal (eg. vt100).  
	 *             Today, this obsolete device is removed from SAC.
	 *             The plumbing is left in place in case a new device is
	 *             added later, it can become device1.  For now, 
         *             pass back "        ", and 3.
	 *             (Q: Why 3?  A: That's essentially what it was setting
	 *             *idtype to before; the process was more complicated, but
	 *             the result was always 3)   maf 
	 *    862020:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  862020
	 *===================================================================== */
	/* PROCEDURE: */
	fstrncpy( kdname , kdname_s-1 , "        " , 8 );
	*idtype = 3 ;

L_8888:
	return;

} /* end of function */

