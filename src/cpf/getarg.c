#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
char *getarg(argno,nchar)
int argno;
int *nchar;
{



	/*=====================================================================
	 * PURPOSE: To return a pointer to the requested argument, if requested
         *          argument <= 0, return pointer to the current argument.
	 *=====================================================================
	 * MODULE/LEVEL:  COM/4
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * PARAMETERS:
	 *=====================================================================
	 * VARIABLE DEFINITIONS:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *
	 *    940719:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */

        if( argno > kmcom.nkargs) {
          *nchar = 0;
          return (char *)NULL;
	}

        if (argno <= 0) argno = cmcom.jcom;

        *nchar = strlen(kmcom.kargs[argno-1]);

        return kmcom.kargs[argno-1];        

} /* end of function */

