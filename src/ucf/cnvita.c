#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ cnvita(intgr, kintgr, kintgr_s)
int intgr;
char *kintgr;   int kintgr_s;
{
	char kfmt[9];
	int ncf, nck;
	void *_p0;
        char *s1;

	/*=====================================================================
	 * PURPOSE:  To convert an integer into its ASCII equivalent.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    INTGR:   Integer to convert. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KINTGR:  ASCII equivalent. [c]
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KFMT:    Format statement used to encode integer. [c8]
	 *    NCK:     Number of characters in KINTGR. [i]
	 *    NCF:     Used in creating KFMT. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Determine length of character variable. */
	nck = (kintgr_s - 1);

	/* - Create format statement. */

	strcpy( kfmt, "%" );
        ncf = 1;
	if( nck <= 9 ){
                sprintf(kfmt+ncf,"%1d",nck);
                ncf++;
                		}
	else if( nck <= 99 ){
                sprintf(kfmt+ncf,"%2d",nck);
                ncf += 2;
		}
	else{
                sprintf(kfmt+ncf,"%3d",nck);
                ncf += 3;
		}
        strcpy(kfmt+ncf,"d");

	/* - Encode integer into string. */

        if( sprintf(kintgr,kfmt,intgr) < 0 )
           fstrncpy(kintgr, kintgr_s - 1,"BADINPUT",8);
L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    800102:  Original version.
	 *===================================================================== */

} /* end of function */

