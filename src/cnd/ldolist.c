#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/cnd.h"
int /*FUNCTION*/ ldolist(nerr)
int *nerr;
{
	char kcl[1001];
	int ldolist_v;
	int nchars;
	void *_p0;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE: To pick off the next do list variable off the string.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: .
	 *=====================================================================
	 * MODULE/LEVEL:  CND/2
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    ndolevel: Incremented by one.
	 *    lifresp:  Set to value of if test.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:   getvvstring, putvvstring
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    871110:  Increased size of character list.
	 *    870817:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	getvvstring( (char*)kcnd.kdovar[cnd.ndolevel - 1],MCPFN+1, (char*)kcnd.kdolist[cnd.ndolevel - 1]
	 ,MCPFN+1, &nchars, kcl,1001, nerr );
	if( lnxtcl( kcl,1001, &Idoin1[cnd.ndolevel], &Idoin2[cnd.ndolevel] ) ){
		nchars = Idoin2[cnd.ndolevel] - Idoin1[cnd.ndolevel] + 1;
                strtemp = malloc(min(Idoin2[cnd.ndolevel],1000) - 
		 Idoin1[cnd.ndolevel] + 2);
                strncpy(strtemp,kcl+Idoin1[cnd.ndolevel] - 1,min(Idoin2[cnd.ndolevel],1000) - 
		 Idoin1[cnd.ndolevel] + 1);
                strtemp[min(Idoin2[cnd.ndolevel],1000)-Idoin1[cnd.ndolevel] + 1] = '\0';
		putvvstring( (char*)kcnd.kdovar[cnd.ndolevel - 1],MCPFN+1, (char*)kcnd.kdoname[cnd.ndolevel - 1]
		 ,MCPFN+1, nchars, strtemp, min(Idoin2[cnd.ndolevel],1000) - 
		 Idoin1[cnd.ndolevel] + 2, nerr );
                free(strtemp);
		ldolist_v = TRUE;
		}
	else{
		ldolist_v = FALSE;
		}

L_8888:
	return( ldolist_v );

} /* end of function */

