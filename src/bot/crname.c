#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"


void apcmsg2(char* kalpha, int kalpha_s);


void crname(char* kname, int kname_s, byte kdelim, char* kappnd, int kappnd_s, int* nerr)
{
	int mname, nappnd, nname;
        char *cattemp;
        byte delimtemp;

	/*=====================================================================
	 * PURPOSE:  To create a pathname piece by piece.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Current name to build upon. [c]
	 *    KDELIM:  Delimiter to put before next part. [c1]
	 *    KAPPND:  New part of path name to append to current name. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KNAME:   Concantenation of current name, delimiter, and new name. [c]
	 *    NERR:    Error return flag.  Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXB
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    MNAME:   Maximum length of KNAME. [i]
	 *    NNAME:   Current length of KNAME. [i]
	 *    NAPPND:  Length of KAPPND. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine length of base and additional name. */

	mname = (kname_s - 1);
	nname = indexb( kname,kname_s );
	nappnd = indexb( kappnd,kappnd_s );

	/* - Concantenate delimiter and new name to base name if they fit. */

	if( mname >= (nname + nappnd + 1) ){
                cattemp = malloc(nappnd+2);
                cattemp[0] = kdelim;
                strncpy(cattemp+1,kappnd,nappnd);
                cattemp[nappnd+1] = '\0';
		subscpy( kname, nname, -1, kname_s - 1, cattemp);
                free(cattemp);
	}
	else{
		*nerr = 916;
		setmsg( "ERROR", *nerr );
                apcmsg2(kname,nname);
                delimtemp = kdelim;
		apcmsg( &delimtemp, 1 );
                apcmsg2(kappnd,nappnd);
	}

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    831110:  Original version.
	 *===================================================================== */

} /* end of function */

