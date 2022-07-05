#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gam.h"
void /*FUNCTION*/ qpicks()
{
	int j, j_;
        char *cattemp;


	/*=====================================================================
	 * PURPOSE:  To report about the current values of the PICKS parameters.
	 *=====================================================================
	 * MODULE/LEVEL:   EXM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    GAM:     LDSPPK, MPKNAM, KPKNAM, KPKTYP, IPKTYP, PKWDTH, PKHGTH
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  REPLV, REPAV, REPRV
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820921:  Original version (from QDISPL).
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870728
	 *===================================================================== */
	replv( "PICK display$",14, cmgam.ldsppk );
	repav( "Type of each pick display$",27, "        ",9 );
	for( j = 1; j <= MPKNAM; j++ ){
		j_ = j - 1;
                cattemp = malloc(2+2+2);
                strcpy(cattemp,"  ");
                strncat(cattemp,kmgam.kpknam[j_],2);
                strcat(cattemp,"$");
		repav( cattemp, 2+2+1+1, (char*)kmgam.kpktyp[Ipktyp[j] - 1],9 );
                free(cattemp);
		}
	reprv( "WIDTH of pick lines$",21, cmgam.pkwdth );
	reprv( "HEIGHT of pick lines$",22, cmgam.pkhgth );

L_8888:

	return;

} /* end of function */

