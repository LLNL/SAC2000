#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lcra(nramn, nramx, ra, nra)
int nramn, nramx;
float ra[];
int *nra;
{
	char ktemp[9];
	int lcra_v;
	int nerr;
        char *cattemp;

	float *const Ra = &ra[0] - 1;



	/*=====================================================================
	 * PURPOSE: To parse a "real array" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lcra:    .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    nramn:   Minimum number of reals to return. [i]
	 *    nramx:   Maximum number of reals to return. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ra:      Real array found in command. [fa]
	 *    nra:     Length of RA. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     cfmt, cresp
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820914:  Was not setting function value upon return.
	 *    820721:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Pop real variables off command until:
	 *   (1) Maximum number is reached  OR
	 *   (2) Next token is not a real variable. */
	*nra = 0;
	nerr = 0;
L_2000:
	if( *nra < nramx && Itypcm[cmcom.jcom] == cmcom.inumbr ){
		*nra = *nra + 1;
		Ra[*nra] = Flnum[cmcom.jcom];
		cmcom.jcom = cmcom.jcom + 1;
		if( cmcom.jcom <= cmcom.ncom )
			goto L_2000;

		/* - Perform standard error recovery if minimum number of reals not found. */

		}
	else if( *nra < nramn ){
                sprintf(ktemp,"%5d",nramn);
                cattemp = malloc( 14+strlen(ktemp)+8+1);
                strcpy(cattemp,"NEED AT LEAST ");
                strcat(cattemp,ktemp);
                strcat(cattemp," REALS:$");
		cfmt( cattemp,  14+strlen(ktemp)+8+1);
		cresp();
                free(cattemp);
		if( lcmore( &nerr ) )
			goto L_2000;
		lcra_v = TRUE;
		goto L_8888;
		}

	/* - If we get to here with no errors, set function value to .TRUE. */

	lcra_v = nerr == 0;

L_8888:

	return( lcra_v );


} /* end of function */

