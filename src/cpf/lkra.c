#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int /*FUNCTION*/ lkra(kkey, kkey_s, nramn, nramx, ra, nra)
char *kkey;   int kkey_s;
int nramn, nramx;
float ra[];
int *nra;
{
	char ktemp[9];
	int lkra_v;
	int nerr;
        char *cattemp;

	float *const Ra = &ra[0] - 1;



	/*=====================================================================
	 * PURPOSE: To parse a "keyed real array" command construct.
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lkra:    .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [l]
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kkey:    Keyword to search for. [c]
	 *             Append a dollar sign ("$") to end of keyword.
	 *             Append a pound sign ("#") to end of minimum allowed
	 *             abbreviation if it is more than a single character.
	 *    nramn:   Minimum number of reals to return. [i]
	 *    nramx:   Maximum number of reals to return. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    ra:      Real array found in command. [fa]
	 *    nra:     Length of "ra". [i]
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
	 *    sac:     cfmt, cresp, lckey
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    820914:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900510
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Search for key.  Return if not found. */
	lkra_v = lckey( kkey,kkey_s );
	if( !lkra_v )
		goto L_8888;

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
		}

	/* - If we get to here with no errors, set function value to .TRUE. */

	lkra_v = nerr == 0;

L_8888:

	return( lkra_v );


} /* end of function */

