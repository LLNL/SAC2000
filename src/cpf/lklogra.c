#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>

#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/com.h"
int lklogra(char* kkey, int kkey_s, int* offOnFlt, int nramn, int nramx, float* ra, int* nra , int* nerr )
{
	char ktoken[9];
	int lklogra_v;


	/*=====================================================================
	 * PURPOSE: To parse a keyed command construct which could take either
	 *          a logical ON|OFF or an array of floats (reals).
	 *=====================================================================
	 * FUNCTION VALUE:
	 *    lklogra: .TRUE. if the construct was found at the current
	 *             command symbol, .FALSE. otherwise. [i]
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
	 *    offOnFlt:  Denotes the users input:
	 *             0 means OFF, 1 means ON, 2 means array of floats. [l]
         *    ra:      Real array found in command. [fa]
         *    nra:     Length of RA. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  cpf/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    com:     jcom, ncom, kcom, itypcm, inumbr, ialpha, flnum
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    com:     jcom
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     cfmt, cresp, lckey, modcase
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970207:  Original version (merging lklog and lcra).
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  820426
	 *===================================================================== */
	/* PROCEDURE: */

	*nerr = 0 ;

	/* - Check for key. */
	lklogra_v = lckey( kkey,kkey_s );

	/* - Copy current token to local storage and convert to uppercase. */

	modcase( TRUE, (char*)kmcom.kcom[cmcom.jcom - 1], MCPW, ktoken );

	/* - Check for "ON" or "OFF" at next token.
	 *   Set logical variable to .TRUE. if not found. */

	if( lklogra_v ){
		if( memcmp(ktoken,"ON",2) == 0 ){
			*offOnFlt = 1;
			cmcom.jcom = cmcom.jcom + 1;
		}
		else if( memcmp(ktoken,"OF",2) == 0 ){
			*offOnFlt = 0;
			cmcom.jcom = cmcom.jcom + 1;
		}
		else{   /* look for real numbers. */
		    for ( *nra = 0 ; *nra < nramx && 
		      Itypcm[cmcom.jcom] == cmcom.inumbr && 
		      cmcom.jcom <= cmcom.ncom ; (*nra)++ ){
			ra[*nra] = Flnum[cmcom.jcom];
			cmcom.jcom = cmcom.jcom + 1;
		    } /* end for */

		    if( *nra < nramn ){
			char ktemp[50] ;
			sprintf ( ktemp , "NEED AT LEAST %d REALS OR 'ON' OR 'OFF':$" , nramn ) ;
			cfmt ( ktemp , strlen ( ktemp ) + 1 ) ;
			cresp() ;
			*nerr = 10000 ;
		    } /* end else if( *nra < nramn ) */
		    else
			*offOnFlt = 2 ;

		} /* end else */
	}

L_8888:
	return( lklogra_v );

} /* end of function */

