#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ xglobalstack(nerr)
int *nerr;
{
	int ldlyi, ldlyt;
	float delay, delayi;



	/*=====================================================================
	 * PURPOSE: To parse the parameter-setting command GLOBALSTACK.
	 *          This command defines global stack file list properties.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001.
	 *=====================================================================
	 * MODULE/LEVEL:  sss/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    sss:     wtg, ldlyt, ldlyt, dlytg, dlytig, dlyng, dlynig,
	 *             lpolg, dstg
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lkreal, lclog2, lkchar
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    850812:  Major revision.
	 *    821207:  Original version from STMCOM.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850812
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "WEIGHT v":  define global weight property. */
		if( lkreal( "WEIGHT$",8, &cmsss.wtg ) ){

			/* -- "DELAY v":  define global static delay propertys. */
			}
		else if( lkreal( "DE#LAY$",8, &delay ) ){
			if( lckey( "SECONDS$",9 ) ){
				ldlyt = TRUE;
				}
			else if( lckey( "POINTS$",8 ) ){
				ldlyt = FALSE;
				}
			if( ldlyt ){
				cmsss.dlytg = delay;
				}
			else{
				cmsss.dlyng = delay;
				}

			/* -- "INCREMENT v":  define global static delay propertys. */
			}
		else if( lkreal( "INCREMENT$",11, &delayi ) ){
			if( lckey( "SECONDS$",9 ) ){
				ldlyi = TRUE;
				}
			else if( lckey( "POINTS$",8 ) ){
				ldlyi = FALSE;
				}
			if( ldlyi ){
				cmsss.dlytig = delay;
				}
			else{
				cmsss.dlynig = delay;
				}

			/* -- "NORMAL/REVERSED":  define global polarity property. */
			}
		else if( lclog2( "NORMAL$",8, "REVERSED$",10, &cmsss.lpolg ) ){

			/* -- "DISTANCE v":  define global distance property. */
			}
		else if( lkreal( "DI#STANCE$",11, &cmsss.dstg ) ){

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

