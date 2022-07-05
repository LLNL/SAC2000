#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/sss.h"
void /*FUNCTION*/ xdistancewind(nerr)
int *nerr;
{
	int lfixed;
	int ntused;



	/*=====================================================================
	 * PURPOSE:  To execute the DISTANCEWINDOW command.
	 *           This command controls the distance window properties
	 *           of the record section plot (PLOTRECORDSECTION).
	 *=====================================================================
	 * Output ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1001.
	 *=====================================================================
	 * MODULE/LEVEL:  sss/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    sss:     kdwun, ndwun
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    sss:     idwun, idwop, dwwid, dwlim
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lckey, lcra, lclist, lkreal
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970514:  If no option is specified, it now assumes FIXED.  maf
	 *    860304:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860304
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	lfixed = FALSE;
	while ( lcmore( nerr ) ){

		/* -- "USEDATA":  use the minimum and maximum distance from DFL. */
		if( lckey( "USEDATA$",9 ) ){
			cmsss.idwop = 1;
		}

		/* -- "WIDTH v":  use minimum distance from DFL but force width to fixed value. */
		else if( lkreal( "WIDTH$",7, &cmsss.dwwid ) ){
			cmsss.idwop = 2;
		}

		/* -- "UNITS KILOMETERS|DEGREES":  set distance window units.
		 *    (Also allow "KM" as abbreviation for "KILOMETERS".) */
		else if( lklist( "UNITS$",7, (char*)kmsss.kdwun,9, MDWUN, 
		 &cmsss.idwun ) ){
			if( cmsss.idwun == 3 )
				cmsss.idwun = 1;
			if( cmsss.idwun != cmsss.ndwun ){
				if( cmsss.idwun == 1 ){
					Dwlim[1] = Dwlim[1]*RKMPERDG;
					Dwlim[2] = Dwlim[2]*RKMPERDG;
				}
				else{
					Dwlim[1] = Dwlim[1]/RKMPERDG;
					Dwlim[2] = Dwlim[2]/RKMPERDG;
				}
			}
			cmsss.ndwun = cmsss.idwun;
		}

                /* -- "FIXED v1 v2":  fix data window to be between v1 and v2. */
                else if( lkra( "FIXED$",7, 2, 2, cmsss.dwlim, &ntused ) ){
                        cmsss.idwop = 3;
                        lfixed = TRUE;
                }

		/* -- If the command line just has two real numbers, 
		      it is implicitly fixed.  maf 970514 */
		else if ( lcra ( 2 , 2 , cmsss.dwlim , &ntused ) ) {
			cmsss.idwop = 3 ;
			lfixed = TRUE ;
		}

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}

	} /* end while */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	return;

} /* end of function */


