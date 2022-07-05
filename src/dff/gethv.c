#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"


void apcmsg2(char* kalpha, int kalpha_s);



void /*FUNCTION*/ gethv(kname, kname_s, kvalue, kvalue_s, nerr)
char *kname;   int kname_s;
char *kvalue;   int kvalue_s;
int *nerr;
{
	int lsave;
	int ic1, ic2, icdfl1, icdfl2, icurdf, int_, isave, jdfl, 
	 nc, ncerr, ntused;
	void *_p0;
	static int iolddf = 1;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE: To get and format a header variable.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   The name of the variable to retrieve. [c]
	 *             KNAME is of the form: "FILE,VARIABLE" where
	 *             "FILE" is the name or number of the data file
	 *             and "VARIABLE" is the name of the header variable.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KVALUE:  The formatted value of the header variable.
	 *             Set to 'UNDEFINED' if not defined.
	 *             Set to 'ERROR' if an error occurred.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPW
	 *    DFM:     NDFL, KDFL, IDFLC
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvati.  0 means that if a string
         *             of digits is too int, let it slide by.  maf 
	 *    871103:  Changed return value of KVALUE for errors.
	 *    871022:  Fixed a more subtle variation of last bug.
	 *    871015:  Fixed bug in save/restore logic for header.
	 *    870722:  Added descriptive error messages.
	 *    870302:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870302
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine the file number. */

	nc = indexb( kname,kname_s );
	ic1 = 1;
	ic2 = 1;
L_2000:
	if( kname[ic2 - 1] == ',' ){
		if( ic2 == 1 ){
			icurdf = iolddf;
			}
		else{
                        int idx ;
 
                        strtemp = malloc(ic2-ic1+1);
			for( idx = 0 ; idx < ic2-ic1+1 ; idx++ )
			    strtemp[ idx ] = ' ' ;
                        strtemp[ ic2-ic1+1 ] = '\0' ;
                        strncpy(strtemp,kname+ic1 - 1,ic2-ic1);
                        strtemp[ic2-ic1] = '\0';

			cnvati( strtemp, ic2-ic1+1, &int_, 0, &ncerr );
							/* add 0. maf 970129 */

                        free(strtemp);

			if( ncerr == 0 ){
				if( int_ >= 1 && int_ <= cmdfm.ndfl ){
					icurdf = int_;
					}
				else{
					*nerr = 1310;
					setmsg( "ERROR", *nerr );
					apimsg( int_ );
					fstrncpy( kvalue, kvalue_s-1, "ERROR", 5);
					goto L_8888;
					}
				}
			else{
				icdfl1 = 0;
				icdfl2 = 0;
				jdfl = 1;
L_3000:
				if(lnxtcl( kmdfm.kdfl,MAXCHARS, &icdfl1, &icdfl2 )){
				  if( memcmp(kname+ic1 - 1,kmdfm.kdfl+icdfl1 - 
				      1,min(ic2-ic1,icdfl2-icdfl1+1)) == 0 ){
					  icurdf = jdfl;
					  }
				  else if( jdfl < cmdfm.ndfl ){
					  icdfl1 = icdfl2 + 1;
					  jdfl = jdfl + 1;
					  goto L_3000;
					  }
			        }
				else{
					*nerr = 1363;
					setmsg( "ERROR", *nerr );
                                        apcmsg2(&kname[ic1 - 1],ic2-ic1);
					fstrncpy( kvalue, kvalue_s-1, "ERROR", 5);
					goto L_8888;
					}
				}
			}
		}
	else if( ic2 < nc ){
		ic2 = ic2 + 1;
		goto L_2000;
		}
	else{
		*nerr = 1364;
		setmsg( "ERROR", *nerr );
		fstrncpy( kvalue, kvalue_s-1, "ERROR", 5);
		goto L_8888;
		}

	/* - Get the header from the memory manager if necessary. */

	if( icurdf != cmdfm.idflc ){
		if( cmdfm.idflc > 0 ){
			lsave = TRUE;
			isave = cmdfm.idflc;
			putfil( cmdfm.idflc, nerr );
			if( *nerr != 0 )
				goto L_8888;
			}
		else{
			lsave = FALSE;
			}
		getfil( icurdf, FALSE, &ntused, &ntused, &ntused, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}
	else{
		lsave = FALSE;
		}

	/* - Format the requested header field. */

        strtemp = malloc(nc - (ic2 + 1) + 2);
        strncpy(strtemp,kname+ic2,nc - (ic2 + 1) + 1);
        strtemp[nc - (ic2 + 1) + 1] = '\0';

	formhv( strtemp, nc-(ic2+1)+2, 3, kvalue,kvalue_s, nerr );

        free(strtemp);

	if( *nerr > 0 ){
		fstrncpy( kvalue, kvalue_s-1, "ERROR", 5);
		goto L_8888;
		}
	else if( *nerr < 0 ){
		fstrncpy( kvalue, kvalue_s-1, "UNDEFINED", 9);
		}
	iolddf = icurdf;

	/* - Get the saved header from the memory manager if necessary. */

	if( lsave ){
		getfil( isave, FALSE, &ntused, &ntused, &ntused, nerr );
		if( *nerr != 0 )
			goto L_8888;
		}

L_8888:
	return;

} /* end of function */


