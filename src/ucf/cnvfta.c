#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
void /*FUNCTION*/ cnvfta(float_, nchar, nsig, kfloat, kfloat_s)
double float_;
int nchar, nsig;
char *kfloat;   int kfloat_s;
{
	char _c0[2], kfmt[9];
	int ic;
	void *_p0;
        char *s1;

	/*=====================================================================
	 * PURPOSE:  To convert a floating point number to its ASCII equivalent.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    FLOAT:   Floating point number to convert. [f]
	 *    NCHAR:   Number of characters in ASCII equivalent.
	 *    NSIG:    Number of significant figures in ASCII equivalent. [i]
	 *             This format statement must including the parenthesis.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KFLOAT:  ASCII equivalent. [c]
	 *             Set to 'BADINPUT' if number could not be converted.
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    RNDOFF
	 *=====================================================================
	 * EXAMPLE OF USE:
	 * - If you wanted to convert the variable F to ASCII and store in KF
	 *   and you wanted the format statement to be F10.3 then the call
	 *   would be:   CALL CNVFTA(F,10,3,KF).  KF must then be a character
	 *   variable at least 10 characters int.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KFMT:    Format statement constructed from input arguments. [c8]
	 *    IC:      Counter used in creating KFMT. [i]
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Build format statement by encoding input variables.
	 *   (If NSIG is 0 then create an integer rather than real format.) */
	if( nsig > 0 ){
		strcpy( kfmt, "%" );
		ic = 1;
		if( nchar < 9 ){
                        sprintf(kfmt+ic,"%1d",nchar);
			ic = ic + 1;
			}
		else{
                        sprintf(kfmt+ic,"%2d",nchar);
			ic = ic + 2;
			}
		kfmt[ic] = '.';
		ic = ic + 1;
		if( nsig <= 9 ){
                        sprintf(kfmt+ic,"%1d",nsig);
			ic = ic + 1;
			}
		else{
                        sprintf(kfmt+ic,"%2d",nsig);
			ic = ic + 2;
			}
                strcpy(kfmt+ic,"f");
                if(sprintf(kfloat,kfmt,float_) < 0) goto L_9000;
		}
	else{
		strcpy( kfmt, "%" );
		ic = 1;
		if( nchar <= 9 ){
                        sprintf(kfmt+ic,"%1d",nchar);
			ic = ic + 1;
			}
		else{
                        sprintf(kfmt+ic,"%2d",nchar);
			ic = ic + 2;
			}
                strcpy(kfmt+ic,"d");
                if(sprintf(kfloat,kfmt,(int)( (1. + 10.*RNDOFF)*
		 float_ ) ) < 0) goto L_9000;
		}

L_8888:
	return;

L_9000:
	fstrncpy( kfloat, kfloat_s-1, "BADINPUT", 8 );
	goto L_8888;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    830922:  Made machine independent by using F77 encode capability.
	 *    800102:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850111
	 *===================================================================== */

} /* end of function */

