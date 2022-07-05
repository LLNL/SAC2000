#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/wild.h"
void /*FUNCTION*/ xwild(nerr)
int *nerr;
{
	char _c0[2];
	int _l0, nret;
	char citr[4];
	void *_p0;
        char *strtemp;


	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command WILD.
	 *           This command defines the wild-card keys to be used
	 *           in subsequent READ, READHDR, and READALPHA commands.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    WILD:    SNGL, MULT, CITR, CCON
	 *    DFM:     LECHOF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LKCHAR, LKLOG
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    860922:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860922
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- "SINGLE char": define character to use to match single characters. */
                _c0[0] = kmwild.sngl;
                _c0[1] = '\0';
		_l0 = lkchar( "SINGLE$",8, 1, _c0, 2, &nret );
		 kmwild.sngl = _c0[0];
		if( _l0 ){

			/* -- "MULTIPLE char": define character to use to match multiple characters. */
			}
		else{
                        _c0[0] = kmwild.mult;
                        _c0[1] = '\0';
			_l0 = lkchar( "MULTIPLE$",10, 1, _c0,2, &nret );
			 kmwild.mult = _c0[0];
			if( _l0 ){

				/* -- "ITERATION chars": define two characters to use for iteration. */
				}
			else{
                                strcpy(citr,"   ");
				_l0 = lkchar( "ITERATION$",11, 2, citr, 4, &nret );

				if( _l0 ){
					if( nret != 2 ){
						ictok( -1 );
						cfmt( "NEED TWO CHARACTERS:$",22 );
						cresp();
						}

					/* -- "CONCATENATION char": define two characters to use for concatentation. */
					}
				else if( lkchar( "CONCATENATION$",15, 2, kmwild.ccon
				 ,3, &nret ) ){
					if( nret != 2 ){
						ictok( -1 );
						cfmt( "NEED TWO CHARACTERS:$",22 );
						cresp();
						}

					/* -- "ECHO [ON|OFF]": turn echoing of filelists on or off. */
					}
				else if( lklog( "ECHO$",6, &cmdfm.lechof ) ){

					/* -- Bad syntax. */
					}
				else{
					cfmt( "ILLEGAL OPTION:$",17 );
					cresp();

					}
				}
			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

L_8888:
	return;

} /* end of function */

