#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gem.h"
void /*FUNCTION*/ xline(nerr)
int *nerr;
{
	int lwarning;
	int int_;



	/*=====================================================================
	 * PURPOSE:  To execute the parameter-setting command LINE.
	 *           This command controls the linestyle attributes.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      nerr:  Error return flag [i]
	 *=====================================================================
	 * MODULE/LEVEL: gem/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    gem:     MILINE, ICLINE
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    gem:     lline, icline, liline
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    sac:     lcmore, cfmt, cresp, lclog, lcint, lklog, lckey,
	 *             setmsg, apcmsg, apimsg, aplmsg, outmsg
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    910301:  Changed iline to icline.
	 *    900402:  Added warning message if linestyle list exceeds maximum.
	 *    830114:  Added LIST option.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820817:  Documented subroutine.
	 *    811228:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900402
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Parse positional tokens first: */

	/* -- ON/OFF turns on/off line drawing. */
	if( lclog( &cmgem.lline ) ){

		/* -- An integer changes the linestyle and turns line drawing on. */
		}
	else if( lcint( &cmgem.icline ) ){
		cmgem.lline = TRUE;

		}

	/* - Parse position-independent tokens: */

L_1000:
	if( lcmore( nerr ) ){

		/* -- INC turns on or off the increment linestyle attribute. */
		if( lklog( "I$",3, &cmgem.liline ) ){
			cmgem.lline = TRUE;
			cmgem.jiline = 0;

			/* -- SOLID changes linestyle to solid. */
			}
		else if( lckey( "S$",3 ) ){
			cmgem.icline = cmgem.isolid;
			cmgem.lline = TRUE;

			/* -- DOTTED changes linestyle to dotted. */
			}
		else if( lckey( "D$",3 ) ){
			cmgem.icline = cmgem.idot;
			cmgem.lline = TRUE;

			/* -- "LIST STANDARD/n ...":  change list of linestyle numbers to use. */
			}
		else if( lckey( "L$",3 ) ){
			if( lckey( "S$",3 ) ){
				inilin( cmgem.iiline, &cmgem.niline );
				}
			else{
				cmgem.niline = 0;
				lwarning = FALSE;
L_1100:
				if( lcint( &int_ ) ){
					if( cmgem.niline < MILINE ){
						cmgem.niline = cmgem.niline + 1;
						Iiline[cmgem.niline] = int_;
						}
					else if( !lwarning ){
						setmsg( "WARNING", 1 );
						apcmsg( "Maximum length of linestyle list is"
						 ,36 );
						apimsg( MILINE );
						aplmsg( "Will ignore remaining entries in list."
						 ,39 );
						outmsg();
						clrmsg();
						lwarning = TRUE;
						}
					goto L_1100;
					}
				if( cmgem.niline <= 0 )
					inilin( cmgem.iiline, &cmgem.niline );
				cmgem.icline = Iiline[1];
				cmgem.lline = TRUE;
				cmgem.jiline = 0;
				}

			/* -- Bad syntax. */
			}
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

L_8888:
	return;

} /* end of function */

