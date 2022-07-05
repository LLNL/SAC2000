#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/exm.h"
void /*FUNCTION*/ xreport(nerr)
int *nerr;
{
	int index, jrep;



	/*=====================================================================
	 * PURPOSE:  To execute the action command REPORT.
	 *           This command reports the current status of SAC commands.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      nerr:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  exm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MUNOUT
	 *    exm:     kreptp, nreptp
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    exm:     nrep, irep
	 *=====================================================================
	 * GLOBAL COUPLING:
	 * - Order of items in "kreptp" which is defined in "iniexm" must
	 *   match order in case statement below.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lclist, lcchar
	 *             putcl, lnxtcl, autooutmsg, q...
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920528:  Added line WIDTH report.
	 *    920403:  Added DATASET report.
	 *    870728:  Added LINE, SYMBOL, GTEXT, YLIM, and MTW reports.
	 *             Cleaned up calling arguments to q... routines.
	 *    850729:  Added AM (array manager) report.
	 *    840912:  Added terminal list report.
	 *    830121:  Added CUT and XLIM reports.
	 *    821208:  Added TITLE, XLABL, and YLABL reports.
	 *    820817:  Changed to newest set of parsing and checking functions.
	 *    820325:  Fixed bug in initializing NREP.
	 *    820323:  Added DISPLAY command report.
	 *    820316:  Added COLOR command report.
	 *    810514:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870301
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Reset report request counter if there are tokens in command. */

	if( lcmore( nerr ) )
		cmexm.nrep = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- "item":  the name of a reportable item. */
		if( lclist( (char*)kmexm.kreptp,9, cmexm.nreptp, &index ) ){
			cmexm.nrep = cmexm.nrep + 1;
			Irep[cmexm.nrep] = index;
		}

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - Set up automatic output mode. */

	autooutmsg( TRUE );
	setmsg( "OUTPUT", 99 );

	/* - For each item in list, perform a case branch and report value. */

	for( jrep = 1; jrep <= cmexm.nrep; jrep++ ){
		switch( Irep[jrep] ){
			case 1: qhpf () ;
				break ;
			case 2: qapf () ;
				break  ;
			case 3: qcolor () ;
				break ;
			case 4: qfid () ;
				break ;
			case 5: qpicks () ;
				break ;
			case 6: qtitle () ;
				break ;
			case 7: qxlabl () ;
				break ;
			case 8: qylabl () ;
				break ;
			case 9: qcut () ;
				break ;
			case 10: qxlim () ;
				 break ;
			case 11: qam () ;
				 break ;
			case 12: qdevices () ;
				 break ;
			case 13: qline () ;
				 break ;
			case 14: qsymbol () ;
				 break ;
			case 15: qgtext () ;
				 break ;
			case 16: qylim () ;
				 break ;
			case 17: qmtw () ;
				 break ;
			case 18: qwidth () ;
				 break ;
		} /* end switch */
	} /* end for */
	autooutmsg( FALSE );

L_8888:
	return;

} /* end of function */

