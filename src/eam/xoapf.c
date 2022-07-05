#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "eam.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);


void /*FUNCTION*/ xoapf(nerr)
int *nerr;
{
	int junk;


	/*=====================================================================
	 * PURPOSE:  To execute the action command OAPF.
	 *           This command opens an alphanumeric pick file.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1902, 1903
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     LAPFOP, KAPFNM, NAPFUN, LPFSTD
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EAM:     LAPFOP, KAPFNM, NAPFUN, LPFSTD
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLOG2, LCDFL,
	 *             ZCLOSE, GTOUTM, ZGTFUN, ZNFILE
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){
		/* -- "STD/NAME":  use standard pick id or simply name of data file. */
		if( lclog2( "STANDARD$",10, "NAME$",6, &cmeam.lpfstd ) )
		{ /* do nothing */ }

		/* -- "filename":  the name of the APF to open. */
		else if( lcchar( MCPFN, kmeam.kapfnm,MCPFN+1, &junk ) )
		{ /* do nothing */ }

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

	/* - Close previous APF if it is open. */

	if( cmeam.lapfop ){
		zcloses( &cmeam.napfun, nerr );
		if( *nerr == 0 ){
			cmeam.lapfop = FALSE;
		}
		else{
			*nerr = 1903;
			setmsg( "ERROR", *nerr );
			goto L_8888;
		}
	}


	/* - Open APF. */

	znfiles( &cmeam.napfun, kmeam.kapfnm,MCPFN+1, "TEXT",5, nerr );
	if( *nerr == 0 ){
		cmeam.lapfop = TRUE;
	}
	else{
		*nerr = 1902;
		setmsg( "ERROR", *nerr );
		apcmsg( kmeam.kapfnm,MCPFN+1 );
		goto L_8888;
	}

	/* - Position to end-of-file. */

	if ( fseek ( cmeam.napfun , 0L , SEEK_END ) != 0 )
		fprintf ( stdout , "fseek returned error-xoapf\n" ) ;

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    820809:  Changed to newest set of parsing and checking functions.
	 *             Deleted option for a global APF.
	 *    810414:  Added call to ZGTFUN to get a free fortran file unit.
	 *    810219:  Changed location of global pick files.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800725:  Original version.
	 *===================================================================== */

} /* end of function */

