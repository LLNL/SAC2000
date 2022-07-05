#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "eam.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);


void /*FUNCTION*/ xohpf(nerr)
int *nerr;
{
	int njunk;


	/*=====================================================================
	 * PURPOSE:  To execute the action command OHPF.
	 *           This command opens a HPF (HPF).
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1901
	 *=====================================================================
	 * MODULE/LEVEL:  EAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    EAM:     LHPFOP, KHPFNM
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    EAM:     LHPFOP, KHPFNM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, LCCHAR, CFMT, CRESP,
	 *             ZCLOSE, SETMSG, APCMSG, ZGTFUN, ZNFILE
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920505:  Added hypo input file End-Of-File identifier string.
	 *             (That's 17 spaces and a '10').
	 *    870507:  Fixed bug in call to old message system.
	 *    820810:  Changed names for HPF and APF variables.
	 *    820621:  Changed to newest set of parsing and checking functions.
	 *    810414:  Added call to ZGTFUN to get an unused fortran file unit.
	 *    810205:  Replaced call to ZFILNM with more general ZCHSTR.
	 *    810120:  Changed to output message retrieval from disk.
	 *    800308:  Original version.
	 *=====================================================================
	 * DOCUMENTED:  820624
	 *===================================================================== */
	*nerr = 0;

	/* PROCEDURE: */

	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){
		/* -- "name":  name of HPF to open. */
		if( lcchar( MCPFN, kmeam.khpfnm,MCPFN+1, &njunk ) )
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

	/* - Close previous HPF, (after writing eof string), if it is open. */

	if( cmeam.lhpfop ){
                fprintf(cmeam.nhpfun,"%19s\n","10");
		zcloses( &cmeam.nhpfun, nerr );
	}

	if( *nerr != 0 ){
		*nerr = 1901;
		setmsg( "ERROR", *nerr );
		apcmsg( kmeam.khpfnm,MCPFN+1 );
		goto L_8888;
	}

	/* - Open new file. */

	znfiles( &cmeam.nhpfun, kmeam.khpfnm,MCPFN+1, "TEXT",5, nerr );
	if( *nerr != 0 ){
		*nerr = 1901;
		setmsg( "ERROR", *nerr );
		apcmsg( kmeam.khpfnm,MCPFN+1 );
		goto L_8888;
	}

	/* - Position to end-of-file. */

	if ( fseek (cmeam.nhpfun , 0L , SEEK_END ) != 0 )
		fprintf ( stdout , "fseek returned error-xohpf\n" ) ;

	/* - Remove the previous hypo eof marker by simply backspacing. */

	backspace( cmeam.nhpfun, 1L );

	/* - Set flag showing that a HPF is open. */

	cmeam.lhpfop = TRUE;

L_8888:
	return;

} /* end of function */

