#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "nnm.h"
void /*FUNCTION*/ xwritenn(nerr)
int *nerr;
{
	int jdfl, ndx1, ndx2, nlen, nlenheader, nlocdisk, 
	 notused, nun;
	void zwabs();


	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITENN.
	 *           This command writes data files to disk in neural net format.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1301
	 *=====================================================================
	 * MODULE/LEVEL:  nnm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPFN
	 *    dfm:     ndfl
	 *    hdr:     user0
	 *    mem:     sacmem
	 *    nnm:     kwritenn
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    nnm:     numpoints, numfiles, headerarray
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lcchar, setmsg, getfil,
	 *             zgtfun, znfile, zwabs, zclose
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    nlen:    Number of data points in each component. [i]
	 *    ndx1:    Index in SACMEM array of first data component. [i]
	 *    ndx2:    Index in SACMEM array of second data component. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    890306:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  890306
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- "filename":  name of neural net file to write. */
		if( lcchar( MCPFN, kmnnm.kwritenn,MCPFN+1, &notused ) )
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

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	if( cmdfm.ndfl <= 0 ){
		*nerr = 1301;
		setmsg( "ERROR", *nerr );
		goto L_8888;
	}

	/* EXECUTION PHASE: */

	/* - Start filling in header values. */

	cmnnm.numpoints = 0;
	cmnnm.numfiles = cmdfm.ndfl;
	nlenheader = 2 + cmnnm.numfiles;

	/* - Create data file and write dummy header into it. */

	znfile( &nun, kmnnm.kwritenn,MCPFN+1, "DATA",5, nerr );
	if( *nerr != 0 )
		goto L_8888;

	nlocdisk = 0;
	zwabs( &nun, &cmnnm.numpoints, nlenheader, &nlocdisk, nerr );
	if( *nerr != 0 )
		goto L_8888;
	nlocdisk = nlenheader;

	/* - Write each file in memory to disk. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){

		/* -- Get file from memory manager. */
		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Check number of data points. */
		if( jdfl == 1 ){
			cmnnm.numpoints = nlen;
		}
		else if( nlen != cmnnm.numpoints ){
			*nerr = 2801;
			setmsg( "ERROR", *nerr );
			goto L_8888;
		}

		/* -- Save header variable user0 into local array. */
		Headerarray[jdfl] = *user0;

		/* -- Write the data. */
		zwabs( &nun, cmmem.sacmem[ndx1], cmnnm.numpoints, &nlocdisk, nerr );
		if( *nerr != 0 )
			goto L_8888;
		nlocdisk = nlocdisk + cmnnm.numpoints;

	}

	/* - Rewrite header now that it is complete. */

	nlocdisk = 0;
	zwabs( &nun, &cmnnm.numpoints, nlenheader, &nlocdisk, nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Close file. */

	zclose( &nun, nerr );

L_8888:
	return;

} /* end of function */

