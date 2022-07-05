#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
void /*FUNCTION*/ xconv(nerr)
int *nerr;
{
	int iinout, junk, ndx1, ndx2, ndxh, nlen;
	static int iin = 1;
	static int iout = 2;


	/*=====================================================================
	 * PURPOSE:  To execute the action command CONV.
	 *           This command converts data files from one format to another.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    dfm:     krwfmt, nrwfmt, icfmt, kcfile, kcfmt
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    dfm:     icfmt, kcfile, kcfmt
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lckey, lcdfl, lkchar
	 *             cleardfl, rdsac, rdci, wrsac, wrci
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    IINOUT:  Flag used to distinguish between input and output
	 *             command parameters.
	 *    IIN:     Constant meaning input.  Set to 1.
	 *    IOUT:    Constant meaning output.  Set to 2.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880913:  Added call to clear data file list before execution.
	 *    850801:  Deleted SOCKITTOME format.
	 *             Changes in argument lists for RDSAC and RDCI.
	 *    820809:  Changed to newest set of parsing and checking functions.
	 *    810728:  Added optional format for ALPHA read/write.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  880913
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Initialize in/out flag. */

	iinout = iin;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "FROM":  set in/out flag to in. */
	    if( lckey( "&FROM$",7 ) )
		iinout = iin;

	    /* --"TO":  set in/out flag to out. */
	    else if( lckey( "&TO$",5 ) )
		iinout = iout;

	    /* -- "OVER": set in/out flag to out and use same filename as in. */
	    else if( lckey( "&OVER$",7 ) ){
		iinout = iout;
		strcpy( kmdfm.kcfile[iout - 1], kmdfm.kcfile[iin - 1] );
	    }

	    /* -- "SAC/ALPHA":  select in/out file format. */
	    else if(lclist((char*)kmdfm.krwfmt,9, cmdfm.nrwfmt, &Icfmt[iinout]))
	    { /* do nothing */ }

	    else if( lckey( "CI#$",5 ) )
		Icfmt[iinout] = 2;

	    /* -- "FMT string":  define a card-image format for in/out file. */
	    else if( lkchar( "FMT$",5, 16, (char*)kmdfm.kcfmt[iinout - 1]
	     ,strlen( (char*)kmdfm.kcfmt[iinout - 1])+1, &junk ) )
	    { /* do nothing */ }

	    /* -- "filename":  define in/out filename. */
	    else if( lcchar( MCPFN, (char*)kmdfm.kcfile[iinout - 1],MCPFN+1, 
	     &junk ) )
	    { /* do nothing */ }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	} /* end while */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
	    goto L_8888;

	/* EXECUTION PHASE: */

	cleardfl( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	if( Icfmt[iin] == 1 ){
	    rdsac( 1, (char*)kmdfm.kcfile[iin - 1],MCPFN+1, TRUE, TRUE,
		   &nlen, &ndxh, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}
	else if( Icfmt[iin] == 2 ){
	    rdci( 1 , (char*)kmdfm.kcfile[iin - 1] , MCPFN+1 ,
		  &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}
	cmdfm.ndfl = 1;

	/* Added to run data through SeisMgr. maf 980918 */
	cmdfm.nreadflag = HIGH ;
	cmdfm.lread = TRUE ;
	sacToSeisMgr ( TRUE , FALSE , TRUE , nerr ) ;
	cmdfm.lread = FALSE ;

	if ( *nerr ) 
	    goto L_8888 ;

	/* Write file if applicable */
	if( Icfmt[iout] == 1 )
	    wrsac( 1, (char*)kmdfm.kcfile[iout - 1],MCPFN+1, TRUE, 0, nerr );

	else if( Icfmt[iout] == 2 )
	    wrci( 1, (char*)kmdfm.kcfile[iout - 1],MCPFN+1,
	     (char*)kmdfm.kcfmt[iout - 1] , nerr );

L_8888:
	return;

} /* end of function */

