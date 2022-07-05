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
#include "icm.h"
#include "spe.h"

void /*FUNCTION*/ xprewit(nerr)
int *nerr;
{
	int iprew , jdfl , nlen , ndxy , ndxx ;
	float samint, coefficients[MPREWH+1];
	char errmsg[131] ;

	/* provide filenames for the FD option */
	int name1 = 0 , name2 = 0 ;
	char kname[ MCPFN + 10 ] ;

	/*=====================================================================
	 * PURPOSE: To parse and execute the action command WHITEN.
	 *          This command adds white noise to the data. 
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001, 5005, 5006.
	 *=====================================================================
	 * MODULE/LEVEL:  SPE/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, PREWIT
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- order  */
	    if ( lcint ( &iprew ) ) {
		cmicm.iprew = min ( iprew , MPREWH ) ;
	    }

	    /* FILTERDESIGN: pass whiten coefficients into filterdesign */
	    else if( lckey( "FILTERDESIGN #$" , 16 ) || lckey( "FD #$" , 6 ) )
		cmicm.lfd = TRUE ;

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	} /* end if ( lcmore ( nerr ) */

	if( *nerr != 0 )
	    goto L_8888;

	/* CHECKING PHASE: */

        /* - Test for a non-null data file list. */

        vflist( nerr );

        /* - Make sure each file is an evenly spaced time series file. */

        vfeven( nerr );
        if( *nerr != 0 )
            goto L_8888;


	/* EXECUTION PHASE: */
	errmsg[ 0 ] = '\0' ;

	for ( jdfl = 1 ; jdfl <= cmdfm.ndfl ; jdfl++ ) {
	    int idx ;

	    /* Get next file from the memory manager. */
	    getfil ( jdfl , TRUE , &nlen , &ndxy , &ndxx , nerr ) ;
	    if ( *nerr != 0 )
		goto L_8888 ;

	    /* if FD option used, get filename */
	    if ( cmicm.lfd ) {
		lnxtcl( kmdfm.kdfl , strlen( kmdfm.kdfl ) , &name1 , &name2 ) ;
		strncpy( kname , kmdfm.kdfl + name1 - 1 , name2 - name1 + 1 ) ;
		kname[ name2 - name1 + 1 ] = '\0' ;
	    }

	    prewit( cmmem.sacmem[ndxy], *npts, &cmicm.iprew, coefficients,
		    kname, errmsg );
	    if( errmsg[ 0 ] ){
		*nerr = 5005;
		setmsg( "ERROR", *nerr );
		aplmsg( errmsg,131 );
		goto L_8888;
	    }
	}

L_8888:
	/* Turn off cmicm.lfd, it is not sticky */
	cmicm.lfd = FALSE ;

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    980710:  Original version.  maf plagerized from xwhiten.c
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */

} /* end of function */

