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
#include "bom.h"

void apcmsg2(char* kalpha, int kalpha_s);

void /*FUNCTION*/ xaddf(nerr)
int *nerr;
{
	char kdttm[33];
	int ic1, ic2, j, jbfl, jdfl, n1bdtm[6], n1zdtm[6], 
	 n2bdtm[6], ncerr, ndx1, ndx1b, ndx2, ndx2b, nlen, nlenb, npts1;
	int lnewhdr ; /* let header data come from new file */
	float begin1, delta1, delta2, diff, value;

        float *Sacmem1, *Sacmem2;

	/*=====================================================================
	 * PURPOSE:  To execute the action command ADDF.
	 *           This command adds a set of files to data in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1001, 1307, 1306, 1801
	 *=====================================================================
	 * MODULE/LEVEL:  BOM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    RNDOFF
	 *    BOM:     MBFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    BOM:     NBFL, KBFL, IBFLC
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCDFL,
	 *             VFLIST, VFEVEN, VBLIST, KADTTM
	 *             GETFIL, GETBFL, COPYI, GTOUTM, WRTXTT, IDTTM, DDTTM,
	 *             EXTRMA, PUTFIL, RELBFL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    881130:  Fixed bug in begin time error checking.
	 *    850730:  Changes due to new memory manager.
	 *    820809:  Changed to newest set of parsing and checking functions.
	 *    820331:  Combined "parse" and "control" modules.
	 *    810224:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  881130
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){
	    /* -- NEWHDR:  take the header from the new file being merged in.*/
	    if ( lklog ( "NEWHDR" , 7 , &lnewhdr ) ) {
		cmbom.lnewhdr = lnewhdr ;
	    }

	    /* -- "filelist':  define a new binop filelist. */
	    if( lcdfl( kmbom.kbfl,MAXCHARS, &cmbom.nbfl ) ){
		cmbom.ibflc = 0;
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

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Check to make sure all files are evenly spaced time series files. */

	vfeven( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Check for a null binop file list. */

	vblist( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Make sure each file in BFL are of the proper type. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    getfil( jdfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
			goto L_8888;
	    npts1 = *npts;
	    delta1 = *delta;
	    copyi( nzdttm, n1zdtm, 6 );
	    begin1 = *begin;
	    jbfl = min( jdfl, cmbom.nbfl );
	    getbfl( jbfl, FALSE, &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    if( *iftype == *irlim || *iftype == *iamph ){
		*nerr = 1307;
		setmsg( "ERROR", *nerr );
		lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
		apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		goto L_8888;
	    }
	    else if( !*leven ){
		*nerr = 1306;
		setmsg( "ERROR", *nerr );
		lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
		apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		goto L_8888;
	    }
	    value = (*delta - delta1)/ *delta;
	    if( !linrng( value, -RNDOFF, RNDOFF ) ){
		setmsg( "OUTPUT", 1801 );
		apcmsg( "DELTA",6 );
		lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
		apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
		apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		if( strcmp(kmbom.kecdel,"FATAL   ") == 0 ){
		    typmsg( "ERROR" );
		    *nerr = 1801;
		    goto L_8888;
		}
		else if( strcmp(kmbom.kecdel,"WARNING ") == 0 ){
		    typmsg( "WARNING" );
		    outmsg();
		}
	    }
	    else if( *npts != npts1 ){
		setmsg( "OUTPUT", 1801 );
		apcmsg( "NPTS",5 );
		lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
		apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
		apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		if( strcmp(kmbom.kecnpt,"FATAL   ") == 0 ){
		    typmsg( "ERROR" );
		    *nerr = 1801;
		    goto L_8888;
		}
		else if( strcmp(kmbom.kecdel,"WARNING ") == 0 ){
		    typmsg( "WARNING" );
		    outmsg();
		}
	    }
	    if( ldttm( n1zdtm ) && ldttm( nzdttm ) ){
		idttm( n1zdtm, begin1, n1bdtm );
		idttm( nzdttm, *begin, n2bdtm );
		ddttm( n1bdtm, n2bdtm, &diff );
		if( fabs( diff ) > RNDOFF ){
		    setmsg( "WARNING", 1802 );
		    lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
		    apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		    lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
		    apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		    aplmsg( "  BEG1:",8 );
		    kadttm( n1bdtm, kdttm,33, &ncerr );
		    apcmsg( kdttm,33 );
		    aplmsg( "  BEG2:",8 );
		    kadttm( n2bdtm, kdttm,33, &ncerr );
		    apcmsg( kdttm,33 );
		    outmsg();
		}
	    }
	}

	/* - Release last binop file. */

	relbfl( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* EXECUTION PHASE: */

	/* - Perform the file addition on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    /* -- Get the next file in DFL, moving header to CMHDR. */

	    getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    delta2 = *delta;

	    /* -- Get the next file in the BFL, moving header to CMHDR. */

	    jbfl = min( jdfl, cmbom.nbfl );
	    getbfl( jbfl, TRUE, &nlenb, &ndx1b, &ndx2b, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    *delta = delta2;

	    /* -- Output file is the shorter of two input files. */

	    npts1 = min( nlen, nlenb );
	    Nlndta[jdfl] = npts1;

	    /* -- Perform file addition on these two files. */

	    Sacmem1 = cmmem.sacmem[ndx1];
	    Sacmem2 = cmmem.sacmem[ndx1b];

	    for( j = 0; j <= (npts1 - 1); j++ ){
		*(Sacmem1++) += *(Sacmem2++);
	    }

	    /* -- Adjust header of file in DFL. */
	    if ( !cmbom.lnewhdr )
		getfil ( jdfl , FALSE , &nlen , &ndx1 , &ndx2 , nerr ) ;
	    *npts = npts1 ;
	    Sacmem1 = cmmem.sacmem[ndx1];
	    extrma( Sacmem1, 1, *npts, depmin, depmax, depmen );

	    /* -- Return file in DFL to memory manager. */

	    putfil( jdfl, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	}

	/* - Release last binop file. */

	relbfl( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

} /* end of function */

