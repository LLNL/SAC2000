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

void alignFiles ( int *nerr );
int lckeyExact(char* kkey,int kkey_s);
void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ xmerge(nerr)
int *nerr;
{
	char kne[9], kstnm1[9];
	int ic1, ic2, eexact, iennd1, ibeg , dbeg, iftyp1, jdx, jbfl, jdfl, jdfl_, n1dttm[6], 
	 nbdttm[6], ndx, ndx1o, ndx2o, ndx1n, ndx2n,
	 ndxbfl, ndxnew, ndxold, nedttm[6], nlen, nlnbfl, nlnnew, nlnold,
	 ntused, nzeros[MDFL], icomORroll;
	float begin1, delta1, diff, ennd1, dennd1 , temp ;
	static int lchecktimes = TRUE;

	int *const Nbdttm = &nbdttm[0] - 1;
	int *const Nedttm = &nedttm[0] - 1;
	int *const Nzeros = &nzeros[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To execute the action command MERGE.
	 *           This command merges two sets of data files.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag.  Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  BOM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    RNDOFF
	 *    DFM:     MDFL, NDFL, KDFL
	 *    BOM:     MBFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    BOM:     NBFL, KBFL, IBFLC
	 *    DFM:     NDXDTA, NLNDTA
	 *    HDR:     NPTS, E, DEPMIN, DEPMAX, DEPMEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, LCDFL, CFMT, CRESP, VFLIST, VBLIST, VFEVEN,
	 *             GETFIL, COPYI, GETBFL, IDTTM, DDTTM, ALLAMB, RELAMB,
	 *             SETMSG, APCMSG, APLMSG, APIMSG, OUTMSG,
	 *             COPY, FILL, EXTRMA, PUTFIL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NLNOLD:  Number of points in current file before merge. [i]
	 *    NDXOLD:  Index in SACMEM array of data before merge. [i]
	 *    NLNBFL:  Number of points in binop file. [i]
	 *    NDXBFL:  Index in SACMEM array of binop file. [i]
	 *    NLNNEW:  Number of points in merged file. [i]
	 *    NDXNEW:  Index in SACMEM array of merged file. [i]
	 *    NZEROS:  Number of zeros needed to fill gap if any. [fa]
	 *    NDSFILES: Number of files in a given data-set. [i]
	 *    NDSFLNUM: File number (senquental) in the current data set. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    980928:  Enforce a commit or rollback prior to executing merge.
	 *             maf.
	 *    980605:  Allow files that overlap to be merged. maf
	 *    970822:  Added eexact to fix subtle bug in timing.  It gets the
         *             value of ennd1 + delta rounded to the millisecond.  maf
	 *    920110:  Added DATA-SET update logic.
	 *    900119:  Added CHECKTIMES option.
	 *    870811:  Added logic to fill gap with zeros if necessary.
	 *             Also made it an error if there is an overlap.
	 *    850617:  Major rewrite due to addition of new memory manager.
	 *    840206:  Fixed bug in writing scratch file headers to disk.
	 *    820331:  Combined "parse" and "control" modules.
	 *    810224:  Changed BFL storage location.
	 *    810130:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850617
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    if( lklog( "&CHECKTIMES$",13, &lchecktimes ) )
	    { ;/* do nothing */ }

            /* -- "COMMIT|RECALLTRACE|ROLLBACK": how to treat existing data. */
	    else if ( lckeyExact ( "COMMIT" , 7 ) )
		cmdfm.icomORroll = COMMIT ;
	    else if (lckeyExact ( "RECALLTRACE" , 12 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "RECALL" , 7 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "ROLLBACK" , 9 ) )
		cmdfm.icomORroll = ROLLBACK ;

	    /* -- define new binop data file list. */
	    else if( lcdfl( kmbom.kbfl,MAXCHARS, &cmbom.nbfl ) )
		cmbom.ibflc = 0;

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

	/* - Check for a null binop file list. */

	vblist( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Check to make sure all files are evenly spaced time series files. */

	vfeven( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* EXECUTION PHASE: */

        /* - Commit or rollback data according to cmdfm.icomORroll */
	alignFiles ( nerr ) ;
	if ( *nerr )
	    return ;

	/* - Release last binop file. */

	relbfl( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Make sure each file in DFL and BFL are of the proper type. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){

	    /* -- Get and save necessary header info from first (dfl) file. */
	    getfil( jdfl, TRUE, &nlen, &ndx1o, &ndx2o, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    delta1 = *delta;
	    strcpy( kstnm1, kstnm );
	    iftyp1 = *iftype;
	    copyi( nzdttm, n1dttm, 6 );
	    begin1 = *begin;
	    ennd1 = *ennd;

	    /* -- Get header info from second (bfl) file. */
	    jbfl = min( jdfl, cmbom.nbfl );
	    getbfl( jbfl, TRUE, &nlen, &ndx1n, &ndx2n, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Don't allow spectral files. */
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

	    /* -- Make sure delta, station name, and file type are the same. */
	    if( *delta != delta1 ){
		strcpy( kne, "DELTA   " );
	    }
	    else if( memcmp(kstnm,kstnm1,strlen(kstnm1)) != 0 ){
		strcpy( kne, "KSTNM   " );
	    }
	    else if( *iftype != iftyp1 ){
		strcpy( kne, "IFTYPE  " );
	    }
	    else{
		strcpy( kne, "OK      " );
	    }
	    if( strcmp(kne,"OK      ") != 0 ){
		*nerr = 1801;
		setmsg( "ERROR", *nerr );
		apcmsg( kne,9 );
		lnumcl( kmdfm.kdfl,MAXCHARS, jbfl, &ic1, &ic2 );
                apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
                apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		goto L_8888;
	    }

	    /* -- Check files for time consistency.
	     *    (1) Fill with zeros and send warning if there is a gap.
	     *    (2) Error out if there is an overlap, and the numbers don't match.
	     *    (3) Relax if they match within machine roundoff. */
	    /* eexact added to fix subtle bug.  maf 970822 */
	    iennd1 = (int) ennd1 ;	/* integer portion of end time */
	    dennd1 = ennd1 - (float) iennd1 ;  /* decimal portion of end time */
	    /* exact decimal portion of ennd1 with delta added in */
	    if ( iennd1 < 10000 ) {
		temp = 1000.0 * ( dennd1 + *delta ) ;
		temp += temp >= 0.0 ? 0.5 : -0.5 ;
		eexact = (int) temp ;
	    }
	    else {
		temp = 100.0 * ( dennd1 + *delta ) ;
		temp += temp >= 0.0 ? 0.5 : -0.5 ;
		eexact = ( (int) temp ) * 10 ;
	    }

	    ibeg = (int) (*begin) ;	/* integer portion of begin */
	    temp = 1000.0 * ( *begin - (float) ibeg ) ;
	    temp += temp >= 0.0 ? 0.5 : -0.5 ;
	    dbeg = (int) temp ;
	    idttmf( n1dttm, iennd1, eexact, nedttm );
	    idttmf( nzdttm, ibeg, dbeg, nbdttm );
	    ddttm( nbdttm, nedttm, &diff );
	    if( diff > RNDOFF && lchecktimes ){
		Nzeros[jdfl] = (int)( (diff - 0.5**delta)/ *delta ) + 1;
		setmsg( "WARNING", 1805 );
		lnumcl( kmdfm.kdfl,MAXCHARS, jbfl, &ic1, &ic2 );
                apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
		lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
                apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
		aplmsg( "END1:",6 );
		for( jdx = 1; jdx <= 6; jdx++ ){
		    apimsg( Nedttm[jdx] );
		}
		aplmsg( "BEG2:",6 );
		for( jdx = 1; jdx <= 6; jdx++ ){
		    apimsg( Nbdttm[jdx] );
		}
		outmsg();
	    }
	    else if( diff < -RNDOFF && lchecktimes ){
		/* get the data, compare to see if overlapping data lines up. maf 980603 */
		int idx, kdx ;

		Nzeros[ jdfl ] = 0 ;

/*		getfil( jdfl, TRUE, &nlen, &ndx1o, &ndx2o, nerr );
		getbfl( jbfl, TRUE, &nlen, &ndx1n, &ndx2n, nerr );
*/
		for ( idx = (int)( *begin - begin1 ) / *delta ,
		      kdx = 0 ;
		      idx <= (int)(  ennd1 - begin1 ) / *delta ;
		      idx++ , kdx++ )
		{
		    if ( cmmem.sacmem[ndx1o][idx] == cmmem.sacmem[ndx1n][kdx] )
			Nzeros[ jdfl ]-- ;
		    else {
			*nerr = 1802;
			setmsg( "ERROR", *nerr );
			lnumcl( kmdfm.kdfl,MAXCHARS, jbfl, &ic1, &ic2 );
                	apcmsg2(&kmdfm.kdfl[ic1 - 1],ic2-ic1+1);
			lnumcl( kmbom.kbfl,MAXCHARS, jbfl, &ic1, &ic2 );
                	apcmsg2(&kmbom.kbfl[ic1 - 1],ic2-ic1+1);
			aplmsg( "END1:",6 );
			for( jdx = 1; jdx <= 6; jdx++ ){
			    apimsg( Nedttm[jdx] );
			}
			aplmsg( "BEG2:",6 );
			for( jdx = 1; jdx <= 6; jdx++ ){
			    apimsg( Nbdttm[jdx] );
			}
			relbfl ( nerr ) ;
			goto L_8888;
		    }
		} /* end for ( idx, kdx ) */
	    }
	    else{
		Nzeros[jdfl] = 0;
	    }
	} /* end for ( jdfl ) */

	/* - Release last binop file. */

	relbfl( nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Perform the merge on each file in (active working-storage) 
	 *   data file list */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    jdfl_ = jdfl - 1;

	    /* -- Get next file from binop file list. */
	    jbfl = min( jdfl, cmbom.nbfl );
	    getbfl( jbfl, TRUE, &nlnbfl, &ndxbfl, &ntused, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Get next file from data file list. */
	    getfil( jdfl, TRUE, &nlnold, &ndxold, &ntused, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Get a block from memory manager for merged data. */
	    nlnnew = nlnold + Nzeros[jdfl] + nlnbfl;
                allamb(&cmmem,nlnnew,&ndxnew,nerr);
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Copy old data to beginning of new block. */
            copy((int*)cmmem.sacmem[ndxold],(int*)cmmem.sacmem[ndxnew],nlnold);
	    ndx =  nlnold;

	    /* -- Case of Gap */
	    if( Nzeros[jdfl] > 0 ){
		/* Append zeros to fill in gap if necessary. */
                fill(cmmem.sacmem[ndxnew]+ndx,Nzeros[jdfl],0.);
		ndx = ndx + Nzeros[jdfl];
		/* Append second data series. */
		copy( (int*)cmmem.sacmem[ndxbfl], (int*)(cmmem.sacmem[ndxnew]+ndx), nlnbfl );
	    }

	    /* -- Case of Overlap */
	    else if ( Nzeros[ jdfl ] < 0 ) {
		/* Append second data series except for overlap. */
		copy( (int*)(cmmem.sacmem[ndxbfl] - Nzeros[ jdfl ]),
		      (int*)(cmmem.sacmem[ndxnew]+ndx), nlnbfl + Nzeros[ jdfl ] );
		nlnnew-- ;
	    }

	    /* -- Case of two files that fit perfect. */
	    else
		copy( (int*)cmmem.sacmem[ndxbfl], (int*)(cmmem.sacmem[ndxnew]+ndx), nlnbfl );

	    /* -- Release old memory block. */
	    relamb( cmmem.sacmem, ndxold, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Change the header to reflect effect of merge. */
	    *npts = nlnnew;
	    *ennd = *begin + *delta*(float)( *npts - 1 );
	    extrma( cmmem.sacmem[ndxnew], 1, *npts, depmin, depmax, depmen );

	    /* -- Change DFM entries to reflect new block size. */
	    cmdfm.ndxdta[jdfl_][0] = ndxnew;
	    Nlndta[jdfl] = nlnnew;

	    /* -- Give file back to file manager. */
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

	sacToSeisMgr ( FALSE , FALSE , TRUE , nerr ) ;

L_8888:
	return;

} /* end of function */

