#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "lhf.h"
#include "com.h"

#include "cssStrucs.h"
#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "smDataIO.h"
#include "gc.h"


void Index() ;
int lckeyExact(char* kkey,int kkey_s);
void alignFiles ( int *nerr );

void /* FUNCTION */ xsort ( int *nerr )
{
    /* declare variables */
    char ktemp[ 9 ], **kdata = NULL ;
    int notUsed , notUsed1 , notUsed2 , namelength ;
    int icat , item , lok ;
    int jsort , jdfl, jdx , idx ;
    int icomORroll, idirection , nSortOrder = 0 ;
    float *fdata = NULL ;
    int *ldata = NULL ;
    int *tempIdx , *currIdx = NULL ;
    int *realIdx = NULL ;
    char * knewnames = NULL , * new ;
    int Verbose = 0;

    DBlist tree ;

    ktemp[ 0 ] = '\0' ;

    /* PARSING MODE */

    while ( lcmore ( nerr ) ) {

	/* -- "COMMIT|RECALLTRACE|ROLLBACK": how to treat existing data */
	if ( lckeyExact ( "COMMIT" , 7 ) )
	    cmdfm.icomORroll = COMMIT ;
	else if (lckeyExact ( "RECALLTRACE" , 12 ) )
	    cmdfm.icomORroll = RECALL ;
	else if ( lckeyExact ( "RECALL" , 7 ) )
	    cmdfm.icomORroll = RECALL ;
	else if ( lckeyExact ( "ROLLBACK" , 9 ) )
	    cmdfm.icomORroll = ROLLBACK ;


	if ( !lcmore ( nerr ) )
	    break ;

	/* -- ASCEND */
	if ( lckey ( "ASCEND#$" , 9 ) ) {
	    if ( nSortOrder > 0 )
		cmdfm.idirection[ nSortOrder - 1 ] = Ascending ;
	    else {
		setmsg ( "WARNING" , 1384 ) ;
		outmsg () ;
		clrmsg () ;
	    }
	}

	/* -- DESCEND */
	else if ( lckey ( "DESCEND#$" , 10 ) ) {
	    if ( nSortOrder > 0 )
		cmdfm.idirection[ nSortOrder - 1 ] = Descending ;
	    else {
		setmsg ( "WARNING" , 1384 ) ;
		outmsg () ;
		clrmsg () ;
	    }
	}

	/* -- header names */
	else if( lcchar( MCPW, ktemp,9, &notUsed ) ){
	    if ( nSortOrder < MAXSORTFIELDS ) {
		strcpy ( kmdfm.ksort[ nSortOrder ] , ktemp ) ;
		cmdfm.idirection[ nSortOrder ] = Ascending ;
		nSortOrder++ ;
	    }
	}


	/* Bad syntax */
	else {
	    cfmt( "ILLEGAL OPTION:$",17 );
	    cresp();
	}
    } /* end while */


    /* if nSortOrder is > 0, then sort options were entered.  
	Otherwise, use sort options from the previous sort command. */
    if ( nSortOrder )
	cmdfm.nSortOrder = nSortOrder ;

    /* if no sort options have ever been entered, error out */
    if ( cmdfm.nSortOrder == 0 ) {
	*nerr = 1379 ;
	setmsg ( "ERROR" , *nerr ) ;
	return ;
    }
    
    /* - Commit or rollback data according to cmdfm.icomORroll */
    alignFiles ( nerr ) ;
    if ( *nerr )
	return ;


    /*  EXECUTION PHASE */


    tempIdx = (int *) malloc ( cmdfm.ndfl * sizeof(int) ) ;
    currIdx = (int *) malloc ( cmdfm.ndfl * sizeof(int) ) ;
    realIdx = (int *) malloc ( cmdfm.ndfl * sizeof(int) ) ;
    if ( !currIdx || !currIdx  || !realIdx )
	goto L_MEMORYERROR ;

    for ( jdx = 0 ; jdx < cmdfm.ndfl ; jdx++ )
	realIdx[ jdx ] = jdx ;

    for ( jsort = cmdfm.nSortOrder - 1 ; jsort >= 0 ; jsort-- ) {
	/* get info for current sort */
	hdrfld ( kmdfm.ksort[ jsort ] , strlen ( kmdfm.ksort[ jsort ] ) ,
		 &icat, &item, &lok ) ;

	if ( !lok || icat > cmlhf.icatk ) {
	    *nerr = 1381 ;
	    setmsg ( "ERROR" , *nerr ) ;
	    apcmsg ( kmdfm.ksort[ jsort ] , strlen ( kmdfm.ksort[ jsort ] ) ) ;
	    outmsg () ;
	    clrmsg () ;
	    return ;
	}

	if ( icat == cmlhf.icatf ) {
	    fdata = ( float * ) malloc ( cmdfm.ndfl * sizeof ( float ) ) ;
	    if ( !fdata )
		goto L_MEMORYERROR ;
	    for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ ) {
		getfil ( realIdx[jdfl]+1 , FALSE , &notUsed, &notUsed1, &notUsed2 , nerr ) ;
		fdata[ jdfl ] = Fhdr[ item ] ;
	    }
	    Index ((void*)fdata, cmdfm.ndfl, sizeof(float),
		    cmdfm.idirection[ jsort ] == Ascending ?
		    LessThanFloat : GreaterThanFloat, currIdx );
	    free ( fdata ) ;
	    fdata = NULL ;
	}
	else if ( icat == cmlhf.icatn || icat == cmlhf.icatl ) {
	    ldata = ( int * ) malloc ( cmdfm.ndfl * sizeof ( int ) ) ;
	    if ( !ldata )
		goto L_MEMORYERROR ;
	    for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ ) {
		getfil ( realIdx[jdfl]+1 , FALSE , &notUsed, &notUsed1, &notUsed2 , nerr ) ;
		ldata[ jdfl ] = icat == cmlhf.icatn ? Nhdr[ item ] : Lhdr[ item ] ;
	    }
	    Index ((void*)ldata, cmdfm.ndfl, sizeof(int),
                    cmdfm.idirection[ jsort ] == Ascending ?
		    LessThanLong : GreaterThanLong, currIdx );
	    free ( ldata ) ;
	    ldata = NULL ;
	}
	else if ( icat == cmlhf.icati || icat == cmlhf.icatk ) {
	    kdata = (char **) calloc ( cmdfm.ndfl , sizeof ( char * ) ) ;
	    if ( !kdata ) 
		goto L_MEMORYERROR ;

	    for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ ) {
		getfil ( realIdx[jdfl]+1 , FALSE , &notUsed, &notUsed1, &notUsed2 , nerr ) ;

		kdata[ jdfl ] = (char *) malloc ( 34 * sizeof ( char ) ) ;
		if ( !kdata[ jdfl ] )
		    goto L_MEMORYERROR ;

		if ( icat == cmlhf.icati )
		    strcpy ( kdata[jdfl] , kmlhf.kdiv[ Ihdr[ item ] - 1 ] ) ;
		else {
		    if ( item == 1 )
			strcpy ( kdata[jdfl] , kmhdr.khdr[ 0 ] ) ;
		    else if ( item == 2 ) {	/* kevnm has 17 chars instead of 8 */
			strncpy ( kdata[jdfl] , kmhdr.khdr[ 1 ] , 9 ) ;
			strcpy ( kdata[jdfl] + 9 , kmhdr.khdr[ 2 ] ) ;
		    }
		    else
			strcpy ( kdata[jdfl] , kmhdr.khdr[ item - 1 ] ) ;
		}
	    }
	    Index ((void*)kdata, cmdfm.ndfl, sizeof(char*),
                    cmdfm.idirection[ jsort ] == Ascending ?
		    LessThanStrg : GreaterThanStrg, currIdx );
	    for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ )
		free ( kdata [ jdfl ] ) ;

	    free ( kdata ) ;
	    kdata = NULL ;
	} /* end else */
	
	/* now rearrange realIdx according to currIdx, using tempIdx as a middleIdx */
	for ( jdx = 0 ; jdx < cmdfm.ndfl ; jdx++ ) 
	    tempIdx[ jdx ] = realIdx[ currIdx[ jdx ] ] ;
	for ( jdx = 0 ; jdx < cmdfm.ndfl ; jdx++ )
	    realIdx [ jdx ] = tempIdx[ jdx ] ;
    } /* end for ( jsort ) */

/**********************************************************************/
    tree = smGetDefaultTree () ;

    /* Collect Garbage */
    gcCollect ( tree ) ;


    /* pass index to SeisMgr in a call to re-order the files. */
    if ( !dblSortWfdiscList( tree, realIdx , cmdfm.ndfl ) ) {
	/* error handling */
	*nerr = 1383 ;
	setmsg ( "ERROR" , *nerr ) ;
	outmsg () ;
	clrmsg () ;
    }

    /* Produce a sorted list of file names. */
    namelength = indexb ( kmdfm.kdfl , MAXCHARS ) ;
    knewnames = (char *) malloc ( ++namelength * sizeof ( char ) ) ;
    if ( knewnames == NULL ) {
	*nerr = 301 ;
	setmsg ( "ERROR" , *nerr ) ;
	outmsg () ;
	clrmsg () ;
    }

    new = knewnames ;

    for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ ) {
	char * ptr = kmdfm.kdfl ;
	for ( jdx = 0 ; jdx < realIdx[ jdfl ] ; jdx++ )
	    do { ptr++ ; } while ( *ptr != ' ' ) ;

	do { 
	    *new = *ptr ;
	    new++ ;
	    ptr++ ;
	}while ( *ptr != ' ' ) ;

    } /* end for ( jdfl ) */

    *new = '\0' ;

    memcpy ( kmdfm.kdfl , knewnames , strlen ( knewnames ) ) ;

    /* free memory. */
    free ( knewnames ) ;
    free ( tempIdx ) ;
    free ( currIdx ) ;
    free ( realIdx ) ;

    SeisMgrToSac ( tree , FALSE , nerr, Verbose , TRUE ) ;

    return ;

L_MEMORYERROR:

    if ( kdata ) {
	for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ ) {
	    if ( kdata[ jdfl ] )
		free ( kdata[ jdfl ] ) ;
	}
	free ( kdata ) ;
    }

    if ( tempIdx ) free ( tempIdx ) ;
    if ( currIdx ) free ( currIdx ) ;
    if ( realIdx ) free ( realIdx ) ;
    *nerr = 301 ;
    setmsg ( "ERROR" , *nerr ) ;
    outmsg () ;
    clrmsg () ;

} /* end xsort() */

