#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "mem.h"
#include "sam.h"
#include "hdr.h"
#include "dfm.h"
#include "extfunc.h"

void aphdrw () ;
void gdhdrw () ;
void irhdrw () ;
void fillNZ () ;
double tmGetEpochTime() ;
void zputc(char* str,int strlen,int* array,int pnumc);

void fdWhitenWrite( int * memptr , char * kprefix ,
				   float * userData , int newnpts ,
				   int nFreq , int * nerr )
{
	/* index sacmem for Amplitude, Phase, 
	   and the impulse response. */
        int tmpLong;
        int writehdr=0;
	int fileDescriptor = 0 , hdrindex , 
		 idx, jdx, jcomp, nlcmem, nlcdsk, nptwr, 
		 unused1 , unused2 , unused3 ;

	char kname[ MCPFN ] , ksuffix[ 3 ][ 6 ] ;

	float *bufout = NULL , *ptr , **amph = NULL ;
	void zwabs() ;

	*nerr = 0;

	/* allocate room for spectral data */
	amph = (float **) calloc ( 2 , sizeof( float ) ) ;
	if ( amph == NULL ) {
	    *nerr = 301 ;
	    goto L_ERROR ;
	}

	amph[ 0 ] = (float *) malloc ( ( nFreq ) * sizeof( float ) ) ;
	amph[ 1 ] = (float *) malloc ( ( nFreq ) * sizeof( float ) ) ;

	if ( amph[ 0 ] == NULL || amph[ 1 ] == NULL ) {
	    *nerr = 301 ;
	    goto L_ERROR ;
	}

	/* handle strings */
	if ( strlen ( kprefix ) > MCPFN - 4 )
	    kprefix[ MCPFN - 4 ] = '\0' ;
	strcpy ( ksuffix[ 0 ] , ".spec" ) ;
	strcpy ( ksuffix[ 1 ] , ".imp" ) ;
	strcpy ( ksuffix[ 2 ] , ".gd" ) ;


	/* fill the amplitude and phase array */
        for ( idx = 0 ; idx < nFreq ; idx++ ) {
            amph[ 0 ][ idx ] = cmmem.sacmem[ memptr[ 1 ] ][ idx ] ;
            amph[ 1 ][ idx ] = cmmem.sacmem[ memptr[ 2 ] ][ idx ] ;
        }

	/* Allocate block for headers. */
	allamb ( &cmmem, MHDR, &hdrindex , nerr ) ;
	if ( *nerr != 0 )
	    goto L_ERROR ;

	/* null the header */
	for ( idx = 0 ; idx < MFHDR ; idx++ )
	    cmhdr.fhdr[ idx ] = FUNDEF ;
	for ( idx = 0 ; idx < MNHDR ; idx++ )
	    cmhdr.nhdr[ idx ] = NUNDEF ;
	for ( idx = 0 ; idx < MIHDR ; idx++ )
	    cmhdr.ihdr[ idx ] = IUNDEF ;
	for ( idx = 0 ; idx < MKHDR ; idx++ )
	    strcpy ( kmhdr.khdr[ idx ] , "-12345  " ) ;

	/* fill some fields. */
	for ( idx = 0 ; idx < 9 ; idx++ )		/* user fields */
	    *( user0 + idx ) = userData[ idx ] ;

	switch ( (int) (*user0 + 0.5) ) {
	    case 1:  strcpy ( kuser0 , "lowpass " ) ;
		     break ;
	    case 2:  strcpy ( kuser0 , "highpass" ) ;
		     break ;
	    case 3:  strcpy ( kuser0 , "bandpass" ) ;
		     break ;
	    case 4:  strcpy ( kuser0 , "bandrej " ) ;
		     break ;
	    case 5:  strcpy ( kuser0 , "whiten  " ) ;
		     break ;
	    default: strcpy ( kuser0 , "-12345  " ) ;
		     break ;
	}

	switch ( (int) (*user1 + 0.5) ) {
	    case 1:  strcpy ( kuser1 , "Butter  " ) ;
		     break ;
	    case 2:  strcpy ( kuser1 , "Bessel  " ) ;
		     break ;
	    case 3:  strcpy ( kuser1 , "C1      " ) ;
		     break ;
	    case 4:  strcpy ( kuser1 , "C2      " ) ;
		     break ;
	    case 5:  strcpy ( kuser0 , "whiten  " ) ;
		     break ;
	    default: strcpy ( kuser1 , "-12345  " ) ;
		     break ;
	}

	*begin  = 0.0 ;					/* other fields */
	*sb     = 0.0 ;
	*nvhdr  = 6 ;
	*idep   = IUNKN ;
	*iztype = IB ;
	*leven  = TRUE ;
	*lpspol = TRUE ;
	*lovrok = TRUE ;
	*lcalda = FALSE ;

	for ( jdx = 0 ; jdx < 3 ; jdx++ ) {	/* loop between output files. */
 
	    /* fill other header fields specific to the data */
	    switch ( jdx ) {
		case 0:	aphdrw( newnpts , nFreq ) ;
			nlcmem = memptr[ 1 ] ;
			break ;
		case 1:	irhdrw( newnpts , nFreq ) ;
			nlcmem = memptr[ 0 ] ;
			break ;
		case 2: gdhdrw( newnpts , nFreq ) ;
			nlcmem = memptr[ 3 ] ;
			break ;
		default: goto L_ERROR ;
	    }

	    /* Get file name */
	    sprintf ( kname , "%s%s" , kprefix , ksuffix[ jdx ] ) ;

	    /* Open file */
	    znfile( &fileDescriptor , kname , MCPFN , "DATA" , 5 , nerr );
	    if ( *nerr )
		goto L_ERROR ;

	    /* Get ready to write header to disk */
	    nlcdsk = 0;
	    nptwr = MHDRFILE;

	    if ( ( bufout = (float *) malloc ( MHDRFILE * 4 ) ) == NULL ) {
		*nerr = 301;
		goto L_ERROR ;
	    }

	    /* move header into working memory */
	    copy ( (int*) cmhdr.fhdr , (int*) cmmem.sacmem[ hdrindex ] , MCMHDR );
	    zputc ( kmhdr.khdr[ 0 ] , 9 , &tmpLong , ( MCPW + 1 ) * MKHDR ) ;
            *(cmmem.sacmem[ hdrindex ] + MCMHDR) = tmpLong;
	    /* move header into output buffer */
	    map_hdr_out ( cmmem.sacmem[ hdrindex ] , bufout, &writehdr ) ;

	    /* write the headers */
	    zwabs( &fileDescriptor, bufout, nptwr, &nlcdsk, nerr );

	    free(bufout);
	    bufout = NULL ;

	    nlcdsk += nptwr;
	    nptwr = nFreq ;

	    /* Write data to disk */

	    switch ( jdx ) {
		case 0:	
			zwabs ( &fileDescriptor, amph[ 0 ] , nptwr, &nlcdsk, nerr ) ;
			nlcmem = memptr[ 2 ] ;
			nlcdsk += nptwr;
			zwabs ( &fileDescriptor, amph[ 1 ] , nptwr, &nlcdsk, nerr ) ;
			free ( amph[ 0 ] ) ;
			free ( amph[ 1 ] ) ;
			free ( amph ) ;
			amph = NULL ;
			break ;

		case 1:	zwabs( &fileDescriptor, cmmem.sacmem[nlcmem] ,
				nptwr, &nlcdsk, nerr );
			break ;
		case 2: zwabs( &fileDescriptor, cmmem.sacmem[nlcmem] ,
				nptwr, &nlcdsk, nerr ) ;
			break ;
	    }



	    /* Close file */
	    zclose ( &fileDescriptor , nerr ) ;
	    fileDescriptor = 0 ;
	} /* end for */

L_ERROR:

	if ( *nerr ) {
	    setmsg ( "ERROR" , *nerr ) ;
	    outmsg () ;
	    clrmsg () ;
	}

	if ( cmdfm.ndfl > 0 )
	    getfil ( 1 , TRUE , &unused1 , &unused2 , &unused3 , nerr ) ;

	if ( amph ) {
	    if ( amph[ 0 ] )
		free ( amph[ 0 ] ) ;
	    if ( amph[ 1 ] )
		free ( amph[ 1 ] ) ;
	    free ( amph ) ;
	}
	if ( bufout ) 
	    free ( bufout ) ;
	if ( fileDescriptor ) 
	    zclose ( &fileDescriptor , nerr ) ;
	relamb ( cmmem.sacmem , hdrindex , nerr );
}


void aphdrw ( int newnpts , int nFreq )
{
        *nsnpts = newnpts ;
        *npts = nFreq ;
        *sdelta = *user6 ;
        *delta = 1. / ( *sdelta * (float) ( nFreq ) ) ;
        *iftype = IAMPH ;

        strcpy ( kevnm , "FD: AMP/PH" ) ;
}


void gdhdrw ( int newnpts , int nFreq )
{
        *nsnpts = newnpts ;
        *npts = nFreq / 2 ;
        *sdelta = *user6 ;
        *delta = 1. / ( *sdelta * nFreq ) ;
        *iftype = ITIME ;

        strcpy ( kevnm , "FD: GROUP DELAY" ) ;
}

void irhdrw ( int newnpts , int nFreq )
{
        *nsnpts = nFreq ;
        *npts = newnpts ;
        *delta = *user6 ;
        *sdelta = 1. / ( *delta * (float) ( *npts ) ) ;
        *iftype = ITIME ;

        strcpy ( kevnm , "FD: IMPULSE" ) ;
}

