#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifndef __STDC__
#  include "../../inc/complex.h"
#  include "../../inc/proto.h"
#endif

#include "../../inc/mach.h"
#include "../../inc/dfm.h"


void setmsg(char* ktype, int number);
void apcmsg(char *kalpha, int kalpha_s);
void outmsg(void);
void clrmsg(void);
void modcase(int upflag, char* input, int nchar, char* output);



void getprefs ( lauth , lphase )
int	lauth ,		/* 1 means read author list, 0 means don't */
	lphase ;	/* 1 means read header info, 0 means don't */
{
    /* Declare local variables. */
    char *temp , 
	  prefsLine [ 120 ] ;	/* line from preferences file */
    int   nAuthors = 0 , 	/* number of authors. */
	  idx ;
    FILE *pFile ;		/* pointer to prefs file */

    /*=====================================================================
     * PURPOSE:  To read pick preferences file.  called by inidfm, 
     *           xpickauthor, and xpickphase.
     *=====================================================================
     * INPUT ARGUMENTS:
     *    lauth    1 means read author list, 0 means don't
     *    lphase   1 means read header info, 0 means don't
     *=====================================================================
     * MODULE/LEVEL:
     *=====================================================================
     * GLOBAL INPUT:
     *=====================================================================
     * GLOBAL OUTPUT:
     *    DFM:     IAUTHORS, KAUTHORS, KTPH, KTAU
     *=====================================================================
     * SUBROUTINES CALLED:
     *=====================================================================
     * MODIFICATION HISTORY:
     *    970409:  Original version.  maf
     *===================================================================== */
    /* PROCEDURE: */

    /* Open the file to count the number of authors in list. */
    pFile = fopen ( kmdfm.kprefsFileName , "r" ) ;
    if ( pFile == NULL ) {
        setmsg ( "WARNING" , 108 ) ;
	apcmsg ( kmdfm.kprefsFileName , strlen ( kmdfm.kprefsFileName ) ) ;
        apcmsg ( ".  readcss will not read picks." , 32 ) ;
        outmsg () ;
        clrmsg () ;
        return ;
    }

    while ( fgets ( prefsLine , 120 , pFile ) && !isspace ( prefsLine[ 0 ] ) )
        nAuthors++ ;

    /* if reading authors */
    if ( lauth ) {
	fclose ( pFile ) ;  /* close file to reopen it from the begining. */

	/* free up space from previous authors ( if any ) */
	if ( kmdfm.kauthors != NULL ) {
	    for ( idx = 0 ; idx < cmdfm.iauthors ; idx++ )
		free( kmdfm.kauthors[idx] ) ;
	    free ( kmdfm.kauthors ) ;
	    cmdfm.iauthors = 0 ;
	} /* end if ( kmdfm.kauthors != NULL ) */

	/* allocate space for author names. */
	kmdfm.kauthors = (char **) calloc ( nAuthors , sizeof( char * ) ) ;
	if ( kmdfm.kauthors == NULL ) {
	    setmsg ( "ERROR" , 301 ) ;
	    apcmsg ( "  readcss will not read picks." , 31 ) ;
	    outmsg () ;
	    clrmsg () ;
	    return ;
	}
	for ( idx = 0 ; idx < nAuthors ; idx++ ) {
	    kmdfm.kauthors[ idx ] = ( char * ) calloc ( 16 , sizeof ( char ) ) ;
	    if ( kmdfm.kauthors[ idx ] == NULL ) {
		setmsg ( "ERROR" , 301 ) ;
		apcmsg ( "  readcss will not read picks." , 31 ) ;
		outmsg () ;
		clrmsg () ;
		nAuthors = idx ;
		goto L_8888 ;
	    }
	}

	/* Open the file to read it. */
	pFile = fopen ( kmdfm.kprefsFileName , "r" ) ;

	/* Read author names */
	for ( idx = 0 ; idx < nAuthors ; idx++ )
	{
	    char * firstWhiteSpace = NULL ;

	    fgets ( prefsLine , 120 , pFile ) ;
	    firstWhiteSpace = strpbrk ( prefsLine , " \n\t\v\f" ) ;
	    if ( firstWhiteSpace == NULL )
		prefsLine[15] = '\0' ;
	    else
		* firstWhiteSpace = '\0' ;

	    /* convert to lowercase for case insensitive comparisons. */
	    modcase ( FALSE , prefsLine , strlen( prefsLine ) , prefsLine ) ;

	    strcpy ( kmdfm.kauthors[idx] , prefsLine ) ;
	}

	/* Read blank line delimiter */
	fgets ( prefsLine , 120 , pFile ) ;
    } /* end if ( lauth ) */

    /* if reading header information */
    if ( lphase ) {
	/* Read default phases and authors for individual pick header vars. */
	for ( idx = 0 ; idx < 10 ; idx++ )
	{
	    int check ;

	    check = fscanf ( pFile , "t%*d\t%s\t%s\n" , kmdfm.ktPh[idx] , 
							kmdfm.ktAu[idx] ) ;
	    if ( check != 2 || strlen ( kmdfm.ktPh[idx] ) > 8 || 
			       strlen ( kmdfm.ktAu[idx] ) > 15 ) {
		setmsg ( "WARNING" , 125 ) ;
		apcmsg ( "  readcss will not read picks." , 31 ) ;
		outmsg () ;
		clrmsg () ;
		return ;
	    }

	    /* convert to lower case for case insensitive comparisons. */
	    modcase ( FALSE , kmdfm.ktAu[idx] , strlen( kmdfm.ktAu[idx] ) , 
			      kmdfm.ktAu[idx] ) ;
	}
    } /* end if ( lphase ) */

    if ( lauth )
	cmdfm.iauthors = nAuthors ;

    return ;

L_8888:
    /* on error free up allocated memory */
    for ( idx = 0 ; idx < nAuthors ; idx ++ ) 
	free ( kmdfm.kauthors[idx] ) ;

    free ( kmdfm.kauthors ) ;

    return ;
}
