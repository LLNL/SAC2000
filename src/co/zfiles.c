/*============================================================================
 * PURPOSE:  To obtain a list of those files in a directory which conform to a
 *           given UNIXlike regular expression.  Names of
 *           "hidden" files and sub-directories are not included.
 *============================================================================
 * INPUT ARGUMENTS:
 *    kdirin:  The directory in which to search.  [c]
 *    kpatrn:  The regular expression to be matched. [c]
 *============================================================================
 * OUTPUT ARGUMENTS:
 *    files:     Character array that will hold the list of files in the
 *                directory that match the regular expression (no path).  [c]
 *    nFiles:    Number of entries in "files".  [i]
 *    nErr:      Error flag.  [i]
 *============================================================================
 * LOCAL VARIABLES:
 *    pfd:       The pipe file identifier.  [ptr]
 *    comand:    The command line used to open the pipe.  [c]
 *    buf:       The buffer for pipe reads.  [c]
 *    idx:       Temporary variable to hold each length of "buf".  [i]
 *============================================================================
 * SUBROUTINES CALLED:
 *    UNIX:      strcpy, strcat, popen, strlen, pclose, strrchr
 *    SACLIB:    getline
 *============================================================================
 * MACHINE DEPENDENCIES:
 * - Method of accessing directory file.
 *============================================================================
 * MODIFICATION HISTORY:
 *    961031:    Added kpatrn, a string (containing wile card characters)
 *               which indicates the files to be opened. Reworked the way
 *               that strings are used.  maf
 *    961018:	 Overhauled to use dynamic memory allocation, and make
 *               files local to the calling function.  Removed the
 *               variables faLen and stringLen.  files is now a pointer
 *               to a pointer so that the pointer to the allocated space
 *               is passed by reference.  maf
 *    910703:    changed drLen, faLen to *drLen, *faLen
 *               because fortran2c passing of string lens seems buggy
 *============================================================================
 */



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

void setmsg(char* ktype, int number);
void outmsg(void);
void clrmsg(void);
int getlineSAC(FILE* pfd, char* pch, short maxlen);
FILE * popen (const char *command, const char *type);


int zfiles( kdirin, kpatrn , files , nFiles , nErr )
char *kdirin,
     *kpatrn,		/* Indicates the files to be opened.  maf 961031 */
     **files ;
int *nFiles, *nErr ;
{
    /* Declare local variables */
    FILE *pfd ;
    char *comand, buf[BUFSIZ] ;
    int idx , 			/* index a string */
	filesIncrement = 5000 ,	/* how much memory to allocate (or reallocate)
				   to *files, at a time.  maf 961018 */
	nIncrements = 1 ;	/* number of times filesIncrement bytes are
				   added to *files, default is once.  maf 961018 */

    /*  kpatrn was added, so that the ls would only return the
        names of the files of interest, instead of every name in the
        whole directory.  This saved lots of time, and is much shorter
        and more readable code.  maf 961031 */


    /* Allocate room for the command.  maf 961031 */
    comand = ( char * ) malloc ( strlen ( kdirin ) + strlen ( kpatrn ) + 7 ) ;
    if ( comand == NULL ) {
	*nErr = 301 ;
        setmsg ( "Error: Insufficient memory to read in this directory", *nErr ) ;
        outmsg () ;
	clrmsg () ;
 	return FALSE ;
    }

    /* Compose the command. much shorter than previous version.  maf 961031 */
    sprintf ( comand , "ls -d %s%s" , kdirin, kpatrn ) ;

    /* Initialize */
    *nFiles = 0 ;
    *nErr = 0 ;

    /* popen returns the directory list (ls) in the form of a FILE */
    pfd = popen ( comand, "r" );
    if ( pfd == NULL )
    {
	*nErr = 1 ;
	free ( comand ) ;
	return FALSE ;
    }


    /* allocate space for list of files */
	/* Note: files is passed back to the calling function.
		 The calling function is responsible for
		 freeing the memory allocated here. */
    *files = ( char * ) malloc ( filesIncrement ) ;
    if ( *files == NULL )
    {
	/* error handling */
	*nErr = 301 ;
	setmsg ( "Error: Insufficient memory to read in this directory", *nErr ) ;
	outmsg () ;
	clrmsg () ;
	pclose ( pfd ) ;
	free ( comand ) ;
	return FALSE ;
    }

    /* initialize files */
    strcpy ( *files , " " ) ;

    /*  look at pfd one line at a time.  if the current line is the
	name of a directory, ignore it, else write the filename to
	files.  files is a space delimited list of filenames
	including paths.  If ls -F put a '*' to indicate a file is
	executable, strip it off */
    /*  modified to allow wildcards in the pathname. maf 970108 */
    while ( getlineSAC(pfd,buf,BUFSIZ) != EOF )
    {
	idx = strlen(buf) - 1 ;
       	*nFiles += 1 ;

	strcat ( *files, buf ) ;
        strcat ( *files, " " ) ;

	/* Make sure that there are more than 80 bytes in *files */
	if ( strlen ( *files ) > nIncrements * filesIncrement - 80 )
	{
	    char * temp = NULL ;

	    temp = (char * ) realloc ( *files , ++nIncrements * filesIncrement ) ;
	    if ( temp == NULL )
	    {
		/* error handling */
		*nErr = 301 ;
		setmsg ( " Error: Insufficient memory" , *nErr ) ;
		outmsg () ;
		free ( *files ) ;
		pclose ( pfd ) ;
		free ( comand ) ;
		return FALSE ;
	    } /* end if ( temp == NULL ) */
	    *files = temp ;
	} /* end if ( strlen ( *files ) > nIncrements * filesIncrement - 80 ) */
    } /* end while */
    pclose ( pfd ) ;		/* close the pipe */
    strcat ( *files, " ") ;
    free ( comand ) ;
    return TRUE ;
} /* end zfiles */
