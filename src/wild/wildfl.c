#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"

void /*FUNCTION*/ wildfl(kdfdir, kdfdir_s, kdflin, kdflin_s, ndflin, 
	 kdflou, kdflou_s, ndflou, lexpnd)
char *kdfdir;   int kdfdir_s;
char *kdflin;   int kdflin_s;
int ndflin;
char *kdflou;   int kdflou_s;
int *ndflou;
int *lexpnd;
{
	char kdirin[MCPFN+1], kfile[MCPFN+1], kpatrn[MCPFN+1];
	char * kdirfl = NULL ;	/* string with names of all files in a directory,
				   which match the unix regular expression. 
				   replaces kmdfm.kdirfl. maf 961018 */
	int lFree = FALSE ;	/* TRUE if kdirfl needs to be freed at the end. maf 961018 */
	int ic1, ic2, ic3, ic4, idx, j, nc1, nc2, 
	 ncfdir, ndirfl, nerr = 0 ;
	int  zfiles();	/* zfiles retuned void *, it now returns int.  maf 961018 */
        char *s1;



	/*=====================================================================
	 * PURPOSE:  To convert a potentially wild-card laden input file list
	 *           to an expanded output list of file names.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    kdfdir:  Default input directory. [c]
	 *             Used if input file list has no directory part.
	 *             Set to blanks if current directory is to be used.
	 *    kdflin:  Input file list, possibly containing wild cards. [cl]
	 *    ndflin:  Number of entries in KDFLIN. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    kdflou:  Output file list after expanding wild cards. [cl]
	 *    ndflou:  Number of entries in KDFLOU. [i]
	 *    lexpnd:  Set to .TRUE. if the output list differs from the
	 *             the input list because of wild card expansion. [l]
	 *=====================================================================
	 * MODULE/LEVEL:  wild/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPFN, MODEFILECASE
	 *    dfm:     MDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lnxtcl, lwildc, getdir, zfiles, wild, lnumcl,
	 *             putcl
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970108:  Allows wildcards in the directory path now.  Also,
	 *             much of the string handling now uses C routines.  maf
	 *    961031:  Allows access to files in very large directories, and
	 *             at higher speeds.  Most of the changes here are in
	 *             support of changes in zfiles().  maf
	 *    910703:  fotran2c interface which adds string length is buggy,
	 *             changed to pass string lens explicitly to zfiles_();
	 *             changed zfiles call to accept directory name that
	 *             include dirname specified in the param dir
	 *    900821   Changed global variable MODE_FILE_CASE to MODEFILECASE
	 *    900611:  Changed first argument being passed to zfiles.
	 *             Added filename case conversion logic.
	 *    870619:  Fixed bug in building pathname.
	 *    860918:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900109
	 *===================================================================== */
	/* PROCECURE: */
	/* - Initialize the output file list. */
	memset ( kdflou , ' ' , kdflou_s - 1 );	/* FORTRAN initiallize */
	kdflou[0] = '\0' ;		/* C initiallize string. maf 970108  */
	*ndflou = 0;
	*lexpnd = FALSE;

	for( idx = 0 ; idx < MCPFN ; idx++ ) {
	    kdirin[ idx ] = ' ' ;
	    kfile[ idx ] = ' ' ;
	    kpatrn[ idx ] = ' ' ;
	}
	kdirin[MCPFN] = '\0' ;
	kfile[MCPFN] = '\0' ;
	kpatrn[MCPFN] = '\0' ;

	/* - Initialize input file list pointer. */

	ic1 = 0;

	/* - Loop on each entry in input file list. */
	  /* kdflin is a space delimited list of filenames possibly including paths
	     and wild cards, ic1 points is the index of the beginning of the current
	     filename and ic2 is the index of the end of the current filename. */
	while ( lnxtcl( kdflin,kdflin_s, &ic1, &ic2 ) ) {

		/* prepare s1 to see if entry contains any wild-cards */
                strncpy((s1=malloc(ic2-ic1+2)),kdflin+ic1 - 1,ic2 - ic1 + 1);
                s1[ic2-ic1+1] = '\0';

		if( lwildc( s1 , ic2 -ic1 + 2 ) ){	/* if there is/are wildcard(s) */
			/* --- Break entry into directory part and pattern part. */

			getdir( s1, ic2 - ic1 + 2, kdirin,MCPFN+1, kpatrn,MCPFN+1 );

			/* The following if, else if, else if stucture was overhauled. maf 970108 */
			/* --- If no directory name was typed, use the default one. */
			if ( strncmp ( kdirin , "        " , 8 ) == 0 ) 
			    strncpy ( kdirin , kdfdir , MCPFN+1 ) ;
                        /*     Else if an absolute directory was typed, just use it. */
			else if ( kdirin[ 0 ] == '/' )
			    { /* do nothing */ }
			/*     Else if a relative directory was typed, append it to kdfdir. */
			else if ( strncmp ( kdfdir , "        " , 8 ) != 0 )
			{
			    char kTemp[ MCPFN ] , * pTemp = NULL ;

			    pTemp = strrchr ( kdfdir , '/' ) ;

			    if ( pTemp != NULL ) {
				int iTemp ;

				nc1 = indexb ( kdirin , MCPFN + 1 ) ;  /* return beginning of padding */
				iTemp = nc1 < MCPFN - ( pTemp - kdfdir ) ?
					nc1 : MCPFN - ( pTemp - kdfdir ) ;
				strncpy ( kTemp , kdfdir , pTemp - kdfdir + 1 ) ;
				strncat ( kTemp + ( pTemp - kdfdir + 1 ) , kdirin , iTemp ) ;
				strncpy ( kdirin , kTemp , MCPFN + 1 ) ;
			    }
			} /* end else if ( strcmp ( kdfdir , "        " , 8 ) != 0 ) */

			    
			/* --- Perform case conversion of directory and pattern if necessary. */
			nc1 = indexb ( kdirin , MCPFN + 1 ) ;
			nc2 = indexb ( kpatrn , MCPFN + 1 ) ;
			if( MODEFILECASE > 0 ){
				modcase( TRUE, kdirin, nc1, kdirin ); /* convert to upper case */
				modcase( TRUE, kpatrn, nc2, kpatrn );
			}
			else if( MODEFILECASE < 0 ){
				modcase( FALSE, kdirin, nc1, kdirin ); /* convert to lower case */
				modcase( FALSE, kpatrn, nc2, kpatrn );
			}

			/* Here's a trick to make these strings behave like C strings. 
			   After zfiles is run, set these back to FORTRANesc strings. maf 961031*/
			kdirin[ nc1 < MCPFN ? nc1 : MCPFN ] = '\0' ;
			kpatrn[ nc2 < MCPFN ? nc2 : MCPFN ] = '\0' ;

			/*     if kdirfl has already been allocated, free it up
			 *     so zfiles can allocated it again. */
			if ( lFree ) {
			    free ( kdirfl ) ;
			    kdirfl = NULL ;
			    lFree = FALSE ;
			}

                        /* --- Get the list of files in the directory that
                         *     match the regular expression. */
			/* zfiles now returns int, maf 961018 */
			lFree = zfiles( kdirin, kpatrn, &kdirfl, &ndirfl, &nerr );
			if( nerr != 0 )
				goto L_8888;

			/* set these strings back to their FORTRANish mode for future use. maf 961031 */
                        kdirin[ nc1 < MCPFN ? nc1 : MCPFN ] = ' ' ;
                        kpatrn[ nc2 < MCPFN ? nc2 : MCPFN ] = ' ' ;

			if( MODEFILECASE > 0 )
				modcase( TRUE , kdirfl, strlen(kdirfl)+1, kdirfl );
			else if( MODEFILECASE < 0 )
				modcase( FALSE, kdirfl, strlen(kdirfl)+1, kdirfl );

			/* get the length of kdirin */
			cmdfm.ncdir = indexb( kdirin,MCPFN+1 );
			
			/* The following two lines replace an old for loop and allow 
			   wild cards in the directory names of read commands.  maf 970108 */
			strcat ( kdflou , kdirfl ) ;
			*ndflou += ndirfl ;
			*lexpnd = TRUE;

		} /* end if ( lwildc( s1 , ic2 -ic1 + 2 ) ) */
		/* -- See if entry is a previously defined filelist. */
		else if( lfilec( s1, *ndflou, kdflou, nerr ) )
			*lexpnd = TRUE;

		/* -- Otherwise, entry is a simple file name. Add to output. */
		else{
			/* --- Break entry into directory part and name part. */
                      
			getdir( s1 ,ic2- ic1 + 2, kdirin,MCPFN+1, kpatrn,MCPFN+1 );

			/* --- If no directory name was typed, use the default one.
			 *          if(kdirin.eq.' ')kdirin=kdfdir */
			cmdfm.ncdir = indexb( kdirin,MCPFN+1 );
			ncfdir = indexb( kdfdir,kdfdir_s );
			nc2 = indexb( kpatrn,MCPFN+1 );

			/* make arrays C-like for the following code. maf 970108 */
			kdirin[ cmdfm.ncdir < MCPFN ? cmdfm.ncdir : MCPFN ] = '\0' ;
			kdfdir[ ncfdir < MCPFN ? ncfdir : MCPFN ] = '\0' ;
			kpatrn[ nc2 < MCPFN ? nc2 : MCPFN ] = '\0' ;

			/*     the following if, else structure overhauled. maf 970108 */
			/* --- Recreate filename from directory and name parts. */
			if( ncfdir > 0 ){
				if( cmdfm.ncdir > 0 ){
				    /* if kdirin is an absolute path, just use it */
				    if ( kdirin[ 0 ] == '/' ) {
					strcpy ( kfile , kdirin ) ;
					strcat ( kfile , kpatrn ) ;
				    }
				    else {
					strcpy ( kfile , kdfdir ) ;
					strcat ( kfile , kdirin ) ;
					strcat ( kfile , kpatrn ) ;
				    }
				}
				else{
					strcpy ( kfile , kdfdir ) ;
					strcat ( kfile , kpatrn ) ;
				}
			}
			else{
				if( cmdfm.ncdir > 0 ){
					strcpy ( kfile , kdirin ) ;
					strcat ( kfile , kpatrn ) ;
				}
				else{
					strcpy ( kfile , kpatrn ) ;
				}
			}

			strcat ( kdflou , " " ) ;	/* these two lines rewritten in ... */
			strcat ( kdflou , kfile ) ;	/* common C commands.  maf 970108 */
			( *ndflou )++ ;

			/* return strings to their FORTRANish state.  maf 970108 */
			kdirin[ cmdfm.ncdir < MCPFN ? cmdfm.ncdir : MCPFN ] = ' ' ;
			kdfdir[ ncfdir < MCPFN ? ncfdir : MCPFN ] = ' ' ;
                        kpatrn[ nc2 < MCPFN ? nc2 : MCPFN ] = ' ' ;

		} /* end else associated with if( lwildc( s1 , ic2 -ic1 + 2 ) ) */

                free(s1);

		if( nerr != 0 )
			break ;

	} /* end while */

	kdflou[ strlen( kdflou ) ] = ' ' ;      /* make it a FORTRANish string. maf 970108 */

L_8888:

	if ( lFree )	     /* lFree is TRUE if kdirfl was allocated by zfiles, else FALSE */
	    free ( kdirfl ) ;	/* maf 961018 */
	return;

} /* end of function */

