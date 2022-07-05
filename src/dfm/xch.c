#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#define	MGDTTM	15

#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "lhf.h"

void iztypeMessage () ;


void /*FUNCTION*/ xch(nerr)
int *nerr;
{
	char khdrc[MKHDR][MCPW+1], ktemp[19], ktok[9];
	int lallt, lfound, lhdrc[MLHDR], lnumbr, log;
	int icat[MHDR], icatx, igdttm, ihdrc[MIHDR], item[MHDR], 
	 itemx, ival, j, j1, j2, jc, jdfl, jj, junk1, 
	 junk2, junk3, nc, nckhdr, ndaerr, ngdttm[MGDTTM][6], nhdrc[MNHDR], 
	 nia, nitem;
	float diff, fhdrc[MFHDR], fnumbr, vallt;
	static int icatg = -1;
        char *strtemp;

	/* variables added to allow file specification, maf 960812 */
        int	 doFile[MDFL] ;	/* 1 if the corresponding file is to be modified */
	int	 idx ;		/* index doFile during initialization */
        float	 rnum ;		/* number passed back from lctok */


	float *const Fhdrc = &fhdrc[0] - 1;
	int *const Icat = &icat[0] - 1;
	int *const Ihdrc = &ihdrc[0] - 1;
	int *const Item = &item[0] - 1;
	int *const Lhdrc = &lhdrc[0] - 1;
	int *const Nhdrc = &nhdrc[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To execute the action command CHNHDR.
	 *           This command changes values for header variables in memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    LHF:     ICATF, ICATN, ICATI, ICATL, ICATK, KIV(), MIV
	 *             NCKHDR
	 *    HDR:     MFHDR, MNHDR, MIHDR, MLHDR, MKHDR, MHDR,
	 *             FUNDEF, NUNDEF, IUNDEF, KUNDEF
	 *    DFM:     NDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     Potentially all.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, CERR, LCTOK, ICTOK,
	 *             LCREAL, LCKEY, LCINT, LCLIST, LCLOG2, LCCHAR,
	 *             GTOUTM, GETFIL, IDTTM, PUTFIL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    FHDRC:   Array used to store changes in FHDR array.
	 *    NHDRC:   Same for NHDR array.
	 *    IHDRC:   Same for IHDR array.
	 *    LHDRC:   Same for LHDR array.
	 *    KHDRC:   Same for KHDR array.
	 *    ICAT:    Stores "category" of each changed header field.
	 *    ITEM:    Stores "item number" of each changed header field.
	 *=====================================================================
	 * KNOWN ERRORS:
	 * - Current method uses too much local storage.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Initialize several state variables. */

	nitem = 0;
	lallt = FALSE;
	igdttm = 1;

	/* - Parse position dependant tokens, ie FILE; maf 960812 */

	if ( lctok ( ktok , 9 , &lnumbr , &rnum ) ) {	/* get next token */
	    upcase ( ktok , 9 , ktok , 9 ) ;	/* convert token to uppercase */

	    if ( strncmp ( ktok , "FILE" , 4 ) == 0 ) { /* if token is "FILE" */
		ictok ( 1 ) ; 				/* increment token */

        	/* initialize doFile to FALSE */
        	for ( idx = 0 ; idx < MDFL ; idx++ )
		    doFile[idx] = FALSE ;

		/* set specified values of doFile to TRUE */
		while ( lctok ( ktok , 9 , &lnumbr , &rnum ) && lnumbr ) {
                    ictok ( 1 ) ;
		    if ( rnum <= cmdfm.ndfl && rnum >= 1 ) 
		        doFile[ (int) rnum - 1 ] = TRUE ;
		    else { 
			char rnumString[10] ;

			sprintf ( rnumString , "%d" , (int) rnum ) ;

			setmsg ( "WARNING" , 1003 ) ;
			apcmsg ( rnumString , 9 ) ;
			outmsg () ;
		    } /* end else */
                } /* end while */

	    } /* end if (strncmp ... ) */

	    else {
        	/* no files specified, so do all; initialize doFile to TRUE */
        	for ( idx = 0 ; idx < MDFL ; idx++ )
        	    doFile[idx] = TRUE ;
	    } /* end else */

	} /* end if ( lctok ... ) */

	/* end parsing the file specification. maf 960812 */


	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- Get next token from command. */
	    if( lctok( ktok,9, &lnumbr, &fnumbr ) ){
		hdrfld( ktok,9, &icatx, &itemx, &lfound );
		/* --- If it is the name of a SAC header field. */
		if( lfound ){
		    /* ---- Increment token counter. */
		    ictok( 1 );

		    /* Block changes to certain header fields */
		       /* nvhdr, norid, nevid, npts, nsnpts, nwfid */
		    if ( icatx == cmlhf.icatn && itemx >= 7 && itemx <= 12 ) {
			/* Increment token counter. */
			ictok( 1 );
			setmsg ( "WARNING" , 1389 ) ;
			outmsg () ;
			clrmsg () ;
			continue ;
		    }

		    /* ---- Save category and item number. */
		    nitem = nitem + 1;
		    Icat[nitem] = icatx;
		    Item[nitem] = itemx;
		    /* ---- Depending upon what kind of header item (floating, integer,
		     *      alphanumeric, etc.), verify and save next token. */
		    if( icatx == cmlhf.icatf ){
			if( lcreal( &Fhdrc[itemx] ) )
			{ /* do nothing */ }
			else if( lckey( "UNDEF$",7 ) ){
			    Fhdrc[itemx] = cmhdr.fundef;
			}
			else if( lkia( "GMT$",5, 1, 6, &ngdttm[igdttm - 1][0], &nia ) ){
			    Icat[nitem] = icatg;
			    Fhdrc[itemx] = (float)( igdttm );
			    for( j = nia ; j < 6; j++ ){
				ngdttm[igdttm - 1][j] = 0.;
			    }
			    if( ngdttm[igdttm - 1][0] <= 99 ) {
				time_t t;
				char *tmptm;
				int century, year;

				t = time(NULL);
				tmptm = ctime(&t);

				year = atoi ( tmptm + 22 ) ;
				century = atoi ( tmptm + 20 ) - year ;

				if ( ngdttm[igdttm - 1][0] > year )
				    century -= 100 ;

				ngdttm[igdttm - 1][0] = century + ngdttm[igdttm - 1][0];
			    }
			    igdttm = igdttm + 1;
			}
			else{
			    cerr( 1001 );
			    goto L_8888;
			}
		    }
		    else if( icatx == cmlhf.icatn ){
			if( lcint( &Nhdrc[itemx] ) )
			{ /* do nothing */ }
			else if( lckey( "UNDEF$",7 ) ){
			    Nhdrc[itemx] = cmhdr.fundef;
			}
			else{
			    cerr( 1001 );
			    goto L_8888;
			}
		    }
		    else if( icatx == cmlhf.icati ){
			if( lclist( (char*)kmlhf.kiv,9, MIV, &ival ) ){
			    Ihdrc[itemx] = ival;
			}
			else if( lckey( "UNDEF$",7 ) ){
			    Ihdrc[itemx] = cmhdr.iundef;
			}
			else{
			    cerr( 1001 );
			    goto L_8888;
			}
		    }
		    else if( icatx == cmlhf.icatl ){
			if( lclog2( "TRUE$",6, "FALSE$",7, &log ) ){
			    Lhdrc[itemx] = log;
			}
			else if( lclog2( "YES$",5, "NO$",4, &log ) ){
			    Lhdrc[itemx] = log;
			}
			else if( lclog2( ".TRUE.$",8, ".FALSE.$",9, &log ) ){
			    Lhdrc[itemx] = log;
			}
			else if( lckey( "UNDEF$",7 ) ){
			    Lhdrc[itemx] = FALSE;
			}
			else{
			    cerr( 1001 );
			    goto L_8888;
			}
		    }
		    else if( icatx == cmlhf.icatk ){
			j1 = itemx;
			j2 = j1 + Nkhdr[itemx] - 1;
			if( lckey( "UNDEF$",7 ) ){
			    /* Disallow undefined kstnm and kcmpnm */
			    if ( itemx == 1 || itemx == 21 ) {
				nitem-- ;
				setmsg ( "WARNING" , 1390 ) ;
				outmsg () ;
				clrmsg () ;
				continue ;
			    }
			    strcpy( khdrc[j1 - 1], kmhdr.kundef );
			    for( j = j1 ; j < j2; j++ ){
				strcpy( khdrc[j], "        " );
			    }
			}
			else{
			    strcpy( ktemp, "                  " );
			    nckhdr = MCPW*Nkhdr[itemx];
			    if( lcchar( nckhdr, ktemp,19, &nc ) ){
                                if(strchr(ktemp,'$')!= NULL){
				    ictok(-1);
				    strncpy(ktemp,getarg(-1,&nc),min(nc,nckhdr));
				    ictok(1);
				}

				jc = 1;
				for( j = j1-1; j < j2; j++ ){
				    strncpy(khdrc[j],ktemp+jc-1,MCPW+1);
				    jc = jc+MCPW+1;
				    if ( /*Item[ j ]*/ j != 1 )
					khdrc[j][MCPW] = '\0';
				}
			    }
			}
		    }
		    else{
			*nerr = 901;
			setmsg( "ERROR", *nerr );
			apcmsg( "CHNHDR",7 );
			goto L_8888;
		    }
		}
		else if( lckey( "ALLT$",6 ) ){
		    if( lcreal( &vallt ) ){
			lallt = TRUE;
		    }
		    else{
			cerr( 1001 );
			goto L_8888;
		    }
		}
		else{
		    cfmt( "ILLEGAL OPTION:$",17 );
		    cresp();
		}
	    }
	    else{
		/* -- Bad syntax. */
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;

	/* EXECUTION PHASE: */

	/* - For each file in data file list: */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    if ( doFile [ jdfl - 1 ] ) {	/* file specification. maf 960812 */

		/* -- Get file from memory manager. */
		getfil( jdfl, FALSE, &junk1, &junk2, &junk3, nerr );
		if( *nerr != 0 )
		    goto L_8888;

		/* -- Update appropriate header fields. */
		for( j = 1; j <= nitem; j++ ){
		    if( Icat[j] == cmlhf.icatf ){
			Fhdr[Item[j]] = Fhdrc[Item[j]];
			if ( Item[ j ] >= 6 && Item[ j ] <= 20 )
			    iztypeMessage ( Item[ j ] , *iztype ) ;
		    }
		    else if( Icat[j] == icatg ){
			igdttm = (int)( Fhdrc[Item[j]] );
			ddttm( &ngdttm[igdttm - 1][0], nzdttm, &diff );
			Fhdr[Item[j]] = diff;
		    }
		    else if( Icat[j] == cmlhf.icatn ){
			Nhdr[Item[j]] = Nhdrc[Item[j]];
		    }
		    else if( Icat[j] == cmlhf.icati ){
			Ihdr[Item[j]] = Ihdrc[Item[j]];
		    }
		    else if( Icat[j] == cmlhf.icatl ){
			Lhdr[Item[j]] = Lhdrc[Item[j]];
		    }
		    else if( Icat[j] == cmlhf.icatk ){
			j1 = Item[j];
			j2 = j1 + Nkhdr[Item[j]] - 1;
			for( jj = j1-1; jj < j2; jj++ ){
			    strcpy( kmhdr.khdr[jj], khdrc[jj] );
			}
		    }
		}

		/* -- Change all time fields if requested. */
		if( lallt ){
		    *begin = *begin + vallt;
		    *ennd = *ennd + vallt;
		    if( *nzyear != cmhdr.nundef )
			idttm( nzdttm, -vallt, nzdttm );
		    if( *arrivl != cmhdr.fundef )
			*arrivl = *arrivl + vallt;
		    if( *fini != cmhdr.fundef )
			*fini = *fini + vallt;
		    if( *origin != cmhdr.fundef )
			*origin = *origin + vallt;
		    if( *t0 != cmhdr.fundef )
			*t0 = *t0 + vallt;
		    if( *t1 != cmhdr.fundef )
			*t1 = *t1 + vallt;
		    if( *t2 != cmhdr.fundef )
			*t2 = *t2 + vallt;
		    if( *t3 != cmhdr.fundef )
			*t3 = *t3 + vallt;
		    if( *t4 != cmhdr.fundef )
			*t4 = *t4 + vallt;
		    if( *t5 != cmhdr.fundef )
			*t5 = *t5 + vallt;
		    if( *t6 != cmhdr.fundef )
			*t6 = *t6 + vallt;
		    if( *t7 != cmhdr.fundef )
			*t7 = *t7 + vallt;
		    if( *t8 != cmhdr.fundef )
			*t8 = *t8 + vallt;
		    if( *t9 != cmhdr.fundef )
			*t9 = *t9 + vallt;
		}

		/* -- Recompute ending time if appropriate. */
		if( *leven )
		    *ennd = *begin + (float)( *npts - 1 )**delta;

		/* -- Recompute distance, azimuth, etc. if proper header fields are present. */
		if( (((*stla != cmhdr.fundef && *stlo != cmhdr.fundef) && 
		 *evla != cmhdr.fundef) && *evlo != cmhdr.fundef) && *lcalda ){
		    *dist = 0.;
		    *az = 0.;
		    *baz = 0.;
		    *gcarc = 0.;

		    distaz( *evla, *evlo, (float*)stla, (float*)stlo, 1, (float*)dist, 
		     (float*)az, (float*)baz, (float*)gcarc, &ndaerr );

		    /* -- When station=event location, az and baz are randomly set to some value. */

		    if( (*evla == *stla) && (*evlo == *stlo) ){
			*az = 0.;
			*baz = 0.;
		    }

		    if( ndaerr != 0 ){
			*dist = cmhdr.fundef;
			*az = cmhdr.fundef;
			*baz = cmhdr.fundef;
			*gcarc = cmhdr.fundef;
		    }
		}


		/* -- Give file back to memory manager. */
		putfil( jdfl, nerr );
		if( *nerr != 0 )
		    goto L_8888;

	    } /* end if ( doFile [ jdfl - 1 ] ). maf 960812 */
	} /* end for loop between files */

L_8888:
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    961111:  Changed variables *a, *b, *e, *f, and *o to *arrivl, 
	 *             *begin, *ennd, *fini, and *origin, respectively (see
	 *             hdr.h)  maf.
	 *    960812:  Added FILE option to allow file specification, maf
	 *    910808:  Added test for identical station-event location and set
	 *             AZ and BAZ values both to zero. 
	 *    900517:  Fixed VAX bug in changing inter header character variables.
	 *    840216:  Added ability to change header times to any GMT time.
	 *    821004:  Added automatic recalculation of distance, azimuth, etc.
	 *    820204:  Fixed bug in "ALLT" option.
	 *    820119:  Fixed bug in changing KHDR values.
	 *    811120:  Added recalculation of ENND everytime.
	 *    810630:  Added a special "ALLT' option to change all header times.
	 *    810120:  Changed to output message retrieval from disk.
	 *===================================================================== */

} /* end of function */

