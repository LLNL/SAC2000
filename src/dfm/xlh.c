#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/lhf.h"
void /*FUNCTION*/ xlh(nerr)
int *nerr;
{
	char kerase[41], kline[MCMSG+1], kresp[9], krpttx[MRPT][41], ktok[9], 
	 kwait[9];
	int lwait, lwdes, lwname, lwundf;
	int ic1, ic2, j, j_, jdfl, jrpt, jrpt_, jrpttx, jrpttx_, 
	 jsprpt, junk1, junk2, junk3, nc, nc1, nc2, nc3, nc4, nctx[MRPT], 
	 nctxm, nferr, nlscrn, nlw, nrpttx, ntused;
	void zgpmsg();
	static int iform = 1;
	static char kblank[41] = "                                        ";
        char *cattemp;
        char *strtemp1, *strtemp2, *strtemp3, *strtemp4, *strtemp5;
	int idx ;


	int *const Nctx = &nctx[0] - 1;



	/*=====================================================================
	 * PURPOSE:  To execute the action command LISTHDR.
	 *           This command lists header values to short output.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1309, 0901
	 *=====================================================================
	 * MODULE/LEVEL:  dfm/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    MCPW
	 *    dfm:     krpttp, nrpttp, irpttp, lwname, lwdes, lwundf, lstall
	 *             mpcd, ispcat, ispitm, nsprpt, nstrpt, npkrpt
	 *             icatf, icatn, icati, icatl, icatk, icata
	 *             istcat, istitm, ipkcat, ipkcat
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    dfm:     irpttp, lwname,lwdes, lwundf, lstall, ispcat, ispitm,
	 *             nsprpt
	 *    hdr:     Potentially all.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lclist, lclog2, lcchar, ictok,
	 *             hdrfld, gtoutm, vflist, getfil, gtoutm, getscreenatr
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    nlst:    Number of items in list being printed.
	 *    icatx:   Category of item currently being printed.
	 *    itemx:   Index of item currently being printed.
	 *    jcol:    Current column in line being formatted.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970425:  Fix bug so display fits in the window.  maf
	 *    970129:  Print file number (jdfl).  maf
	 *    961212:  All of the header variables are now in the default list.
	 *             Added INCLUSIVE option to show headers whether they are
	 *             defined or not.  maf
	 *    900507:  Fixed bug with an odd number of items being listed with
	 *             the two-column output option. (VAX/VMS bug fix.)
	 *    890104:  Now sending output to message handling system.
	 *    860930:  Added a wait mechanism after each full screen.
	 *    841026:  Extensive modifications made to entire subroutine.
	 *    820806:  Changed to newest set of parsing and checking functions.
	 *             Updated line formatting using F77 character constructs.
	 *    820119:  Fixed bug in listing KHDR values.
	 *    811029:  Changed floating point output to G8.1 format.
	 *    810528:  Added option to list only first file in dfl.
	 *    810223:  Added check for null data file list.
	 *    810120:  Changed to output message retrieval from disk.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
        for( idx = 0 ; idx < 8 ; idx++ )
            ktok[idx] = ' ' ;
        ktok[ 8 ] = '\0' ;

	cmhdr.llh = TRUE ;	/* currently executing listhdr command. maf 961212 */
	for( idx = 0 ; idx < MCMSG ; idx++ )
	    kline[ idx ] = ' ' ;
        kline[ MCMSG ] = '\0' ;

	/* PARSING PHASE: */

	jsprpt = 0;

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- "DEFAULT/PICKS/SPECIAL":  change type of header report. */
		if( lclist( (char*)kmlhf.krpttp,9, cmlhf.nrpttp, &cmlhf.irpttp ) ){
			if( cmlhf.irpttp == 1 || cmlhf.irpttp == 4 ){
				for( j = 1; j <= cmlhf.nstrpt; j++ ){
					j_ = j - 1;
					strcpy( kmlhf.krpt[j_], kmlhf.kstrpt[j_] );
				}
				cmlhf.nrpt = cmlhf.nstrpt;
			} /* end if( cmlhf.irpttp == 1 ... */
			else if( cmlhf.irpttp == 2 ){
				for( j = 1; j <= cmlhf.npkrpt; j++ ){
					j_ = j - 1;
					strcpy( kmlhf.krpt[j_], kmlhf.kpkrpt[j_] );
				}
				cmlhf.nrpt = cmlhf.npkrpt;
			} /* end if( cmlhf.irpttp == 2 ) */
			else if( cmlhf.irpttp == 3 ){
				for( j = 1; j <= cmlhf.nsprpt; j++ ){
					j_ = j - 1;
					strcpy( kmlhf.krpt[j_], kmlhf.ksprpt[j_] );
				}
				cmlhf.nrpt = cmlhf.nsprpt;
			} /* end if( cmlhf.irpttp == 3 ) */

		} /* end if( lclist( ... ) */

		/* -- "FILES ALL/nlist":  print all headers or only a subset. */
		else if( lckey( "FILES#$",8 ) ){
			if( lckey( "ALL$",5 ) ){
				cmlhf.lstall = TRUE;
			}
			else if( lcia( 1, cmdfm.ndfl, cmlhf.ilhlst, &cmlhf.nlhlst ) ){
				cmlhf.lstall = FALSE;
			}
		} /* end else if( lckey( "FILES#$",8 ) ) */

		/* -- "INCLUSIVE": print headers even if they are undefined. maf 961212 */
                else if ( lklog( "INC#LUSIVE$", 12, &cmhdr.linc ) )
                { /* do nothing */ }

		/* -- "COLUMNS n": change number of output columns. */
		else if( lkirc( "COLUMNS#$",10, 1, 2, &cmlhf.nlhcol ) )
		{ /* do nothing */ }

		/* -- "WNAME/WONAME":  change option to print name of header variable. */
		else if( lclog2( "WNAME#$",8, "WONAME#$",9, &lwname ) ){
			ictok( -1 );
			cfmt( "OBSOLETE OPTION:$",18 );
			cresp();
		}

		/* -- "WDES/WODES":  change option to print description of variable. */
		else if( lclog2( "WDES#$",7, "WODES#$",8, &lwdes ) ){
			ictok( -1 );
			cfmt( "OBSOLETE OPTION:$",18 );
			cresp();
		}

		/* -- "WUNDEF/WOUNDEF":  change option to list undefined header variables. */
		else if( lclog2( "WUNDEF#$",9, "WOUNDEF#$",10, &lwundf ) ){
			ictok( -1 );
			cfmt( "OBSOLETE OPTION:$",18 );
			cresp();
		}

		/* -- "FIRST": Obsolete keyword for first file only. */
		else if( lckey( "FIRST#$",8 ) ){
			cmlhf.lstall = FALSE;
			cmlhf.nlhlst = 1;
			Ilhlst[1] = 1;
		}

		else if( lcchar( MCPW, ktok,9, &ntused ) ){
			if( jsprpt < MSPRPT ){
				jsprpt = jsprpt + 1;
				strcpy( kmlhf.ksprpt[jsprpt - 1], ktok );
				cmlhf.nrpt = jsprpt;
				strcpy( kmlhf.krpt[cmlhf.nrpt - 1], ktok );
			}
			else{
				*nerr = 1309;
				setmsg( "ERROR", *nerr );
				apimsg( jsprpt );
			}
		} /* end else if( lcchar( MCPW, ktok,9, &ntused ) ) */

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

		}

	} /* end while */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 ) {
	    	cmhdr.llh = FALSE ;	/* no inter executing xlh(). maf 961212 */
		return;
	}

	/* - Save length of special report if needed. */

	if( jsprpt > 0 )
		cmlhf.nsprpt = jsprpt;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 ) {
		cmhdr.llh = FALSE ;	/* no inter executing xlh(). maf 961212 */
		return;
	}

	/* EXECUTION PHASE: */

	/* - Get screen attributes (number of lines per screen and
	 *   text to send to erase screen, if any.) */

	getalphainfo( &nlscrn, kerase,41 );
	if( nlscrn <= 0 )
		nlscrn = 23;

	if( cmlhf.lstall ){
		setinputmode( "ALL" );
		}
	else{
		setinputmode( "SELECT" );
		selectinputfiles( cmlhf.ilhlst, cmlhf.nlhlst );
		}

	nlw = 0;
	gettextwait( kwait,9 );
	lwait = memcmp(kwait,"ON",2) == 0;
	autooutmsg( TRUE );
	setmsg( "OUTPUT", 99 );

	jdfl = 0;
L_4000:
	if( nextinputfile( &jdfl ) ){
		getfil( jdfl, FALSE, &junk1, &junk2, &junk3, nerr );
		if( *nerr != 0 ) {
		    autooutmsg( FALSE );
		    cmhdr.llh = FALSE ;	/* no inter executing xlh(). maf 961212 */
		    return ;
		}
		if(lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 )){
		  aplmsg( " ",2 );
                  cattemp = malloc(7+(ic2-ic1+1)+7);
                  strcpy(cattemp, " FILE: ");
                  strncat(cattemp,kmdfm.kdfl+ic1 - 1,ic2-ic1+1);
		  sprintf ( cattemp , "%s - %d", cattemp, jdfl ) ;	/* added. maf 970129 */
		  aplmsg( cattemp, strlen ( cattemp ) + 1 );	/* strlen added. maf 970129 */
                  free(cattemp);
		  nc = ic2 - ic1 + 1;

                  memset(kline,'-',nc+6);
                  kline[nc+6]='\n';
                  kline[nc+7]='\0';

		  aplmsg( kline,MCMSG+1 );
		  nlw = nlw + 4;	/* increased 3 to 4.  maf 970425 */
	        }
		nrpttx = 0;
		nctxm = 0;
		for( jrpt = 1; jrpt <= cmlhf.nrpt; jrpt++ ){
			jrpt_ = jrpt - 1;
			nrpttx = nrpttx + 1;
			formhv( (char*)kmlhf.krpt[jrpt_],9, iform, (char*)krpttx[nrpttx - 1]
			 ,41, &nferr );
			if( nferr == 0 ){
				Nctx[nrpttx] = indexc( (char*)krpttx[nrpttx - 1],41, 
				 '=' );
				nctxm = max( nctxm, Nctx[nrpttx] );
				}
			else if ( !cmhdr.linc ) {	/* added cmhdr.linc.  maf 961212 */
				nrpttx = nrpttx - 1;
				}
			}
		if( cmlhf.nlhcol == 1 ){
			for( jrpttx = 1; jrpttx <= nrpttx; jrpttx++ ){
				jrpttx_ = jrpttx - 1;
				nc1 = 2 + nctxm - Nctx[jrpttx];
				nc2 = indexb( (char*)krpttx[jrpttx_],41 );

                                strtemp1 = malloc(nc1+1);
                                strtemp2 = malloc(nc2+1);
                                strncpy(strtemp1,kblank,nc1);
                                strncpy(strtemp2,krpttx[jrpttx_],nc2);
                                strtemp1[nc1] = '\0';
                                strtemp2[nc2] = '\0';

                                sprintf(kline," %s %s",strtemp1,strtemp2);
                  
                                free(strtemp1);
                                free(strtemp2);

				aplmsg( kline,MCMSG+1 );
				nlw = nlw + 1;
				if( lwait && (nlw >= (nlscrn - 2)) ){	/* changed 1 to 2.  maf 970425 */
					outmsg();
					clrmsg();
					setmsg( "OUTPUT", 99 );
					zgpmsg( "Waiting $",10, kresp,9 );
					upcase( kresp, 1, kresp,9 );
					nlw = 0;
					if( kresp[0] == 'K' || kresp[0] == 'Q' ) {
					    autooutmsg( FALSE );
					    cmhdr.llh = FALSE ;	/* no inter executing xlh(). maf 961212 */
					    return ;
					}
					else if( kresp[0] == 'G' ){
						if( strcmp(kerase,"                                        ") != 
						 0 )
							{
                                                        fprintf(MUNOUT," %s\n",kerase);
							}
						lwait = FALSE;
						}
					else if( kresp[0] == 'N' ){
						if( strcmp(kerase,"                                        ") != 
						 0 )
							{
                                                        fprintf(MUNOUT," %s\n",kerase);
							}
						goto L_4000;
						}
					}
				}
			}
		else{
			strcpy( krpttx[nrpttx], "                                        " );
			for( jrpttx = 1; jrpttx <= nrpttx; jrpttx += 2 ){
				jrpttx_ = jrpttx - 1;
				nc1 = 2 + nctxm - Nctx[jrpttx];
				nc2 = indexb( (char*)krpttx[jrpttx_],41 );
				nc3 = 2 + nctxm - Nctx[jrpttx + 1];
				nc4 = indexb( (char*)krpttx[jrpttx_ + 1],41 );
				if( nc4 > 0 ){
                                        strtemp1 = malloc(nc1+1);
                                        strtemp2 = malloc(nc2+1);
                                        strtemp3 = malloc(nc3+1);
                                        strtemp4 = malloc(nc4+1);

                                        strncpy(strtemp1,kblank,nc1);
                                        strtemp1[nc1] = '\0';
                                        strncpy(strtemp2,krpttx[jrpttx_],nc2);
                                        strtemp2[nc2] = '\0';
                                        strncpy(strtemp3,kblank,nc3);
                                        strtemp3[nc3] = '\0';
                                        strncpy(strtemp4,krpttx[jrpttx_ + 1],nc4);
                                        strtemp4[nc4] = '\0';
                                        if ((nc1+nc2) < 40 ) {
                                          strtemp5 = malloc(40-(nc1+nc2)+1);
                                          memset(strtemp5,' ',40-(nc1+nc2));
                                          strtemp5[40-(nc1+nc2)] = '\0';
                                          sprintf(kline," %s%s%s%s%s",
                                            strtemp1,strtemp2,strtemp5,strtemp3,strtemp4);
                                          free(strtemp5);
					}
                                        else {
                                          sprintf(kline," %s%s%s%s",
                                            strtemp1,strtemp2,strtemp3,strtemp4);
					}
                                        free(strtemp1);
                                        free(strtemp2);
                                        free(strtemp3);
                                        free(strtemp4);
					}
				else{
                                        strtemp1 = malloc(nc1+1);
                                        strtemp2 = malloc(nc2+1);
                         
                                        strncpy(strtemp1,kblank,nc1);
                                        strtemp1[nc1] = '\0';
                                        strncpy(strtemp2,krpttx[jrpttx_],nc2);
                                        strtemp2[nc2] = '\0';
                                        
                                        sprintf(kline," %s%s",strtemp1,strtemp2);

                                        free(strtemp1);
                                        free(strtemp2);

					}
				aplmsg( kline,MCMSG+1 );
				nlw = nlw + 1;
				if( lwait && (nlw >= (nlscrn - 1)) ){
					outmsg();
					clrmsg();
					setmsg( "OUTPUT", 99 );
					nlw = 0;
					zgpmsg( "Waiting $",10, kresp,9 );
					upcase( kresp, 1, kresp,9 );
					if( kresp[0] == 'K' || kresp[0] == 'Q' ) {
					    autooutmsg( FALSE );
					    cmhdr.llh = FALSE ;	/* no inter executing xlh(). maf 961212 */
					    return ;
					}
					else if( kresp[0] == 'G' ){
						if( strcmp(kerase,"                                        ") != 
						 0 )
							{
                                                        fprintf(MUNOUT," %s\n",kerase);
							}
						lwait = FALSE;
						}
					else if( kresp[0] == 'N' ){
						if( strcmp(kerase,"                                        ") != 
						 0 )
							{
                                                        fprintf(MUNOUT," %s\n",kerase);
							}
						goto L_4000;
						}
					}
				}
			}

		/* -- Loop on entries in input dfl. */
		goto L_4000;
		}

	/* - Turn automatic output mode off before returning. */

	autooutmsg( FALSE );

	cmhdr.llh = FALSE ;	/* no inter executing xlh() maf 961212 */
	return;

} /* end of function */

