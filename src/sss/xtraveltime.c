#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include "complex.h"
#include "proto.h"
#define	MBLKSZ	500
#define	MENTRY	40

#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "sss.h"
#include "tt.h"

#define FUNDEF (-12345.0)

void readtaup () ;
void apcmsg2(char* kalpha, int kalpha_s);

/*FUNCTION*/
void xtraveltime(nerr)
int *nerr;
{
	char kalpha[21], kcard[MCMSG+1], kcont[9], kdflin[MCMSG+1], kform[9];
	int lbcksp, lexpnd, lfree, lmore, lphase, lpicks = FALSE, ltoend;
	int idx, ic, ic1, ic2, index, iopch[MXTT], iphase, 
	 ittc1, ittc2, itype, jdx, jch, jdfl, jen, lastElement=360, n1, nblksz, 
	 nblksz2, nc, ncerr, nchar, ndcont, ndflin, ndflrq, ndform, ndxmem, 
	 nentry, newndx, nhlines, nmodel, nPickStart = 0, nttmsv, numch, numxch, 
	 numych, numsave;
        FILE *nun = NULL;
	float fentry[MENTRY], ttscale, ttValue;
        char *s1, kValue[ 21 ] ;


	static int nphaseNames = 0 ;	/* for use with TAUP options only */

	int lmodel = FALSE ,	/* was global, now it's local.  maf 960829 */
	   ltaup = FALSE ,	/* TRUE if input file was produced by taup_curve. */
	   lunitsSet = FALSE ;	/* used to change from the default units  
				   degrees for models and km for files maf 960829 */

	/* variables added to put traveltime into a blackboard variable. maf 970512 */
	int fileNumber ;
	int lbb = FALSE ;
	char bbName[33] ;


	float *const Fentry = &fentry[0] - 1;
	int *const Iopch = &iopch[0] - 1;



	/*=====================================================================
	 * PURPOSE:  To read in travel time curves from a file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 
	 *=====================================================================
	 * MODULE/LEVEL: SSS/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NSNDFL:  Save count of files in working storage [i].
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970808:  Globalized nhlines to keep it consistant.
	 *             Update the way the X data is allocated and filled. maf
	 *    970514:  Add bb option to allow one traveltime for one phase for
	 *             one waveform to be stored in a blackboard variable. maf
         *    970129:  Add parameter (1) to cnvfre.  1 means that if a string
         *             of digits is too int, warn user & end command.  maf 
	 *    960829:  Made iasp91 the default model for traveltime.  
	 *             Code also added to allow succeeding calls to 
	 *             traveltime to use current settings as default.
	 *             Also set default units: degrees for model and
	 *             kilometers for files.  The user can change
	 *             the units at the command line.
	 *    920722:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* initialize */
	ndflin = 0 ;		/* added.  maf 960829 */
	/* initialize bbName. maf 970514 */
	strcpy ( bbName , "                                " ) ;

        for( idx = 0 ; idx < 8 ; idx++ )
            kform[ idx ] = ' ' ;
        kform[ 8 ] = '\0' ;


	/* PARSING PHASE: */

	lmore = FALSE;
	nttmsv = 0;
	iphase = 0;
	lphase = FALSE;

	/* - Loop on each token in command: */

	while( lcmore( nerr ) ){

	    /* -- "MORE":  signifies addition of more files to current travel time curves
	     *             rather than replacement of current list with new one. */
	    if( lckey( "&MORE$",7 ) ){
		lmore = TRUE;
		nttmsv = cmtt.nttm;
	    }

	    /* -- "DIR CURRENT|name":  set the name of the default subdirectory. */
	    else if( lkchar( "DIR#$",6, MCPFN, kmdfm.krddir,MCPFN+1, &nchar ) ){
		if( memcmp(kmdfm.krddir,"CURRENT",7) == 0 || memcmp(kmdfm.krddir ,
		 "current",7) == 0 ){
		    fstrncpy( kmdfm.krddir, MCPFN, " ", 1 );
		}
		else if( kmdfm.krddir[nchar - 1] != KDIRDL ){
		    char dirDelimterString[ 2 ] ;
                    dirDelimterString[0] = KDIRDL;
                    dirDelimterString[1] = '\0';
		    subscpy( kmdfm.krddir, nchar, -1, MCPFN, dirDelimterString );
		}
	    }

	    /* -- "UNITS degrees/kilometers":  select units of travel time curves */
	    else if( lklist( "UNITS$",7, (char*)kmsss.kdwun,9, MDWUN, &cmtt.ittunit ) ){
		lunitsSet = TRUE ; /* maf 960829 */
	    }

	    /* -- "DEPTH depth": Depth at which to produce curves */
	    else if( lkreal( "DEPTH$",7, &cmtt.ttdep ) )
	    { /* do nothing */ }

	    /* -- "HEADER count": set number of lines to skip. */
	    else if( lkint( "HEADER$",8, &nhlines ) )
	    { 
		cmtt.nhlines = nhlines ;  /* update global.  maf 970808 */
	    }

	    /* -- "CONTENT string": set default wave. */
	    else if( lkchar( "&CONTENT$",10, MCMSG, kmdfm.kdcont,MCMSG+1, 
	     &ndcont ) ){
		modcase( TRUE, kmdfm.kdcont, ndcont, kmdfm.kdcont );
	    }

	    /* -- "MODEL string": set default model. */
	    else if( lkchar( "&MODEL$",8, MCPW, kmtt.kmodel,9, &nmodel ) ){
		lmodel = TRUE;	/* maf 960829 */
	    }

	    /* -- "FREE":  use free-field rather than formatted input. */
	    else if( lckey( "&FREE$",7 ) ){
		cmdfm.ldfree = TRUE;
	    }

	    /* -- "BB": put traveltime of first phase in list for the 
			specified waveform into the specified blackboard 
			variable.  maf 970514 */
	    else if ( lkint ( "BB$" , 4 , &fileNumber ) ) {
		if ( lcchar ( 32 , bbName , 31 , &nchar ) ) {
		    lbb = TRUE ;
		    bbName [ nchar ] = '\0' ;
		}
		else {
		    setmsg ( "WARNING" , 5123 ) ;
		    outmsg () ;
		    clrmsg () ;
		    lbb = FALSE ;
		}
	    }

	    else if ( lckey ( "PIC#KS$" , 8 ) ) {
		lpicks = TRUE ;
		lcint ( &nPickStart ) ;
	    }

	    /* -- "PHASE":  the rest are phases */
	    else if( lckey( "&PHASE$",8 ) ){
		lphase = TRUE;
	    }

	    /* -- "TAUP":  read a file produced by taup_curve. */
	    else if( lckey( "&TAUP$",8 ) ){
		ltaup = TRUE;
	    }

	    /* -- "FORMAT string": use formatted input and set default format. */
	    else if( lkchar( "&FORMAT$",9, MCMSG, kmdfm.kdform,MCMSG+1, &ndform ) ){
		cmdfm.ldfree = FALSE;
		modcase( TRUE, kmdfm.kdform, ndform, kmdfm.kdform );
	    }

	    /* -- "phaselist": add a phase to the list */
	    else if( lphase ){
		if( lcchar( MTTLEN, (char*)kmtt.kphases[iphase],6, &nchar ) )
		    iphase = iphase + 1;
	    }

	    /* -- "filelist":  define a new filelist (or add to old filelist). */
	    else if( lcdfl( kdflin,MCMSG+1, &ndflin ) ){
		if ( lmodel ) {
		    /* we have a model and a file.  today we aren't set up to do
		       both.  Display a message to that effect, and get on with
		       the model. everything within the else if is from maf 960829 */
		    setmsg ( "WARNING", 5120 ) ;
		    outmsg () ;
		} /* end if ( lmodel ) */
	    }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }

	} /* end while ( lcmore( nerr ) ) */

        /* - The above loop is over when one of two conditions has been met:
         *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
         *   (2) All the tokens in the command have been successfully parsed. */

        if( *nerr != 0 )
            goto L_8888;

	/* Set default model to iaspmodel.  maf 960829 */
	if ( lmodel == FALSE && ndflin <= 0 ) {
	    if ( cmtt.lpreviousModel ) 
		/* use same model used last time. */
	    	lmodel = TRUE ;
	    else if ( cmtt.previousFileNames != NULL && lphase == FALSE ) {
		/* use same file(s) used last time */
		strncpy ( kdflin , cmtt.previousFileNames , MCMSG ) ;
		ndflin = cmtt.npreviousFileNames ;
	    }
	    else {
		lmodel = TRUE ;
	    	strcpy ( kmtt.kmodel , "iasp91  " ) ;
	    }
	}  /* end if ( lmodel == FALSE && ndflin <= 0 ) */

	/* Disallow concurrent TAUP and MODEL options. */
	if ( ltaup && lmodel ) {
	    *nerr = 5124 ;
	    goto L_8888 ;
	}

	/* Next 20 lines allow next call to traveltime to default to current settings. maf 960829 */
	cmtt.lpreviousModel = lmodel ;

	cmtt.npreviousFileNames = 0 ;
	if ( cmtt.previousFileNames != NULL ) {
	    free ( cmtt.previousFileNames ) ;
	    cmtt.previousFileNames = NULL ;
	}

	if ( !lmodel ) {
	    /* Allocate space for .previousFileNames */
	    cmtt.previousFileNames = ( char * ) malloc ( ( sizeof ( kdflin ) < MCMSG ? 
		sizeof ( kdflin ) : MCMSG ) + 1 ) ;
	    if ( cmtt.previousFileNames == NULL ) {
		*nerr = 301 ;
		setmsg ( "ERROR" , *nerr ) ;
		outmsg ( ) ;
		goto L_8888 ;
	    }
	    strncpy ( cmtt.previousFileNames , kdflin , MCMSG ) ;
	    cmtt.npreviousFileNames = ndflin ;
	}

	/* set the default units unless specified in command line. maf 960829 */
	if ( !lunitsSet ) {
	    if ( lmodel )
		cmtt.ittunit = TTDEGREE ;
	    else
		cmtt.ittunit = TTKILOMETER ;
	} /* end if ( !lunitsSet ) */

	/* set default phases if no phases selected */
	if (lmodel && iphase == 0 && lphase == FALSE) {
	    lphase = TRUE;
	    strcpy(kmtt.kphases[0], "P    " );
	    strcpy(kmtt.kphases[1], "S    " );
	    strcpy(kmtt.kphases[2], "Pn   ");
	    strcpy(kmtt.kphases[3], "Pg   ");
	    strcpy(kmtt.kphases[4], "Sn   ");
	    strcpy(kmtt.kphases[5], "Sg   ");
	    iphase = 6;
	}


	if( iphase != 0 )
	    cmtt.nphases = iphase;

	if( cmtt.ttdep == 0.0 && cmdfm.ndfl > 0 ){
	    getfil( 1, FALSE, &n1, &n1, &n1, nerr );
	    cmtt.ttdep = *evdp/1000.0;
	}


	/* EXECUTION PHASE: */

	/* - Convert KDFLIN which may contain wild-cards, predefined file sets, etc.
	 *   into an expanded file list.  Echo expanded file list if requested. */


	/* - Release memory associated with old travel time curves */

	if( cmtt.nttm != nttmsv ){
	    for( idx = 1; idx <= cmtt.nttm; idx++ ){
		relamb( cmmem.sacmem, Ndxttx[idx], nerr );
		relamb( cmmem.sacmem, Ndxtty[idx], nerr );
	    } /* end for */
	} /* end if */

	/* - Release old TAUP phase names. */
	if ( kmtt.kphaseNames ) {
	    for ( idx = 0 ; idx < nphaseNames ; idx++ ) {
		if ( kmtt.kphaseNames[ idx ] )
		    free ( kmtt.kphaseNames[ idx ] ) ;
	    }
	    free ( kmtt.kphaseNames ) ;
	    kmtt.kphaseNames = NULL ;
	    nphaseNames = 0 ;
	}

	/* - Set the current file count for using read or read-more. */

	cmtt.nttm = nttmsv;
	if( lmodel ){
            float zero = 0.0;
	    iaspmodel( cmtt.ttdep, &zero, 1.0, 360, nerr );
	    goto L_7777 ;
	}

	/* if there is no alphanumeric data file, quit here.  maf 960829 */
	if ( ndflin <= 0 ) {
	    setmsg ( "WARNING" , 5121 ) ;
	    outmsg () ;
	    goto L_8888;
	}

	wildfl ( kmdfm.krddir,MCPFN+1 , kdflin,MCMSG+1 , ndflin ,
	  kmdfm.kdflrq,MAXCHARS , &ndflrq , &lexpnd ) ;
	if( cmdfm.lechof && lexpnd ){
	    setmsg( "OUTPUT", 0 );
	    ic1 = 0;
	    while ( lnxtcl( kmdfm.kdflrq,MAXCHARS, &ic1, &ic2 ) ){
                apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
	    }
	    wrtmsg( MUNOUT );
	}


	if( !lmore )
	    cmtt.nttm = 0;

	/* - Main loop on each input file: */

	ittc1 = 0;

	while ( lnxtcl( kmdfm.kdflrq,MAXCHARS, &ittc1, &ittc2 ) ){

	    /* -- Open input alphanumeric data file. */

            strncpy((s1=malloc(ittc2-ittc1+2)),kmdfm.kdflrq+ittc1 - 1, ittc2-ittc1+1);
            s1[ittc2-ittc1+1] = '\0';

	    zopens( &nun, s1, ittc2 - ittc1 + 2, "ROTEXT",7, nerr );

	    free(s1);

	    if( *nerr != 0 )
		goto L_8888;

	    /* If it's a TauP file, handle it separately. */
	    if ( ltaup ) {
		readtaup ( nun , &numych , nerr ) ;
		if ( *nerr )
		    goto L_9000 ;
		nphaseNames = numych ;
		goto L_5000 ;
	    }

	    /* -- Read first card.
	     * --- If is a description card, decode it into PHASE information
	     * --- Otherwise, backspace input file. */
	    strscpy( kcont, kmdfm.kdcont, 8 );
	    nc = indexb( kmdfm.kdform,MCMSG+1 );
	    if( kmdfm.kdform[0] == '(' && kmdfm.kdform[nc - 1] == ')' ){
		fstrncpy( kform, 8, kmdfm.kdform, min(nc,8));
	    }
	    else{
                fstrncpy( kform, 8, "(", 1 );
                fstrncpy( kform+1, 8-1, kmdfm.kdform, min(nc,6));
                fstrncpy( kform+1+min(nc,6), 8-1-min(nc,6), ")", 1);
	    }
	    lfree = cmdfm.ldfree;

            if(fgets( kcard,MCMSG+1,nun)==NULL)
		goto L_9000;
            if(kcard[(numsave=strlen(kcard)-1)] == '\n')
		kcard[numsave] = ' ';

	    nc = indexb( kcard,MCMSG+1 );
	    ic = 0;
	    upcase( kcard, nc, kcard,MCMSG+1 );
	    poptok( kcard, nc, &ic, &ic1, &ic2, &itype );
	    lbcksp = TRUE;
	    if( memcmp(kcard+ic1 - 1,"PHASE",5) == 0 ){
		jdx = 1;
		poptok( kcard, nc, &ic, &ic1, &ic2, &itype );
		while ( !( itype == 0 || (jdx + cmtt.nttm > MXTT) ) ) {
		    fstrncpy( kmtt.kttnm[jdx + cmtt.nttm - 1], 5, kcard+ic1 - 
		     1,min(ic2,MCMSG) - ic1 + 1);
		    jdx++ ;
		    poptok( kcard, nc, &ic, &ic1, &ic2, &itype );
		} /* end while */
	    } /* end if( memcmp(kcard+ic1 - 1,"PHASE",5) == 0 ) */
	    else if( lbcksp ){
		backspace( nun, 1L );
	    }

	    /* -- Skip over header lines */
	    for( idx = 1; idx <= cmtt.nhlines; idx++ ){	/* nhlines is now global. maf 970808 */
                if(fgets( kcard,MCMSG+1,nun)==NULL){
                    if(feof(nun))
			goto L_5000;
                    goto L_9000;
		} /* end if */
                if(kcard[(numsave=strlen(kcard)-1)] == '\n') kcard[numsave] = ' ';
	    } /* end for */

	    /* -- Determine number of entries per card if formatted option. */
	    if( !lfree )
		detnum( kform,9, &nentry );

	    /* -- Decode content information. */
	    decont( kcont,9, MXTT, &numch, &numxch, &numych, iopch, &ltoend, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* -- Prime it to read in first card to see how many entries are really there */

            if(fgets( kcard,MCMSG+1,nun)==NULL){
                if(feof(nun))
		    goto L_5000;
                goto L_9000;
	    }
            if(kcard[(numsave=strlen(kcard)-1)] == '\n') kcard[numsave] = ' ';


	    /* --- Convert card to list of floating point values.
	     *     Each card is in either free field or formatted. */
	    if( lfree ){
		cnvfre( kcard,MCMSG+1, MENTRY, &nentry, fentry, iopch, kalpha
		 ,21, 1, nerr );	/* add 1 before nerr. maf 970129 */
	    }
	    else{
		cnvfmt( kcard,MCMSG+1, kform,9, nentry, fentry, nerr );
	    }

	    /* -- Loop on each line in text file.
	     * --- Say something descriptive about data problems and get out. */
	    if( *nerr != 0 ){
		setmsg( "ERROR", *nerr );
		reperr( *nerr );
		proerr( nerr );
		goto L_8888;
	    }

	    /* --- Redetermine how many y channels there really are */
	    for( idx = 1; idx <= nentry; idx++ ){
		if( Iopch[idx] > 0 )
		    numych = Iopch[idx];
	    }

	    if( cmtt.ittunit == TTDEGREE ){	
		ttscale = RKMPERDG;
	    }
	    else{
		ttscale = 1.0;
	    }

	    /* --- Allocate memory block for X channel */
	    nblksz = MBLKSZ;
	    *npts = 0;
	    if( numxch == 1 ){
		allamb( &cmmem, nblksz, &ndxmem, nerr );
		if( *nerr != 0 )
		    goto L_8888;
	    }
	    else if( numxch > 1 ){
		*nerr = 1361;
		setmsg( "ERROR", *nerr );
		apcmsg( "Can only have one X channel per file.",38 );
	    }

	    /* -- Allocate memory blocks for y channel(s). */
	    if( numych >= 1 ){
		for( jdx = 1; jdx <= numych; jdx++ ){
		    Nttpt[jdx + cmtt.nttm] = 0;
		    if( numxch == 1 ){			/* case of one X channel.  maf 970808 */
			Ndxttx[jdx + cmtt.nttm] = ndxmem;
			Ltteven[jdx + cmtt.nttm] = FALSE ;
		    }
		    else {				/* case of no X channels.  maf 970808 */
			Ltteven[jdx + cmtt.nttm] = TRUE;
			Xttfirst[jdx + cmtt.nttm] = 0.0;
			Xttdel[jdx + cmtt.nttm] = 1.0*ttscale;
		    }
		    allamb( &cmmem, nblksz, &Ndxtty[jdx + cmtt.nttm], nerr );
		    if( *nerr != 0 )
			goto L_8888;

		} /* end for */
	    } /* end if ( numych >= 1 ) */
	    else{
		*nerr = 1362;
		setmsg( "ERROR", *nerr );
	    }

	    /* --- Say something descriptive about data problems and get out. */
L_4000:
	    if( *nerr != 0 ){
		setmsg( "ERROR", *nerr );
		reperr( *nerr );
		proerr( nerr );
		goto L_8888;
	    }

	    /* --- Fill in the channels, according to the the content vector. */

	    /* This block had been changed in 96, but has now been largely restored to
	       what it was in xtraveltime.c.orig.  maf 970808 */
	    *npts = *npts + 1;
	    for( jen = 1; jen <= nentry; jen++ ){
		jch = Iopch[jen];
		if( jch >= 0 ){
		    if( *npts > nblksz ){
			/* --- This block is filled to capacity, release all blocks associated 
			 *     with the current model */
			nblksz2 = 2*nblksz;
			/* --- Reallocate for x values */
			if( !Ltteven[cmtt.nttm + 1] ) {
                            index = Ndxttx[cmtt.nttm + 1];
                            reaamb( cmmem.sacmem, nblksz, nblksz2,
                              index, &newndx, nerr );
                            if( *nerr != 0 )
                                goto L_8888;
                            Ndxttx[cmtt.nttm + idx] = newndx;
			} /* end if( !Ltteven[cmtt.nttm + 1] ) */
			/* --- Reallocate for all y values */
			for( idx = 1; idx <= numych; idx++ ){
			    index = Ndxtty[cmtt.nttm + idx];
			    reaamb( cmmem.sacmem, nblksz, nblksz2, 
			    index, &newndx, nerr );
			    if( *nerr != 0 )
				goto L_8888;
			    Ndxtty[cmtt.nttm + idx] = newndx;
			} /* end for */
			nblksz = nblksz2;
		    } /* end if ( *npts > nblksz ) */
		    if ( jch == 0 ) 
			*(cmmem.sacmem[Ndxttx[cmtt.nttm + 1]]+*npts-1) = 
			  Fentry[ jen ] * ttscale;
		    else {
			*(cmmem.sacmem[Ndxtty[cmtt.nttm + jch]]+*npts-1) = Fentry[jen];
			Nttpt[cmtt.nttm + jch] = *npts;
		    }
		} /* end if ( jch >= 0 ) */
	    } /* end for ( jen ) */

	    /* -- Read in next card. */

            if(fgets( kcard,MCMSG+1,nun)==NULL){
                if(feof(nun))
		    goto L_5000;
                goto L_9000;
	    }
            if(kcard[(numsave=strlen(kcard)-1)] == '\n') kcard[numsave] = ' ';

	    /* --- Convert card to list of floating point values.
	     *     Each card is in either free field or formatted. */
	    if( lfree ){
		cnvfre( kcard,MCMSG+1, MENTRY, &nentry, fentry, iopch, kalpha
		 ,21, 1, nerr );	/* add 1 before nerr. maf 970129 */
	    }
	    else{
		cnvfmt( kcard,MCMSG+1, kform,9, nentry, fentry, nerr );
	    }
	    /* --- Loop on each card in input file. */
	    goto L_4000;


L_5000:	    /* -- Come to here on end-of-file. */

	    /* -- Close input file. */
	    zcloses( &nun, nerr );
	    if( *nerr != 0 )
		goto L_8888;
	    nun = NULL ;

	    /* -- Update current number of travel time curves. */
	    cmtt.nttm = cmtt.nttm + numych;

	} /* end while ( lnxtcl( kmdfm.kdflrq,MAXCHARS, &ittc1, &ittc2 ) ) */

	/* - Check again for a non-null DFL. */

	if( cmtt.nttm <= 0 ){
	    *nerr = 1301;
	    setmsg( "ERROR", *nerr );
	}


L_7777:
	/* If a blackboard variable was requested, set it here. maf 970514 */
	if ( lbb ) {
	    getfil ( fileNumber , FALSE, &n1, &n1, &n1, nerr );

	    ttValue = timecrossing ( cmmem.sacmem [ Ndxttx [ 1 ] ] + 1 ,
				     *dist ,
				     cmmem.sacmem [ Ndxtty [ 1 ] ] + 1 ,
				     Nttpt[ 1 ] ) ;

	    if ( ttValue != -12345.0 ) {
		sprintf ( kValue , "%f" , ttValue ) ;
		setbbv ( bbName , kValue, nerr, strlen ( bbName ) , 
			 strlen ( kValue ) ) ;
	    }
	    else {
		/* warning: no blackboard variable set. */
		setmsg ( "WARNING" , 5122 ) ;
		apcmsg ( bbName , strlen ( bbName ) + 1 ) ;
		outmsg () ;
		clrmsg () ;
	    }
	}  /* end if ( lbb ) */

	/* If Picks are requested, set them here.  maf 990426 */
	if ( lpicks ) {
	    for ( jdfl = 1 ; jdfl <= cmdfm.ndfl ; jdfl++ ) {
		getfil ( jdfl , FALSE , &n1, &n1, &n1, nerr ) ;
		for ( jdx = nttmsv+1 ; jdx <= cmtt.nttm ; jdx++ ) {
		    float * thisPick = t0 + nPickStart + jdx - 1 ;
		    char * thisKT = kmhdr.khdr[ 6 + nPickStart + jdx - 1 ] ;

		    if ( thisPick > t9 )
			break ;

		    * thisPick = timecrossing ( cmmem.sacmem [ Ndxttx [ jdx ] ] + 1 ,
						*dist ,
						cmmem.sacmem [ Ndxtty [ jdx ] ] + 1 ,
						Nttpt[ jdx ] ) ;
		    if ( *thisPick != FUNDEF && strlen ( kmtt.kphases[ jdx - 1 ] ) )
			sprintf ( thisKT , "%-8.8s" , kmtt.kphases[ jdx - 1 ] ) ;
		    else
			strcpy ( thisKT , "-12345  " ) ;
		} /* end for ( jdx ) */
		putfil ( jdfl , nerr ) ;
	    } /* end for ( jdfl ) */
	} /* end if ( lpicks ) */

L_8888:
	/* - Return. (Try to close alphanumeric data file just to be sure.) */

	if ( nun )
	    zcloses( &nun, &ncerr );

	return;

L_9000:
	*nerr = 114;
	setmsg( "ERROR", *nerr );
        apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
	goto L_8888;

} /* end of function */




void readtaup ( FILE *taupfile , int *ncurves , int *nerr )
{
	/* Declare local variables. */
	char buffer[ 121 ] , *bPtr , *phPtr ;
	char **phaseNames = NULL , **temp ;
	int idx, jdx ;

	/* Find first phase line. */
	do {
	    bPtr = fgets ( buffer , 120 , taupfile ) ;
	    if ( !bPtr ) {
		*nerr = 5125 ;
		goto L_ERROR ;
	    }
	} while ( *bPtr != '>' ) ;

	/* Loop between phases to count the number of datapoints. */
	for ( jdx = 0 ; bPtr && *bPtr == '>' && jdx+cmtt.nttm < MXTT-1 ; jdx++){
	    /* allocate phaseName */
	    temp = (char **) realloc ( phaseNames , (jdx+1) * sizeof (char *) );
	    if ( !temp ) {
		*nerr = 301 ;
		goto L_ERROR ;
	    }
	    phaseNames = temp ;
	    phaseNames[ jdx ] = (char *) calloc ( 26 , sizeof ( char ) ) ;
	    if ( !phaseNames[ jdx ] ) {
		*nerr = 301 ;
		goto L_ERROR ;
	    }

	    /* Get Phase Name */
	    for ( bPtr += 2 , phPtr = phaseNames[jdx] ; *bPtr != ' ' ;
		  bPtr++ , phPtr ++ )
		*phPtr = *bPtr ;
	    *phPtr = '\0' ;

	    /* count lines. */
	    bPtr = fgets ( buffer , 120 , taupfile ) ;
	    for ( cmtt.nttpt[ jdx ] = 0 ; bPtr && *bPtr != '>' && *bPtr != 'E' ;
		  cmtt.nttpt[ jdx ]++ ) {
		bPtr = fgets ( buffer , 120 , taupfile ) ;
	    }

	}
	*ncurves = jdx ;

	/* Go back to the beginning of the file. */
	rewind ( taupfile ) ;

	/* Prepare to read the data */
	do {
	    bPtr = fgets ( buffer , 120 , taupfile ) ;
	    if ( !bPtr ) {
		*nerr = 5125 ;
		goto L_ERROR ;
	    }
	}while ( *bPtr != '>' ) ;

	/* Now loop between phases to read the data. */
	for ( jdx = 0 ; jdx < *ncurves ; jdx++ ) {
	    /* allocate space for the X (distance) and Y (time) data */
	    allamb ( &cmmem , cmtt.nttpt[ jdx ] , &cmtt.ndxtty[ jdx ], nerr );
	    if ( *nerr )
		goto L_ERROR ;
	    allamb ( &cmmem , cmtt.nttpt[ jdx ] , &cmtt.ndxttx[ jdx ], nerr );
	    if ( *nerr )
		goto L_ERROR ;

	    /* Loop between datapoints, read data, convert km to degrees */
	    for ( idx = 0 ; idx < cmtt.nttpt[ jdx ] ; idx++ ) {
		/* read line of data */
		bPtr = fgets ( buffer , 120 , taupfile ) ;

		/* read X, convert to km */
		cmmem.sacmem[ cmtt.ndxttx[ jdx ] ][ idx ] =
		 atof ( bPtr ) * RKMPERDG ;

		/* find and read Y */
		while ( !isspace ( *bPtr ) )
		    bPtr++ ;
		cmmem.sacmem[ cmtt.ndxtty[ jdx ] ][ idx ] = atof ( bPtr ) ;
	    }
	}

L_ERROR:
	if ( *nerr ) {
	    setmsg ( "ERROR" , *nerr ) ;
	    outmsg () ;
	    if ( phaseNames ) {
		for ( ; jdx >= 0 ; jdx-- )
		    if ( phaseNames[ jdx ] )
			free ( phaseNames[ jdx ] ) ;
		free ( phaseNames ) ;
	    }
	    return ;
	}

	kmtt.kphaseNames = phaseNames ;
}
