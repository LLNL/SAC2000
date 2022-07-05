#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "hdr.h"
#include <string.h>
#define	MPOLES	30
#define	MZEROS	30

#include "../../inc/mach.h"


	/*
	 * MODIFICATION HISTORY:
         *    970129:  Add parameter (0) to cnvatf and cnvati.  0 means that if
         *             a string of digits is too int, let it slide by.  maf 
	 */


void /*FUNCTION*/ polezero(nfreq, delfrq, xre, xim, subtyp, subtyp_s, 
	 nerr)
int nfreq;
double delfrq, xre[], xim[];
char *subtyp;   int subtyp_s;
int *nerr;
{
    /* ganz: 2/2012 changed key from 9 to 10 to fix seg fault */
	char key[10], kfile[MCPFN+1], kiline[MCMSG+1], kmcreq[9];
        char *kline;
	int lexist, lopen, lpoles, lzeros;
	int i, idx, ic, ic1, ic2, ipoles, itype, izeros, nc, ncerr, 
	 npoles, nzeros, numsave;
        FILE *nun;
	float temp1, temp2;
	double const_ ;
	complexf poles[MPOLES], zeros[MZEROS];
	void zbasename();
        char *s1;


	complexf *const Poles = &poles[0] - 1;
	double *const Xim = &xim[0] - 1;
	double *const Xre = &xre[0] - 1;
	complexf *const Zeros = &zeros[0] - 1;


        for( idx = 0 ; idx < MCPFN ; idx++ )
            kfile[ idx ] = ' ' ;
        kfile[ MCPFN ] = '\0' ;


	/*     generic transfer function - user supplies poles and zeros */
	/* - Search for the polezero file.  Search order is:
	 *   (1) current directory.
	 *   (2) global polezero directory. */
	lopen = FALSE;
	fstrncpy( kfile, MCPFN, subtyp, strlen(subtyp));
	zinquire( kfile, &lexist );
	if( lexist )
		goto L_5000;

	zbasename( kfile,MCPFN+1 );
	crname( kfile,MCPFN+1, KSUBDL, "polezeros",10, nerr );
	if( *nerr != 0 )
		goto L_8888;
	crname( kfile,MCPFN+1, KDIRDL, kmcreq,9, nerr );
	if( *nerr != 0 )
		goto L_8888;
	zinquire( kfile, &lexist );
	if( lexist )
		goto L_5000;

	/* - Raise error condition if macro file does not exist. */

	*nerr = 108;
	setmsg( "ERROR", *nerr );
	apcmsg( subtyp,subtyp_s );
	goto L_8888;

	/* - Set default values for constant, poles, and zeros. */

L_5000:
	const_ = 1.0;

	for( i = 1; i <= MZEROS; i++ ){
		Zeros[i] = flttocmplx( 0.0, 0.0 );
		}

	for( i = 1; i <= MPOLES; i++ ){
		Poles[i] = flttocmplx( 0.0, 0.0 );
		}

	/* - Open file. */

	zopens( &nun, kfile,MCPFN+1, "ROTEXT",7, nerr );
	if( *nerr != 0 )
		goto L_8888;
	lopen = TRUE;

	/* - Read and decode lines in file. */

	lpoles = FALSE;
	lzeros = FALSE;
L_6000:
        if(fgets(kiline,MCMSG+1,nun)==NULL) {
          if(feof(nun)) goto L_7000;
          goto L_9000;
	}

        /* remove leading blanks. */
        kline = kiline;
        while( (*kline == ' ') || (*kline == '\n') || (*kline == '\t') ) kline++;

        if ( (numsave = strlen(kline)) == 0 ) goto L_6000;
        if( kline[numsave-1] == '\n' ) kline[numsave-1] = ' ';
 
        /* eliminate tabs in the input line */
        for ( i = 0; i < numsave; i++) if(kline[i] == '\t') kline[i] = ' ';

	nc = indexb( kline,MCMSG+1 );
	ic = 0;
	poptok( kline, nc, &ic, &ic1, &ic2, &itype );
	strcpy( key, "        " );

	modcase( TRUE, kline+ic1 - 1, ic2 - ic1 + 1, key );

	if( strcmp(key,"CONSTANT") == 0 ){
		poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
/*                for( idx = 0 ; idx < ic2-ic1+2 ; idx++ )
		    s1[ idx ] = ' ' ;
		s1[ ic2-ic1+2 ] = '\0' ;
*/                s1[ic2-ic1+1] = '\0';

                const_ = atof( s1 ) ;
                free(s1) ;
                if( const_ == 0 || const_ == HUGE_VAL || 
                    const_ == -HUGE_VAL || isNaN( const_ ) ){
                        *nerr = 2118 ;
                        setmsg( "ERROR", *nerr ) ;
                        apcmsg( "Unrecognized Constant: ", 24 ) ;
                        apcmsg( s1 , strlen( s1 ) + 1 ) ;
                        goto L_8888 ;
                }
	}
	else if( strcmp(key,"POLES   ") == 0 ){
		poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
/*                for( idx = 0 ; idx < ic2-ic1+2 ; idx++ )
		    s1[ idx ] = ' ' ;
		s1[ ic2-ic1+2 ] = '\0' ;
*/                s1[ic2-ic1+1] = '\0';
		cnvati( s1, ic2-ic1 + 2, &npoles, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		if( *nerr != 0 )
			goto L_8888;
		if( npoles > MPOLES ){
			*nerr = 2109;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MPOLES );
			goto L_8888;
			}
		lpoles = TRUE;
		lzeros = FALSE;
		ipoles = 0;
		}
	else if( strcmp(key,"ZEROS   ") == 0 ){
		poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                s1[ic2-ic1+1] = '\0';
		cnvati( s1, ic2-ic1 + 2, &nzeros, 0, nerr ); /* add 0 before nerr. maf 970129 */
		free(s1);
		if( *nerr != 0 )
			goto L_8888;
		if( nzeros > MZEROS ){
			*nerr = 2109;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MZEROS );
			goto L_8888;
			}
		lpoles = FALSE;
		lzeros = TRUE;
		izeros = 0;
		}
	else if( strcmp(key,"*       ") == 0 ){
		}
	else if( lpoles ){
		if( ipoles < MPOLES ){
			ipoles = ipoles + 1;
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1, ic2- ic1 + 2, &temp1, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1, ic2- ic1 + 2, &temp2, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			Poles[ipoles] = flttocmplx( temp1, temp2 );
			}
		else{
			*nerr = 2108;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MPOLES );
			goto L_8888;
			}
		}
	else if( lzeros ){
		if( izeros < MZEROS ){
			izeros = izeros + 1;
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1, ic2- ic1 + 2, &temp1, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			poptok( kline, nc, &ic, &ic1, &ic2, &itype );
                        strncpy((s1=malloc(ic2-ic1+2)),kline+ic1 - 1,ic2-ic1+1);
                        s1[ic2-ic1+1] = '\0';
			cnvatf( s1,ic2 - ic1 + 2, &temp2, 0, nerr ); /* add 0 before nerr. maf 970129 */
			free(s1);
			if( *nerr != 0 )
				goto L_8888;
			Zeros[izeros] = flttocmplx( temp1, temp2 );
			}
		else{
			*nerr = 2109;
			setmsg( "ERROR", *nerr );
			apcmsg( subtyp,subtyp_s );
			apimsg( MZEROS );
			goto L_8888;
			}
		}
	else{
		*nerr = 2110;
		setmsg( "ERROR", *nerr );
		apcmsg( subtyp,subtyp_s );
		apcmsg( key,9 );
		goto L_8888;
		}
	goto L_6000;

	/* - Compute transfer function. */

L_7000:
	printf(" Extracting polezero response for %s, %s...\n", kstnm, kcmpnm);

	getran( nfreq, delfrq, const_, nzeros, zeros, npoles, poles, xre, 
	 xim );

L_8888:
	if( lopen )
		zcloses( &nun, &ncerr );
	return;

L_9000:
	*nerr = 114;
	setmsg( "ERROR", *nerr );
	apcmsg( kfile,MCPFN+1 );
	goto L_8888;

} /* end of function */

