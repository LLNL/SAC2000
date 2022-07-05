#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>
#include <strings.h>

#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

extern enum filetype { sacfi, alpha, xdr, segy } ftype ;
void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);
void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ readfl(ldata, lmore, lsdd, kdirin, kdirin_s,
	 kdflin, kdflin_s, ndflin, nerr)
int ldata, lmore, lsdd ;
char *kdirin;   int kdirin_s;
char *kdflin;   int kdflin_s;
int ndflin, *nerr;
{
	char kcdir[MCPFN+1], kfile[MCPFN+1], kopen[MCPFN+1], kpdir[MCPFN+1];
	int lexpnd, lrdrem, lheader;
	int ic1, ic2, iflag, idx, j0, j1, j2, jcomp,
	 jdfl, jdflrq, jstart, ncerr, ndflrq, 
	 ndflsv, ntused, nun, lswap[ MDFL ] ;
        char *cattemp;
        char *strtemp;
        FILE *fileun;
        XDR xdrs;

	for( idx = 0 ; idx < MDFL ; idx++ )
		lswap[ idx ] = 0 ;

	/*=====================================================================
	 * PURPOSE:  To read a filelist into memory.
	 *=====================================================================
	 * INTPUT ARGUMENTS:
	 *    ldata:   Set to .TRUE. if header and data are to be read. [l]
	 *             Set to .FALSE. if only header (no data) are to be read.
	 *    lmore:   Set to .TRUE. if these files are to be appended to the
	 *             ones in memory, .FALSE. if they are to replace them. [l]
	 *    lsdd:    Set to .TRUE. if these files are are in SDD format,
	 *             Set to .FALSE. if are in any other format. [l]
	 *    kdirin:  Default input directory to use if filename does not
	 *             contain a directory part. [c]
	 *    kdflin:  Input file list. [cl]
	 *    ndflin:  Number of entries in kdflin. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *              1301 = No data files read in.
	 *              1320 = Available memory too small to read files.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     MDFL, KECBDF, NCOMP, NLNDTA
	 *    HDR:     MHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     NDSDFL, KDSDFL
	 *    MEM:     SACMEM
	 *    HDR:     All.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LCDFL,
	 *             ZGTFUN, CLEARWS, CLEARDS, ALLAMB, RDHDR, ZCLOSE, DEFMEM,
	 *             RDDTA, GTOUTM, SETRNG
	 *             GETDSFLNDX, NXTDSNDXAVAIL
	 *             LFILESOK
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NDFLSV:   Number of files in previous DFL. [i]
	 *    NUN:      Fortran unit used for reading files into DFL. [i]
	 *    JSTART, JSTOP:   Limits used in loops on data file list. [i]
	 *              These change depending upon value of MORE option
	 *              and whether some of files in DFL can't be read.
	 *    IFLAG:    Used to keep track of the state of the read. [i]
	 *    JDFL:     Index to file currently being read in memory. [i]
	 *    JDSNDX:   Index in data set storage for the file. [i]
	 *    JDSFLNDX: File number position in the current data-set. [i]
	 *    LRDREM:   Flag used when one or more files in DFL cant be read,
	 *              but the remainder of the files are to be read. [l]
	 *    KFILE:    Name of file currently being read. [c]
	 *    KPDIR:    Previous directory part of file name. [c]
	 *    KCDIR:    Current directory part of file name. [c]
	 *              Above two used only when echoing filelist.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    920619:  Changed lfilesok args to use kdflrq, includes directory,
	 *             and use " " for source directory. Bug introduced while
	 *             fixing READ DIR option.
	 *    920618:  Commented redundent appends that were probably added when
	 *             readcss was implemented. Did not cause a problem unless
	 *             "DIR" option was used in the read. Fixes bug reported 
	 *             6/15/92 by Owens and others.
	 *    920501:  Added memory delete on NOFILES and MEMORY DELETE READERR
	 *             contitions.
	 *    920418:  Added call to lfilesok, and moved memory setup stuff 
	 *             into logical function, lfilesok. This avoids memory 
	 *             shuffle when no files match the requested filelist.
	 *    910115:  Added file count for current data-set.
	 *    910826:  Changed kopen = ""  ---> kopen=' ' at 2 places to improve
	 *             portability to the DEC 2000 workstations,
	 *             per Gyu-sang Jang at UC Davis.
	 *    910813:  Added multiple data-set stuff; changing many of the data
	 *             structure names used for storing information about 
	 *             pointers, component count, file size, etc.
	 *
	 *             Some of this may seem a little backwards in that some
	 *             subroutines load info into working-storage, and the info
	 *             is then copied to permanent data-set storage; But many 
	 *             subroutines use the working storage and so this apporach 
	 *             avoids making changes in many other subroutines.
	 *
	 *    910610:  Fixed bug in opening file not referenced from pwd
	 *    910222:  Better handling of bombed allamb -- back out to ndflsv.
	 *    890417:  Fixed bug in processing of errors in read list.
	 *    880308:  Added LDATA subroutine argument.
	 *    870915:  Temporarily softened failure mode when memory requirement
	 *             exceeds available room.  Still need to add buffered mode.
	 *    870908:  Fixed bug in do loops limits after file deletion.
	 *    870626:  Extracted from xr subroutine.
	 *    860918:  Changed to character lists for data file list storage.
	 *    860130:  Changed to new message handling package.
	 *    850409:  Major rewrite due to addition of memory manager.
	 *    850321:  Fixed bug involving MORE option and null DFL.
	 *    830930:  Moved range calculation into it's own subroutine.
	 *    811230:  Added MORE option.
	 *    810708:  Added logic to compute range of dependent variable.
	 *    810120:  Changed to output message retrieval from disk. 
	 *    800512:  Reworked error messages.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850114
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	for( idx = 0 ; idx < MCPFN ; idx++ ) {
	    kcdir[ idx ] = ' ' ;
	    kfile[ idx ] = ' ' ;
	    kopen[ idx ] = ' ' ;
	    kpdir[ idx ] = ' ' ;
	}
	kcdir[MCPFN] = '\0' ;
	kfile[MCPFN] = '\0' ;
	kopen[MCPFN] = '\0' ;
	kpdir[MCPFN] = '\0' ;


	/* - Set up indices and flags depending upon lmore flag.
	 * - Echo out what type of read this is, and to what data-set. */

	if( lmore ){
	    ndflsv = cmdfm.ndfl;
	}
	else{
	    ndflsv = 0;
	}

	/* - Convert KDFLIN which may contain wild-cards, predefined file sets,
	 *   etc. into an expanded file list. */

	wildfl( kdirin,kdirin_s, kdflin,kdflin_s, ndflin, kmdfm.kdflrq
	 ,MAXCHARS , &ndflrq, &lexpnd );
	fstrncpy( kpdir, MCPFN, " ", 1);

	/* - Echo expanded filelist if requested. */

	if( (cmdfm.lechof && lexpnd) && ndflrq > 0 ){
	    setmsg( "OUTPUT", 0 );
	    ic1 = 0;

	    /* -- Loop until all pathnames in expanded filelist 
		  have been processed. */
	    while ( lnxtcl( kmdfm.kdflrq,MAXCHARS , &ic1, &ic2 ) ){

		/* -- Break pathname into directory and filename parts. */

                strtemp = malloc(ic2-ic1+2);
                strncpy(strtemp,kmdfm.kdflrq+ic1 - 1,ic2-ic1+1);
                strtemp[ic2-ic1+1] = '\0';

		getdir( strtemp, ic2 - ic1 + 2, kcdir,MCPFN+1, kfile,MCPFN+1 );

                free(strtemp);

		/* -- Echo the filename part if there is no directory part. */
		if( memcmp(kcdir," ",1) == 0 ){
		    apcmsg( kfile,MCPFN+1 );
		}
		/* -- Prepend the filename part with some special characters if
		 *    directory part is same as that of the previous file. */
		else if ( memcmp ( kcdir , kpdir , min ( strlen ( kcdir ) ,
			  strlen ( kpdir ) ) ) == 0 ){
                    cattemp = malloc(3+strlen(kfile)+1);
                    strcpy(cattemp,"...");
                    strcat(cattemp,kfile);
		    apcmsg( cattemp, 3+strlen(kfile)+1 );
                    free(cattemp);
		}
		/* -- Echo complete pathname if directory part is different. */
		else{
                    apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
		    strcpy( kpdir, kcdir );
		}
	    } /* end if( lnxtcl( kmdfm.kdflrq,MAXCHARS , &ic1, &ic2 ) ) */
	    wrtmsg( MUNOUT );
	} /* end if( (cmdfm.lechof && lexpnd) && ndflrq > 0 ) */

	/* -- Test to see whether to expect a sac header. */
	if ( ftype == alpha || ftype == segy || lsdd == TRUE )
	    lheader = FALSE ;
	else
	    lheader = TRUE ;

	/* -- Test to see that at least one file exists. */
	if( !lfilesok( kmdfm.kdflrq,MAXCHARS , ndflrq, " ",2, lmore, 
	    lheader, ftype == xdr, nerr ) ){

	    /* --- If destroying files in memory, */
	    if( strcmp(kmdfm.kecmem,"DELETE  ") == 0 )
		cleardfl( nerr );
	    *nerr = 1301;
	    goto L_8888;
	}

	/* - Test number of files requested, limit read to max files SAC can
	 *   handle.  Spit out a message about not being able to read all files
	 */
	if( ndflrq > MDFL ){
	    ndflrq = MDFL;
	    clrmsg();
	    setmsg( "OUTPUT", 0 );
	    apcmsg( "Max files: reading first ",26 );
	    apimsg( MDFL );
	    apcmsg( " files.",8 );
	    wrtmsg( MUNOUT );
	    clrmsg();
	}

	jstart = 1;
	lrdrem = FALSE;
	cmdfm.ndfl = ndflsv + ndflrq;

L_1800:
	iflag = 0 ;
	/* - If it's alpha files, handle that separately. */
	if ( ftype == alpha ) {
	    cmdfm.nreadflag = LOW ;
	    for ( jdflrq = jstart ; jdflrq <= ndflrq ; jdflrq++ ) {
		int nlen, ndx1, ndx2 ;
		jdfl = ndflsv + jdflrq ;

		/* -- Get name of requested file and store in data file list. */
		lnumcl( kmdfm.kdflrq,MAXCHARS , jdflrq, &ic1, &ic2 );
		fstrncpy( kfile, MCPFN, kmdfm.kdflrq+ic1 - 1, ic2 - ic1 + 1);

		rdci ( jdfl , kfile , MCPFN , &nlen, &ndx1, &ndx2, nerr ) ;

		if ( *nerr ) {
		    strcpy ( kmdfm.kecbdf , "WARNING " ) ;
		    goto L_4000 ;
		}
	    }
	    *nerr = 0;
	    goto L_4000 ;
	}

	/* - If it's segy files, handle that separately. */
        if ( ftype == segy ) {
	    cmdfm.nreadflag = LOW ;
            for ( jdflrq = jstart ; jdflrq <= ndflrq ; jdflrq++ ) {
                int nlen, ndx1, ndx2 ;
                jdfl = ndflsv + jdflrq ;

                /* -- Get name of requested file and store in data file list. */
                lnumcl( kmdfm.kdflrq,MAXCHARS , jdflrq, &ic1, &ic2 );
                fstrncpy( kfile, MCPFN, kmdfm.kdflrq+ic1 - 1, ic2 - ic1 + 1);
		terminate ( kfile ) ;

                rdsegy ( jdfl , kfile , &nlen, &ndx1, &ndx2, nerr ) ;

                if ( *nerr ) {
                    strcpy ( kmdfm.kecbdf , "WARNING " ) ;
                    goto L_4000 ;
                }
            }
            *nerr = 0;
            goto L_4000 ;
        }

	/* - Read headers into memory. */

L_2000:
	iflag = 1;
	for( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ){
	    jdfl = ndflsv + jdflrq;

	    /* -- Get name of requested file and store it in data file list. */
	    lnumcl( kmdfm.kdflrq,MAXCHARS , jdflrq, &ic1, &ic2 );
	    fstrncpy( kfile, MCPFN, kmdfm.kdflrq+ic1 - 1, ic2 - ic1 + 1);
	    putcl( kmdfm.kdfl,MAXCHARS, kfile,MCPFN+1, nerr );
	    if( *nerr != 0 )
		goto L_4000;

	    /* -- Open file. */
            if( ftype == xdr ) {
                znfiles(&fileun, kfile, MCPFN+1, "TEXT", 5, nerr);
                if( *nerr == 0 ){
                    xdrstdio_create(&xdrs, fileun, XDR_DECODE);
		}
		else{
                    goto L_4000;
		}
	    }
	    else{
		zopen_sac( &nun, kfile,MCPFN+1, "RODATA",7, nerr );
		if( *nerr != 0 )
		    goto L_4000;
	    }

	    /* -- Allocate block for header. */
	    allamb( &cmmem, MHDR, &Ndxhdr[jdfl], nerr );
	    if( *nerr != 0 )
		goto L_4000;

	    for( idx = 0 ; idx < FOURBYTEHDRS ; idx++ ) {
                cmmem.sacmem[Ndxhdr[jdfl]][idx] = 0 ;
            }

	    /* -- Copy header data-set ptr to working storage. */
	    Nlndta[jdfl] = 0 ;	/* no data yet */

	    /* -- Set the file number (position) */
	    Ndsndx[jdfl] = 1 ;

	    /* -- Read header. */
	    if( lsdd ){
		rdshdr( jdfl, &nun, nerr );
	    }
	    else if( ftype == xdr ){
                xdrhdr( xdrs, cmmem.sacmem[Ndxhdr[jdfl]], nerr);
	    }
	    else{
		lswap[ jdfl ] = rdhdr( jdfl, &nun, nerr );
	    }

	    if( *nevid == -12345 || *norid == -12345 )
		cmdfm.nreadflag = LOW ;

	    if( *nerr != 0 )
		goto L_4000;

	    /* -- Close file. */
            if( ftype == xdr ){
                xdr_destroy( &xdrs );
                zcloses( &fileun, nerr);
	    }
	    else{
		zclose( &nun, nerr );
	    }
	    if( *nerr != 0 )
		goto L_4000;
	}

	/* - Skip to bottom if only headers are to be read. */

	if( !ldata )
	    goto L_4000;

	/* - Define memory requirements for each file. */

	jstart = 1;

L_2200:
	iflag = 2;
	for( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ){
	    jdfl = ndflsv + jdflrq;
	    getfil( jdfl, FALSE, &ntused, &ntused, &ntused, nerr );
	    if( *nerr != 0 )
		goto L_4000;
	    defmem( jdfl, TRUE, nerr );
	    if( *nerr != 0 )
		goto L_4000;
	    putfil( jdfl, nerr );

	    if( *nerr != 0 )
		goto L_4000;
	}

	/* - Allocate memory for each data file.
	 *   If there is not enough room, release allocated memory
	 *   and switch to buffered mode. */

	jstart = 1;

L_2400:
	iflag = 3;
	for( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ){
	    jdfl = ndflsv + jdflrq;
	    for( jcomp = 0; jcomp < Ncomp[jdfl]; jcomp++ ){
		allamb ( &cmmem , Nlndta[ jdfl ] ,
			 &cmdfm.ndxdta[ jdfl - 1 ][ jcomp ] , nerr ) ;

		if( *nerr != 0 ){
		    outmsg () ;

		    for( j0 = ndflsv + jstart; j0 <= (ndflsv + ndflrq); j0++ )
			relamb( cmmem.sacmem, Ndxhdr[j0], nerr );

		    if( *nerr != 0 )
			goto L_8888;
		    for( j1 = ndflsv + jstart; j1 < jdfl ; j1++ ){
			for( j2 = 0; j2 < Ncomp[j1]; j2++ ){
			    relamb(cmmem.sacmem, cmdfm.ndxdta[j1-1][j2], nerr);
			}
		    }
		    if( *nerr != 0 )
			goto L_8888;
		    for( j2 = 0; j2 < jcomp ; j2++ ){
			relamb(cmmem.sacmem, cmdfm.ndxdta[jdfl - 1][j2], nerr);
		    }
		    if( *nerr != 0 )
			goto L_8888;

		    cmdfm.ndfl = ndflsv;

		    goto L_8888 ;
		}
	    } /* end for( jcomp = 1; jcomp <= Ncomp[jdfl]; jcomp++ ) */
	} /* end for ( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ) */

	/* - Now we are finally ready to actually read in the data. */

	jstart = 1;
L_3000:
	iflag = 4;
	for( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ){
	    jdfl = ndflsv + jdflrq;
	    getfil( jdfl, FALSE, &ntused, &ntused, &ntused, nerr );
	    if( *nerr != 0 )
		goto L_4000;
	    /* -- Open file. */
	    lnumcl( kmdfm.kdflrq,MAXCHARS , jdflrq, &ic1, &ic2 );
	    fstrncpy( kfile, MCPFN, kmdfm.kdflrq+ic1 - 1, ic2 - ic1 + 1);
	    if ( ftype != xdr ){
                zopen_sac( &nun, kfile,MCPFN+1, "RODATA",7, nerr );
		if( *nerr != 0 )
		    goto L_4000;
	    }
	    /* -- Read data. */
	    if( lsdd ){
		rdsdta( jdfl, &nun, nerr );
	    }
	    else if( ftype == xdr ){
		rdxdrdta( jdfl, kfile, MCPFN+1, nerr );
	    }
	    else{
		rddta( jdfl, &nun, lswap[ jdfl ], nerr );
	    }
	    if( *nerr != 0 )
		goto L_4000;
	    /* -- Close file. */
	    if( ftype != xdr )zclose( &nun, nerr );
	}

	/* - If an error occurred anywhere in read, either quit
	 *   or delete this file from DFL.  See READERR command. */

L_4000:
	if( *nerr != 0 ){
	    zclose( &nun, &ncerr );
	    if( strcmp(kmdfm.kecbdf,"FATAL   ") == 0 ){
		goto L_8888;
	    }
	    else{
		lnumcl( kmdfm.kdflrq,MAXCHARS , jdflrq, &ic1, &ic2 );
		fstrncpy( kfile, MCPFN, kmdfm.kdflrq+ic1 - 1, ic2 - ic1 + 1);

		if( strcmp(kmdfm.kecbdf,"WARNING ") == 0 ){
		    typmsg( "WARNING" );
		    outmsg();
		}
		*nerr = 0;
		lrdrem = TRUE;
		ldelcl( kmdfm.kdfl,MAXCHARS, kfile,MCPFN+1 );
		/* -- Delete name from file lists and reset do loop variables */
		ldelcl( kmdfm.kdflrq,MAXCHARS , kfile,MCPFN+1 );
		cmdfm.ndfl = cmdfm.ndfl - 1;
		ndflrq = ndflrq - 1;
		cmdfm.ndsflcnt = cmdfm.ndsflcnt - 1;
		jstart = jdflrq;
		/* -- Depending upon where we are in the reading process, we
		 *    must also move some data length and index parameters. */
		for( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ){
		    jdfl = ndflsv + jdflrq;

		    /* --- Get the index position in data-set storage for
			   the next file's info. */
		    if( iflag >= 2 ){
			Ndxhdr[jdfl] = Ndxhdr[jdfl + 1];
		    }

		    if( iflag >= 3 ){
			Nstart[jdfl] = Nstart[jdfl + 1];
			Nstop[jdfl] = Nstop[jdfl + 1];
			Nfillb[jdfl] = Nfillb[jdfl + 1];
			Nfille[jdfl] = Nfille[jdfl + 1];
			Ncomp[jdfl] = Ncomp[jdfl + 1];
			Nlndta[jdfl] = Nlndta[jdfl + 1];
		    }

		    if( iflag >= 4 ){
			for( jcomp = 0; jcomp < Ncomp[jdfl]; jcomp++ ){
			    cmdfm.ndxdta[ jdfl - 1 ][ jcomp ] =
				cmdfm.ndxdta[ jdfl ][ jcomp ] ;
			}
		    } /* end if( iflag >= 4 ) */
		} /* end for( jdflrq = jstart; jdflrq <= ndflrq; jdflrq++ ) */
		/* -- Jump to appropriate loop in the reading process. */
		switch( iflag ){
		    case 0: goto L_1800;
		    case 1: goto L_2000;
		    case 2: goto L_2200;
		    case 3: goto L_2400;
		    case 4: goto L_3000;
		} /* end switch */
	    } /* end else associated with if ( strcmp ( kmdfm.kecbdf ... ) */
	} /* end if ( *nerr != 0 ) */

	/* - Check again for a non-null DFL. */

	if( cmdfm.ndfl <= 0 ){
	    *nerr = 1301;
	    setmsg( "ERROR", *nerr );
	}

	/* - Write a warning message if some of the files could not be read. */
	else if( lrdrem && strcmp(kmdfm.kecbdf,"WARNING ") == 0 ){
	    setmsg( "WARNING", 1333 );
	    apcmsg( " reading the rest of the files.", 32 ) ;
	    outmsg();
	    clrmsg();
	}

	/* - Calculate range of dependent variable for use during plotting. */

	setrng();

L_8888:

	return;

} /* end of function */

