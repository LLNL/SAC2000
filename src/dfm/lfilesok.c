#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);

int /*FUNCTION*/ lfilesok(kfiles, kfiles_s, nfiles, kdir, kdir_s, 
	 lmore, lheader, lxdr, nerr)
char *kfiles;   int kfiles_s;
int nfiles;
char *kdir;   int kdir_s;
int lmore, lheader, lxdr;
int *nerr;
{
	char kfile[MCPFN+1], kopen[MCPFN+1];
	int lfilesok_v ;
	int ic1, ic2, ihdrndx, idx, jdx, nsdxhdr, nun;
        FILE *fileun;
        XDR xdrs;


	/*=====================================================================
	 * PURPOSE:  Test existence of data files to be read, prepare storage.
	 *=====================================================================
	 * INTPUT ARGUMENTS:
	 *    kfiles:  List of files to be read. [c]
	 *    nfiles:  Number of files to be read. [i]
	 *    kdir:    Default directory to read from. [c]
	 *    lmore:   Set to .TRUE. if these files are to be appended to the
	 *             ones in memory, .FALSE. if they are to replace them. [l]
	 *    lheader: True if reading header, false if just opening file. [l]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    lfilesok:True if a file in the list could be opened and the header
	 *             read. False if no files could be opened for reading.
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    kopen:  full path name of file to be opened.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    981103:  Removed call to mkdsflsactive() by removing cmdfm.ldschg
	 *             which is never TRUE since removing the TO option from
	 *             the READ command.  maf
	 *    920618:  Changed kfile to be substr of kfiles rather than kdflrq.
	 *             Problem came up with READ DIR bug reported by Owens.
	 *    920427:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */
	/* PROCEDURE: */
	ic1 = 0;
	*nerr = 0;

	for( idx = 0 ; idx < MCPFN ; idx++ ){
	    kfile[ idx ] = ' ' ;
	    kopen[ idx ] = ' ' ;
	}
	kfile[MCPFN] = '\0' ;
	kopen[MCPFN] = '\0' ;

	/* - Assume the worst. */
	lfilesok_v = FALSE;

	/* -- Save contents of ndxhdr(1) */
	nsdxhdr = Ndxhdr[1];

	/* -- Allocate block for header. Set ndxhdr for this file. */
	allamb( &cmmem, MHDR, &ihdrndx, nerr );
	if( *nerr != 0 )
	    goto L_8888;
	Ndxhdr[1] = ihdrndx;

	for( idx = 0 ; idx < FOURBYTEHDRS ; idx++ ) {
	    cmmem.sacmem[ihdrndx][idx] = 0 ;
	}

	/* - For each file in the list. */
	for( jdx = 1; jdx <= nfiles; jdx++ ){
	    /* -- Try to open and [optionally] read the header of each file in 
	     *    the list. If successfull then it's ok to continue read process. */

	    /* -- Get a file name. */
	    lnumcl( kfiles,kfiles_s, jdx, &ic1, &ic2 );
	    fstrncpy( kfile, MCPFN, kfiles+ic1 - 1, ic2 - ic1 + 1);

	    /* -- Try to open the file. */
	    fstrncpy( kopen, MCPFN, " ", 1);
	    appendstring( kdir,kdir_s, kopen,MCPFN+1, kopen,MCPFN+1 );
	    appendstring( kfile,MCPFN+1, kopen,MCPFN+1, kopen,MCPFN+1 );
            if( lxdr ) {
		znfiles(&fileun, kopen, MCPFN+1, "TEXT", 5, nerr);
		if( *nerr == 0 ){
                    xdrstdio_create(&xdrs, fileun, XDR_DECODE);
		}
		else{
                    continue ;
		}
	    }
	    else{
		zopen_sac( &nun, kopen,MCPFN+1, "RODATA",7, nerr );
		if( *nerr != 0 ){
/*		    zclose( &nun, nerr );	*/
		    continue ;
		}
	    }
	    /* -- Now optionally read the header, or get out. */
	    if( lheader ){
		/* --- Try to read the header. */
		if( lxdr ){
		    xdrhdr( xdrs, cmmem.sacmem[ihdrndx], nerr );
		}
		else{
		    rdhdr( 1, &nun, nerr );
		}
	    }
	    else{
		lfilesok_v = TRUE;
		if( lxdr ) {
                    xdr_destroy( &xdrs );
                    zcloses( &fileun, nerr);
                }
		else{
                    zclose( &nun, nerr );
		}
		break;
	    }

	    /* -- Test for successfull header read with this file. */
	    if( *nerr != 0 ){
		if( lxdr ) {
		    xdr_destroy( &xdrs );
		    zcloses( &fileun, nerr);
		}
		else{
		    zclose( &nun, nerr );
		}
		continue ;
	    }
	    else{
		lfilesok_v = TRUE;
                if( lxdr ) {
		    xdr_destroy( &xdrs );
		    zcloses( &fileun, nerr);
		}
		else{
		    zclose( &nun, nerr );
		}
		break;
	    }
	} /* end for */


	/* - A file was found to be a valid sac file, or we ran out of files */
	if( lfilesok_v ){
	    /* -- When not using READ MORE: */
	    if( !lmore ){
		/* --- Clear working-storage pointers, data-set storage pointers
		 *     and deallocate memory for the current data set. */
		cleardfl( nerr );
	    }
	} /* end if ( lfilesok_v ) */

L_8888:
	relamb( cmmem.sacmem, ihdrndx, nerr );

	/* - Restore ndxhdr */
	Ndxhdr[1] = nsdxhdr;
	return( lfilesok_v );
} /* end of function */










