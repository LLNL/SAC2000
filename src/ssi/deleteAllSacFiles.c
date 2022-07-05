#include <stdio.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"


/* return the number of files deleted */

int deleteAllSacFiles ( int* nerr , int lname )
{
    /* Declare Variables. */
    char kfile[MCPFN+1];
    int  returnValue ;
    int ic1, ic2, jdfl;

    /*=====================================================================
     * PURPOSE:  To remove all files from SACs data file manager (dfm)
     *=====================================================================
     * INPUT ARGUMENTS:
     *=====================================================================
     * OUTPUT ARGUMENTS:
     *    nerr:    Error flag. Set to 0 if no error occurred.
     *=====================================================================
     * MODIFICATION HISTORY:
     *    971202:  Original version.  maf plagerized from deletechannel
     *===================================================================== */

    *nerr = 0 ;

    /* loop between sac files. */
    for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	/* -- Release memory blocks. */
	if( Ndxhdr[jdfl] > 0 ) {
	    relamb( cmmem.sacmem, Ndxhdr[jdfl], nerr );
	    if ( *nerr ) 
		goto L_8888 ;
	}
	if( cmdfm.ndxdta[ jdfl - 1 ][ 0 ] > 0 ) {
	    relamb( cmmem.sacmem, cmdfm.ndxdta[ jdfl - 1 ][ 0 ], nerr );
	    if ( *nerr ) 
		goto L_8888 ;
	}
	if( cmdfm.ndxdta[ jdfl - 1 ][ 1 ] > 0 ) {
	    relamb( cmmem.sacmem, cmdfm.ndxdta[ jdfl - 1 ][ 1 ], nerr );
	    if ( *nerr ) 
		goto L_8888 ;
	}
	/* -- Remove entry from list of data file names. */
	lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
	fstrncpy( kfile, MCPFN, kmdfm.kdfl+ic1 - 1,min(ic2,MAXCHARS-1) - ic1 + 1);
	if ( lname )
	    fndelcl( kmdfm.kdfl,MAXCHARS, 1 );
	/* -- Reset DFM array variables.           */
	Ndxhdr[jdfl] = 0 ;
	Nlndta[jdfl] = 0 ;
	cmdfm.ndxdta[jdfl][0] = 0 ;
	cmdfm.ndxdta[jdfl][1] = 0 ;
	Ncomp[jdfl] = 0 ;
	Nstart  [ jdfl ] = 0 ;
	Nstop   [ jdfl ] = 0 ;
	Nfillb  [ jdfl ] = 0 ;
	Nfille  [ jdfl ] = 0 ;
	Ntotal  [ jdfl ] = 0 ;
	Nxsdd   [ jdfl ] = 0 ;
	Ndsndx  [ jdfl ] = 0 ;
	
    } /* end for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ) */

    returnValue = cmdfm.ndfl ;

    cmdfm.ndfl = 0 ;

    return returnValue ;

L_8888:
    setmsg ( "ERROR" , *nerr ) ;
    apimsg ( jdfl ) ;
    outmsg () ;
    clrmsg () ;
    return jdfl - 1 ;

} /* end deleteAllSacFiles() */
