#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/sss.h"

#include "cssStrucs.h"
#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "smDataIO.h"

int fndelcl ( char * , int , int ) ;


void /*FUNCTION*/ xdeletestack(nerr)
int *nerr;
{
	char kfile[MCPFN+1];
	int lincr, lcommit ;
	int ic1, ic2, idel[MDFL], jdel, jdfl, jdfl2, jdfl2_, 
	 jdfl3, ncfile, ndel ; 

	int *const Idel = &idel[0] - 1;

	DBlist tree ;

	tree = smGetDefaultTree () ;



	/*=====================================================================
	 * PURPOSE:  To execute the DELETESTACK command.  This command deletes
	 *           one or more files from the signal stack.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers:  1001, 5106, 5107
	 *=====================================================================
	 * MODULE/LEVEL:  sss/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:    mcpfn
	 *    dfm:     mdfl
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    dfm:     ndfl, kdfl, ndxhdr, nlndta, ndxdta, ncomp
	 *    sss:     dlyn, dlyt, dlyni, dlyti, ldlyt, ldlyi, wt, dst, lpol
	 *             beginTime, endTime
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    saclib:  lcmore, cfmt, cresp, lcdfl, lcint,
	 *             setmsg, apcmsg, apimsg, sorti, relamb
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    idel:    Array of signal stack file numbers to delete. [i]
	 *    ndel:    Length of idel. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970203:  Added several lines and swapped some array elements
	 *             to fix the bug that it just didn't work.  maf
	 *    960701:  Added begin and end time, maf
	 *    881130:  Fixed bug in handling of data file list.
	 *    881117   Changes due to restructuring data file access methods.
	 *    850812:  Major revision of subprocess.
	 *    821201:  Changed to newest set of parsing and checking functions.
	 *    810120:  Changed to output message retrieval from disk.
	 *    790601:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850812
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	ndel = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

            /* "COMMIT": removes infromation from SeisMgr as well as SAC */
/*	    if( lklog( "COMMIT$",8, &lcommit ) ){
                cmdfm.lcommit = lcommit ;
            }	*/

	    /* -- "n":  the number of a file from the signal stack. */
	    if( lcint( &jdfl ) ){
		if( jdfl < 1 || jdfl > cmdfm.ndfl ){
		    *nerr = 5107;
		    setmsg( "ERROR", *nerr );
		    apimsg( jdfl );
		    goto L_8888;
		}
		ndel = ndel + 1;
		Idel[ndel] = jdfl;
	    }

	    /* -- "filename":  the name of a file from the signal stack. */
	    else if( lcchar( MCPFN, kfile,MCPFN+1, &ncfile ) ){
		jdfl = nfndcl( kmdfm.kdfl,MAXCHARS, kfile,MCPFN+1, &ic1, &ic2 );
		if( jdfl <= 0 ){
		    *nerr = 5106;
		    setmsg( "ERROR", *nerr );
		    apcmsg( kfile,MCPFN+1 );
		    goto L_8888;
		}
		ndel = ndel + 1;
		Idel[ndel] = jdfl;
	    }

	    /* -- Bad syntax. */
	    else{
		cfmt( "ILLEGAL OPTION:$",17 );
		cresp();
	    }

	} /* end while */

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	/* EXECUTION PHASE: */

	/* - Sort the list of file numbers into decreasing order */

	lincr = FALSE;
	sorti( idel, ndel, lincr, idel );

	/* - For each file to be deleted: */

	jdfl = 0;
	for( jdel = 1; jdel <= ndel; jdel++ ){
	    if( Idel[jdel] != jdfl ){
		jdfl = Idel[jdel];
		/* -- Release memory blocks. */
		if( Ndxhdr[jdfl] > 0 )
		    relamb( cmmem.sacmem, Ndxhdr[jdfl], nerr );
		if( cmdfm.ndxdta[ jdfl - 1 ][ 0 ] > 0 )			/* array order swapped. */
		    relamb( cmmem.sacmem, cmdfm.ndxdta[ jdfl - 1 ][ 0 ], nerr );	/* " */
		if( cmdfm.ndxdta[ jdfl - 1 ][ 1 ] > 0 )					/* " */
		    relamb( cmmem.sacmem, cmdfm.ndxdta[ jdfl - 1 ][ 1 ], nerr );  /* maf 970203 */
		/* -- Remove entry from list of data file names. */
		lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
		fstrncpy( kfile, MCPFN, kmdfm.kdfl+ic1 - 1,min(ic2,MAXCHARS-1) - ic1 + 1);
		fndelcl( kmdfm.kdfl,MAXCHARS, jdfl );
		/* -- Move DFM and SSS array variables down.           */
		for( jdfl2 = jdfl; jdfl2 <= (cmdfm.ndfl - 1); jdfl2++ ){
		    jdfl2_ = jdfl2 - 1;
		    jdfl3 = jdfl2 + 1;
		    Ndxhdr[jdfl2] = Ndxhdr[jdfl3];
		    Nlndta[jdfl2] = Nlndta[jdfl3];
		    cmdfm.ndxdta[jdfl2_][0] = cmdfm.ndxdta[jdfl3 - 1][0]; /* array order swapped. */
		    cmdfm.ndxdta[jdfl2_][1] = cmdfm.ndxdta[jdfl3 - 1][1]; /* maf 970203 */
		    Ncomp[jdfl2] = Ncomp[jdfl3];
		    Dlyt[jdfl2] = Dlyt[jdfl3];
		    Dlyti[jdfl2] = Dlyti[jdfl3];
		    Dlyn[jdfl2] = Dlyn[jdfl3];
		    Dlyni[jdfl2] = Dlyni[jdfl3];
		    Wt[jdfl2] = Wt[jdfl3];
		    Dst[jdfl2] = Dst[jdfl3];
		    Tbegin[jdfl2] = Tbegin[jdfl3] ; /* maf 960701 */
		    Tend[jdfl2] = Tend[jdfl3] ; /* maf 960701 */
		    Lpol[jdfl2] = Lpol[jdfl3];

		    /* Added.  maf  970203 */
		    Nstart  [ jdfl2 ] = Nstart  [ jdfl3 ] ;
		    Nstop   [ jdfl2 ] = Nstop   [ jdfl3 ] ;
		    Nfillb  [ jdfl2 ] = Nfillb  [ jdfl3 ] ;
		    Nfille  [ jdfl2 ] = Nfille  [ jdfl3 ] ;
		    Ntotal  [ jdfl2 ] = Ntotal  [ jdfl3 ] ;
		    Nxsdd   [ jdfl2 ] = Nxsdd   [ jdfl3 ] ;
		    Ndsndx  [ jdfl2 ] = Ndsndx  [ jdfl3 ] ;

		}
		/* -- Decrement file count. */
		cmdfm.ndfl = cmdfm.ndfl - 1;
	    } /* end if( Idel[jdel] != jdfl ) */
	} /* end for( jdel = 1; jdel <= ndel; jdel++ ) */

	/* If COMMIT option set, delete files from SeisMgr as well. */
/*	if ( cmdfm.lcommit ) {	*/
	    /* decrement elements of idel for the sake of the C array numbering */
	    for ( jdel = 0 ; jdel < ndel ; jdel++ )
		idel[ jdel ] -- ;

	    dblDeleteWfdiscs ( tree , (int *) idel , ndel ) ;
/*	} * end if ( cmdfm.lcommit ) */

L_8888:
	return;

} /* end of function */

