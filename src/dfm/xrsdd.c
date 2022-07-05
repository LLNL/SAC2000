#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"

extern enum filetype { sacfi, alpha, xdr, segy } ftype ;
int lckeyExact(char* kkey,int kkey_s);
void alignFiles ( int *nerr );

void /*FUNCTION*/ xrsdd(nerr)
int *nerr;
{
	char _c0[2], kdflin[MCMSG+1];
	int ldata, lmore, lsdd;
	int nchar, ndflin , icomORroll ;

	ftype = sacfi ;


	/*=====================================================================
	 * PURPOSE:  To execute the action command READSDD.
	 *           This command reads an SDD file from disk into SAC's memory.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 1301, 1320
	 *=====================================================================
	 * MODULE/LEVEL: DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     KDIRDF
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LCDFL, READFL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    LMORE:   Flag used when adding files to current DFL. [l]
	 *    LDATA:   Flag used to tell READFL that  headers and data are
	 *             to be read into memory. [l]
	 *    LSDD:    Flag used to tell READFL that file is in SDD format. [l]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    880308:  Added LDATA flag to call to READFL.
	 *    870625:  Factored execution portion to readfl.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870625
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Parse position-dependent tokens: */

	lmore = FALSE;

	while ( lcmore( nerr ) ){

	    /* -- "MORE":  signifies addition of more files to current read filelist
	     *             rather than replacement of current list with new one. */
	    if( lckey( "MORE#$",7 ) && cmdfm.ndfl > 0 ){
			lmore = TRUE;
	    }

	    /* -- "DIR CURRENT|name":  set the name of the default subdirectory. */
	    else if( lkchar( "DIR#$",6, MCPFN, kmdfm.krddir,MCPFN+1, &nchar ) ){
		if( memcmp(kmdfm.krddir,"CURRENT",7) == 0 || memcmp(kmdfm.krddir
		 ,"current",7) == 0 ){
		    fstrncpy( kmdfm.krddir, MCPFN, " ", 1);
		}
		else if( kmdfm.krddir[nchar - 1] != KDIRDL ){
		    _c0[0] = KDIRDL;
		    _c0[1] = '\0';
		    subscpy( kmdfm.krddir, nchar, -1, MCPFN, _c0 );
		}
	    }

	    /* -- "COMMIT|RECALLTRACE|ROLLBACK":
		  how to treat existing data */
	    else if ( lckeyExact ( "COMMIT" , 7 ) )
		cmdfm.icomORroll = COMMIT ;
	    else if (lckeyExact ( "RECALLTRACE" , 12 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "RECALL" , 7 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "ROLLBACK" , 9 ) )
		cmdfm.icomORroll = ROLLBACK ;


	    else	/* else move on to next set of tokens. */
		break ;
	}

	/* - Parse position-independent tokens: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "filelist":  define a new input filelist. */
	    if( lcdfl( kdflin,MCMSG+1, &ndflin ) )
	    { /* do nothing */ }

	    /* -- Bad syntax. */
	    else{
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

        /* - Commit or rollback data according to lmore and cmdfm.icomORroll */
        if ( lmore ) {
            alignFiles ( nerr ) ;
	    if ( *nerr )
		return ;
	    cmdfm.nfilesFirst = cmdfm.ndfl ;
        } /* end if */
	else
	    cmdfm.nfilesFirst = 0 ;


	/* - Expand the filelist and read the files into memory. */

	ldata = TRUE;
	lsdd = TRUE;
	readfl( ldata, lmore, lsdd, kmdfm.krddir,MCPFN+1,
		kdflin,MCMSG+1, ndflin, nerr );

	if ( *nerr == 0 ){	/* if no error */
	    cmdfm.nreadflag = LOW ;
	    cmdfm.lread = TRUE ;
	    sacToSeisMgr ( !lmore , FALSE , TRUE , nerr ) ;
	    cmdfm.lread = FALSE ;
	}

L_8888:
	return;

} /* end of function */

