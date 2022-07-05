#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"

enum filetype { sacfi, alpha, xdr, segy } ftype ;
void alignFiles ( int *nerr );
void alignFiles ( int *nerr );
int lckeyExact(char* kkey,int kkey_s);

void /*FUNCTION*/ xr(nerr)
int *nerr;
{
	char dirDelimiter[2], kline[MCMSG+1];
	int	ldata,
		lmore,	/* FALSE if new data replaces old, TRUE if appended */
		lscale,
		lsdd;
	int nchar, icomORroll ;
        static char kdflin[MCMSG+1];
        static int ndflin;

	ftype = sacfi ;


	/*=====================================================================
	 * PURPOSE:  To execute the action command READ.
	 *           This command reads data from disk into SAC's memory.
	 *           File info is kept in data-set storage and is transfered to
	 *           working storage when a data set is made active.
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
	 *             NDFL: Number of files currently in SAC's memory.
	 *
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LCDFL, READFL,
	 *             SETCDSINDEX, MKCDSFLSACTIVE, CLEARWS, CLEARDS
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    LSDD:    Flag used to say file is in SDD format. [l]
	 *    LMORE:   Flag used when adding files to current DFL. [l]
	 *    LDATA:   Flag used to tell READFL that  headers and data are
	 *             to be read into memory. [l]
	 *    NDFLIN:  Number of files to be read into SAC this time. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    981103:  Removed TO options.  There are no datasets.  maf
	 *    920508:  Added a space to the MORE and TO keyword options.
	 *    920428:  Moved memory setup for data-set stuff to lfilesok.
	 *    920403:  Test for max files already in sac's data-sets.
	 *    910826:  Broke cleardfl into two routines, cleards for data-set 
	 *             storage and clearws for working-storage. Cleardfl calls 
	 *             both routines.
	 *    910812:  Added option TO, in support of multiple data-sets.
	 *    880308:  Added LDATA flag to call to READFL.
	 *    870625:  Factored execution portion to readfl.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870625
	 *===================================================================== */
	/* These next two includes are not really needed, but some global 
	 * variables that are initialized in inidfm can't be displayed in the 
	 * debugger without these next two includes.
	 *
	 *     include '../../inc/hdr'
	 *     include '../../inc/mem' */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Parse position-dependent tokens: */

	lmore = FALSE;

	while ( lcmore( nerr ) ){

	    /* -- "MORE":  signifies addition of more files to current read
	     *             filelist rather than replacement of current list
	     *             with new one. */
	    if( lckey( "MORE #$",8 ) && cmdfm.ndfl > 0 ){
		lmore = TRUE;
	    }

	    /* -- "ALPHA":  input file is in alpha format. */
	    else if( lckey( "ALPHA #$", 9 ) )
		ftype = alpha ;

	    /* -- "XDR":  input file is in xdr format. */
	    else if( lckey( "XDR #$",7 )){
		ftype = xdr;
	    }

            /* -- "SEGY":  input file is in segy format. */
            else if( lckey( "SEGY #$",8 )){
                ftype = segy;
            }

	    /* -- "IO, IB": sets reference time, IB is default. */
	    else if( lckey( "IB #$" , 6 ) )
		cmdfm.iztype = IB ;
	    else if( lckey( "IO #$" , 6 ) )
		cmdfm.iztype = IO ;

            /* -- TRUST:  whether or not to trust matching evids while 
                          moving data from SAC buffers to CSS buffers. */
            else if( lklog( "TRUST#$",8, &cmdfm.ltrust ) )
            { /* do nothing */ }


	    /* -- "DIR CURRENT|name":  set name of default subdirectory. */
	    else if(lkchar("DIR#$",6, MCPFN, kmdfm.krddir,MCPFN+1, &nchar)){
		if( memcmp(kmdfm.krddir,"CURRENT",7) == 0 ||
		    memcmp(kmdfm.krddir ,"current",7) == 0 ){
			fstrncpy( kmdfm.krddir, MCPFN, " ", 1);
		}
		else if( kmdfm.krddir[nchar - 1] != KDIRDL ){
                    dirDelimiter[0] = KDIRDL;
                    dirDelimiter[1] = '\0';
		    subscpy( kmdfm.krddir, nchar, -1, MCPFN,
					 dirDelimiter );
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

	    /* -- "SCALE ON|OFF":  turn scaling on or off */
	    else if ( lklog( "SCALE$",7, &lscale ) ) {
		cmdfm.lscale = lscale ;
	/*	if( lscale ) {
		    setmsg ( "WARNING" , 2119 ) ;
		    outmsg () ;
		    clrmsg () ;
		}*/
	    }
			
	    else
		break ;
	} /* end while */

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

	/* CHECKING PHASE: */

	/* ----- How many files will be in SAC memory?  Take no action if this
	 *       read will exceed the max number of files that SAC can store. */

	if( lmore ){
	    /* ----- Current count of files in memory, plus these news ones,
	     *       exceeds MDFL. */
	    if( (cmdfm.ndfl + ndflin) > MDFL ){
		setmsg( "OUTPUT", 0 );
                sprintf(kline,"%s%3d%s","There are already "
		 , cmdfm.ndfl, " files in sac memory." );
		apcmsg( kline,MCMSG+1 );
                sprintf(kline,"%s%3d%s","Attempted to read "
		 , ndflin, " more files." );
		aplmsg( kline,MCMSG+1 );
		aplmsg( "No files read in.",18 );
		wrtmsg( MUNOUT );
		clrmsg();
		*nerr = 6010;
		goto L_8888;
	    }

	    /* Commit or rollback existing data as per user specs. */
	    alignFiles ( nerr ) ;
	    if ( *nerr )
		return ;

	    cmdfm.nfilesFirst = cmdfm.ndfl ;
	}  /* end if( lmore ) */
	else {
	    cmdfm.nreadflag = HIGH ;
            cmdfm.nfilesFirst = 0 ;
	}


	/* EXECUTION PHASE */

	ldata = TRUE;
	lsdd = FALSE;

	/* --- Now go read the data files. */
	readfl( ldata, lmore, lsdd, kmdfm.krddir, MCPFN+1, kdflin,
		MCMSG+1, ndflin, nerr );

	if ( *nerr ) {
	    setmsg( "ERROR" , *nerr ) ;
	    outmsg() ;
	}

	/* put it out to SeisMgr */
	if( cmdfm.ltrust && cmdfm.nreadflag != LOW )
	    cmdfm.nreadflag = HIGH ;
	else
	    cmdfm.nreadflag = LOW ;

	cmdfm.lread = TRUE ;
	sacToSeisMgr ( !lmore , 0 , 1 , nerr ) ;
	cmdfm.lread = FALSE ;

L_8888:
	ftype = sacfi ;
	return;

} /* end of function */

