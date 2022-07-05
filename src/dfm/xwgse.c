#include <stdio.h>
#include <stdlib.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "msg.h"


int lkcharExact(char* kkey, int kkey_s, int mchar, char* kchar, int kchar_s, int* nchar);
int lckeyExact(char* kkey,int kkey_s);
void alignFiles ( int *nerr );
int WriteGSEFile( char *outFile, DBlist tree, char *datasource, int cm6 );
void * smGetDefaultTree ( void ) ;


void /*FUNCTION*/ xwgse ( nerr )
int *nerr;
{
	char delimiter[2], ktemp[9], outFile[129];
	char kdstemp[ 21 ] ;
	static char kdatasource[ 21 ] = "" ;
	static int ldatasource = FALSE ;
	int ntraces ;
	int icomORroll, nchar;
	static int lwrdir = FALSE;

	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITEGSE.
	 *           This command writes GSE formatted data files to disk.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:  1311, 1312, 1303
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPFN, KDIRDL
	 *    DFM:     LOVRRQ, MDFL, NDFL, KDFL, NWFL, KWRDIR,
	 *             KWFL, NRWFMT, KRWFMT, IWFMT
	 *    HDR:     LOVROK
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    DFM:     LOVRRQ, NWFL, KWFL, IWFMT
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCKEY, LCDFL, INDEXB,
	 *             SETMSG, APIMSG, APCMSG, GETFIL, LNUMCL, WRSAC, WRCI
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NLEN:    Number of data points in each component. [i]
	 *    NDX1:    Index in SACMEM array of first data component. [i]
	 *    NDX2:    Index in SACMEM array of second data component. [i]
	 *    KFILE:   Name of file being written. [c]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    990420:  Original Version, plagerized from xw.c.   maf
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;


	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "SOURCE"  specify the data source for the MSG_ID line */
	    if ( lkcharExact ( "SOURCE#$" , 9 , 20 , kdstemp , 21 , &nchar ) ) {
		modcase( TRUE , kdstemp , MCPW , ktemp ) ;
		if( strcmp(ktemp,"ON      ") == 0 )
		    ldatasource = TRUE ;
		else if( strcmp(ktemp,"OFF     ") == 0 )
		    ldatasource = FALSE ;
		else {
		    ldatasource = TRUE ;
		    strcpy ( kdatasource , kdstemp ) ;
		    terminate ( kdatasource ) ;
		}
	    }
		

            /* -- CM6:  if this option is on, write waveforms in CM6 compressed
                        format.  If it is off, write INT data (default). */
            else if( lklog( "CM6#$",6, &cmdfm.lcm6 ) )
            { /* do nothing */ }


	    /* -- "DIR ON|OFF|CURRENT|name":  set the name of the default subdirectory. */
	    else if( lkcharExact( "DIR#$",6, MCPFN, kmdfm.kwrdir,MCPFN+1, &nchar ) ){
		modcase( TRUE, kmdfm.kwrdir, MCPW, ktemp );
		if( strcmp(ktemp,"ON      ") == 0 )
		    lwrdir = TRUE;
		else if( strcmp(ktemp,"OFF     ") == 0 )
		    lwrdir = FALSE;
		else if( strcmp(ktemp,"CURRENT ") == 0 ){
		    lwrdir = TRUE;
		    fstrncpy( kmdfm.kwrdir, MCPFN, " ", 1);
		}
		else if( kmdfm.kwrdir[nchar - 1] != KDIRDL ){
		    lwrdir = TRUE;
		    delimiter[0] = KDIRDL;
		    delimiter[1] = '\0';
		    subscpy( kmdfm.kwrdir, nchar, -1, MCPFN, delimiter );
		}
		else
		    lwrdir = TRUE;
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


	    /* -- "filelist":  write files using names in new filelist. */
	    else if( lcdfl( kmdfm.kwfl,MAXCHARS, &cmdfm.nwfl ) )
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

	/* - Check for null write filelist. */

	if( cmdfm.nwfl <= 0 ){
	    *nerr = 1311;
	    setmsg( "ERROR", *nerr );
	    goto L_8888;
	}

	/* - Make sure the write filelist has one entry */

	if( cmdfm.nwfl != 1 ){
	    *nerr = 1312;
	    setmsg( "ERROR", *nerr );
	    apimsg( cmdfm.nwfl );
	    apimsg( 1 );
	    goto L_8888;
	}

	/* EXECUTION PHASE: */

        /* - Commit or rollback data according to lmore and cmdfm.icomORroll */
	alignFiles ( nerr ) ;
	if ( *nerr )
	    return ;

	/* make filename */
	if ( lwrdir ) {
	    strncpy ( outFile , kmdfm.kwrdir , 80 ) ;
	    outFile[ 80 ] = '\0' ;
	    terminate ( outFile ) ;
            /* SAC file list leaves first char blank */
	    strncat ( outFile , kmdfm.kwfl+1 , 48 ) ;
	    outFile[ 128 ] = '\0' ;
	    terminate ( outFile ) ;
	}
	else {
            /* SAC file list leaves first char blank */
	    strncpy ( outFile , kmdfm.kwfl+1 , 128 );
	    outFile[ 128 ] = '\0' ;
	    terminate ( outFile ) ;
	}

	ntraces = WriteGSEFile( outFile , smGetDefaultTree() ,
                                ldatasource ? kdatasource : "" , cmdfm.lcm6 ) ;
	if ( ntraces > 0 )
	    printf ( "%d waveforms written in %s\n" , ntraces , outFile ) ;
	else {
	    setmsg ( "ERROR" , 1344 ) ;
	    apcmsg ( outFile , strlen ( outFile ) + 1 ) ;
	    outmsg () ;
	    clrmsg () ;
	}



L_8888:

	return;

} /* end of function */

