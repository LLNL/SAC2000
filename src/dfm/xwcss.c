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
#include "../../inc/sddhdr.h"
#include "smDataIO.h"

void apcmsg2(char* kalpha, int kalpha_s);
int lckeyExact(char* kkey,int kkey_s);
void alignFiles ( int *nerr );
int WriteCSSflatFiles(const char *WorkSetName, const char *basename, int Version, const char *tType);
int WriteCSSBfile(const char *WorkSetName, const char *fname, int verbose);


void /*FUNCTION*/ xwcss(nerr)
int *nerr;
{
	char dirDelimeter[2], kcdir[9], kdirpart[MCPFN+1], kfile[MCPFN+1], 
	 kpdir[9], ktemp[9], tType[5];
	int lexpnd;
	int icomORroll , ibinORasc ;
	int ic1, ic2, jdfl, nchar, nwrdir ;
	static int lwrdir = FALSE, Verbose = FALSE, dbSchema = TRUE ;
	static int csstype = FALSE;
        char *cattemp;
        char *strtemp1, *strtemp2;

        char *WorkSetName;
	int dbWriteSchema = 39;

        lexpnd = FALSE;

        tType[0] = '\0';
	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITECSS.
	 *           This command writes data files to disk in CSS ascii flat
	 *           files or CSS binary (CSSB) format.
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
	 *    910731:  Bug fixed in options PREPEND, DELETE, CHANGE.
	 *    900904:  Added SDD as a format for write.
	 *    881228:  Added four new options for generating write file list
	 *             from data file list: APPEND, PREPEND, CHANGE, DELETE.
	 *    880204:  Fixed logic involving use of DIR option in READ and WRITE
	 *             by adding an ON/OFF flag as well as a directory name.
	 *    880115:  Deleted call that forced default directory to lowercase.
	 *    870626:  Added default directory option.
	 *    860917:  Changed to character lists for storing data file names.
	 *    850730:  Deleted SOCKITTOME  format.
	 *    830120:  Added SOCK and CI output formats.
	 *    820721:  Changed to newest set of parsing and checking functions.
	 *    810120:  Changed to output message retrieval from disk.
	 *    810203:  Fixed bug in file overwrite option.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870626
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    if( lckey( "OVER#$",7 ) ){
		cmdfm.lovrrq = TRUE;
		lexpnd = FALSE;
		cmdfm.nwfl = cmdfm.ndfl;
		strcpy( kmdfm.kwfl, kmdfm.kdfl );
	    }

            /* -- "VERBOSE ON|OFF":  turn Verbose mode on or off. */
            else if( lklog( "VER$BOSE",9, &Verbose ) )
            { /* do nothing */ }
	    
	    /* -- "NDCVERS ON|OFF":  turn NDC write mode on or off. */
            else if( lklog( "NDC$VERS",9, &dbSchema ) )
            { /* do nothing */ }


	    /* -- "DIR ON|OFF|CURRENT|name":  set the name of the default subdirectory. */
	    else if( lkchar( "DIR#$",6, MCPFN, kmdfm.kwrdir,MCPFN+1, &nchar ) ){
		modcase( TRUE, kmdfm.kwrdir, MCPW, ktemp );
		if( strcmp(ktemp,"ON      ") == 0 ){
		    lwrdir = TRUE;
		}
		else if( strcmp(ktemp,"OFF     ") == 0 ){
		    lwrdir = FALSE;
		}
		else if( strcmp(ktemp,"CURRENT ") == 0 ){
		    lwrdir = TRUE;
		    fstrncpy( kmdfm.kwrdir, MCPFN, " ", 1);
		}
		else if( kmdfm.kwrdir[nchar - 1] != KDIRDL ){
		    lwrdir = TRUE;
		    dirDelimeter[0] = KDIRDL;
		    dirDelimeter[1] = '\0';
		    subscpy( kmdfm.kwrdir, nchar, -1, MCPFN, dirDelimeter );
		}
		else{
		    lwrdir = TRUE;
		}
	    }	    
	    else if( lkchar( "TYPE#$",7, MCPFN, cmdfm.ltype,MCPFN+1, &nchar ) ){
		modcase( TRUE, cmdfm.ltype, MCPW, ktemp );
		    csstype = TRUE;		   
		    cmdfm.ltype[2] = '\0';
                    strcpy(tType,cmdfm.ltype);		    
	    }
	    
	    /* -- "COMMIT|RECALLTRACE|ROLLBACK": how to treat existing data */
	    else if ( lckeyExact ( "COMMIT" , 7 ) )
		cmdfm.icomORroll = COMMIT ;
	    else if (lckeyExact ( "RECALLTRACE" , 12 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "RECALL" , 7 ) )
		cmdfm.icomORroll = RECALL ;
	    else if ( lckeyExact ( "ROLLBACK" , 9 ) )
		cmdfm.icomORroll = ROLLBACK ;


	    /* -- "BINARY|ASCII": read CSSB or flatfiles */
	    else if( lclist( (char*)kmdfm.kbinORasc, 9, 2, &ibinORasc ) ) 
		cmdfm.lwascii = ibinORasc - 1 ;

	    /* -- "filelist":  write files using names in new filelist. */
	    else if( lcdfl( kmdfm.kwfl,MAXCHARS, &cmdfm.nwfl ) ){
		cmdfm.lovrrq = FALSE;
		lexpnd = FALSE;
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

	if( *nerr != 0 )
		return;
		
	if( dbSchema ){
		dbWriteSchema = 39;
		printf("Writing out NDC schema-based files.\n");
	} else {
		dbWriteSchema = 30;
		printf("Writing out CSS schema-based files.\n");
	} 

	/* CHECKING PHASE: */

	/* - Check for null write filelist. */

	if( cmdfm.nwfl <= 0 ){
	    *nerr = 1311;
	    setmsg( "ERROR", *nerr );
	    return;
	}

	/* EXECUTION PHASE: */

	/* - Echo expanded filelist if requested. */

	if( cmdfm.lechof && lexpnd ){
	    setmsg( "OUTPUT", 0 );
	    ic1 = 0;

	    /* -- Loop until all pathnames in expanded filelist have been processed. */
	    while ( lnxtcl( kmdfm.kwfl,MAXCHARS, &ic1, &ic2 ) ){

		/* -- Break pathname into directory and filename parts. */
		strtemp1 = malloc(ic2-ic1+2);
		strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
		strtemp1[ic2-ic1+1] = '\0';
		if ( strtemp1[ic2-ic1] == KDIRDL )
		    strtemp1[ic2-ic1] = '\0' ;

		getdir( strtemp1, ic2-ic1+2, kcdir,9, kfile,MCPFN+1 );

		free(strtemp1);

		/* -- Echo the filename part if there is no directory part. */
		if( strcmp(kcdir,"        ") == 0 ){
		    apcmsg( kfile,MCPFN+1 );
		}

		/* -- Prepend the filename part with some special characters if 
		 *    directory part is same as that of the previous file. */
		else if( memcmp(kcdir,kpdir,min(strlen(kcdir),strlen(kpdir))) == 0 ){
		    cattemp = malloc(3+strlen(kfile)+1);
		    strcpy(cattemp, "...");
		    strcat(cattemp,kfile);
		    apcmsg( cattemp, 3+strlen(kfile)+1 );
		    free(cattemp);
		}

		/* -- Echo complete pathname if directory part is different. */
		else{
		    apcmsg2(&kmdfm.kwfl[ic1 - 1],ic2-ic1+1);
		    strcpy( kpdir, kcdir );
		}
	    } /* end while */
	    wrtmsg( MUNOUT );
	} /* end if( cmdfm.lechof && lexpnd ) */


	/* -- Prepare output file name:
	 * --- If directory option is ON (lwrdir=.TRUE. and nwrdir>0), 
	 *     concatenate directory name with file name part of write file list.
	 * --- If directory option is CURRENT (lwrdir=.TRUE. and nwrdir=0), 
      	 *     use file name part of write file list.
       	 * --- If directory option is OFF, use write file list. */
        jdfl = 1;

       	lnumcl( kmdfm.kwfl,MAXCHARS, jdfl, &ic1, &ic2 );
        if( lwrdir ){
	    nwrdir = indexb( kmdfm.kwrdir,MCPFN+1);
	    if( nwrdir > 0 ){
		fstrncpy( kfile, MCPFN, kmdfm.kwrdir,min(nwrdir,MCPFN));

		strtemp1 = malloc(ic2-ic1+2);
		strtemp2 = malloc(130-(nwrdir+1));
		strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
		strncpy(strtemp2,kfile+nwrdir,MCPFN+1-(nwrdir + 1));
		strtemp1[ic2-ic1+1] = '\0';
		strtemp2[MCPFN+1-(nwrdir+1)] = '\0';

		getdir( strtemp1, ic2-ic1+2, kdirpart, MCPFN+1, strtemp2,-(nwrdir+1)+130);
		subscpy(kfile,nwrdir,-1,MCPFN,strtemp2);

		free(strtemp1);            
		free(strtemp2);
	    } else{
		fstrncpy( kfile, MCPFN, " ", 1);

		strtemp1 = malloc(ic2-ic1+2);     
		strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
		strtemp1[ic2-ic1+1] = '\0';

		getdir( strtemp1, ic2-ic1+2, kdirpart,MCPFN+1, kfile,MCPFN+1 );

		free(strtemp1);
	    }
	} /* end if ( lwrdir ) */
	else{
	    fstrncpy( kfile, MCPFN, kmdfm.kwfl+ic1 - 1, ic2 - ic1 + 1);
	}

	terminate ( kfile ) ;


	if(! smGetDefaultWorkset() ){
           *nerr = 1385;
           return;
        }

        WorkSetName = smGetDefaultWorksetName();
        if(! WorkSetName){
           *nerr = 1386;
           return;
        }

        /* commit, recall, or rollback data according to user options. */
        alignFiles ( nerr ) ;
        if ( *nerr )
            return ;


	if ( cmdfm.lwascii )
	    WriteCSSflatFiles(WorkSetName, kfile, dbWriteSchema ,tType) ;
	else {
	    if(!WriteCSSBfile(WorkSetName, kfile, Verbose)) *nerr = 115;
	    if(Verbose)printf("\n");
	}

} /* end of function */

