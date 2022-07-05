#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "sddhdr.h"

void apcmsg2(char* kalpha, int kalpha_s);
int lckeyExact(char* kkey,int kkey_s);
void alignFiles ( int *nerr );
int lkcharExact(char* kkey, int kkey_s, int mchar, char* kchar, int kchar_s, int* nchar);
void wrsegy( int idfl , char *filename, int *nerr );



void /*FUNCTION*/ xw(lsdd, nerr)
int lsdd;
int *nerr;
{
	char delimiter[2], kcdir[9], kchange[MCPFN+1], kdirpart[MCPFN+1], kfile[MCPFN+1], 
	 kpdir[9], kstring[MCPFN+1], ktemp[9];
	int lexpnd;
	int ic1, ic2, icomORroll, jdfl, nchange, nchar, nchg, ndx1, ndx2, 
	 nlen, nstr, nstring, nwrdir;
	static int lwrdir = FALSE;
        char *cattemp;
        char *strtemp1, *strtemp2, *strtemp3;

	kschan[12]='\0';
	kschdr[80]='\0';
	ksclas[4]='\0';
	kscom[40]='\0';
	ksevnm[8]='\0';
	ksfrmt[8]='\0';
	ksstnm[8]='\0';

        lexpnd = FALSE;

	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITE.
	 *           This command writes data files to disk.
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
	 *    970702:  Changed lckey and lkchar to lckeyExact and lkcharExact
	 *             throughout xw.c.  This will allow files to begin with 
	 *             the same string as the various options (eg. sacxz.021.z)
	 *             maf.
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

	if( lsdd )
	    cmdfm.iwfmt = 3;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

	    /* -- "SAC|ALPHA":  set format to be used in writing files. */
	    if( lckeyExact( "SAC#$",6 ) )
		cmdfm.iwfmt = 1;

	    else if( lckeyExact( "ALPHA#$",8 ) )
		cmdfm.iwfmt = 2;

	    else if( lckeyExact( "CI#$",5 ) )
		cmdfm.iwfmt = 2;

	    else if( lckeyExact( "SDD#$",6 ) )
		cmdfm.iwfmt = 3;

	    else if( lckeyExact( "XDR#$",6 ) )
		cmdfm.iwfmt = 4;

            else if( lckeyExact( "SEGY#$", 7 ) )
                cmdfm.iwfmt = 5;

	    /* -- "OVER":  overwrite files from previous READ command. */
	    else if( lckeyExact( "OVER#$",7 ) ){
		cmdfm.lovrrq = TRUE;
		lexpnd = FALSE;
		cmdfm.nwfl = cmdfm.ndfl;
		strcpy( kmdfm.kwfl, kmdfm.kdfl );
	    }

	    /* generate names from the KSTCMP header field */
	    else if( lckeyExact( "KSTCMP#$",9 ) ){
		lexpnd = FALSE;
		cmdfm.nwfl = cmdfm.ndfl;
		gennames("KSTCMP ",7,kmdfm.kwfl,MAXCHARS,cmdfm.nwfl,nerr);
		if(*nerr != 0)
		    goto L_8888;
	    }

	    /* -- "APPEND string": append string to filenames from READ command. */
	    else if( lkcharExact( "APPEND#$",9, MCPFN, kstring,MCPFN+1, &nstring ) ){
		memset ( kmdfm.kwfl , ' ' , MAXCHARS - 1 );
		cmdfm.nwfl = 0;
		ic1 = 0;
		ic2 = 0;
		while ( lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 ) ){
		    strtemp1 = malloc(ic2-ic1+2);
		    strncpy(strtemp1,kmdfm.kdfl+ic1-1,ic2-ic1+1);
		    strtemp1[ic2-ic1+1] = '\0';

		    appendstring( kstring,MCPFN+1, strtemp1, ic2-ic1+2, kfile,MCPFN+1 );

		    free(strtemp1);

		    putcl( kmdfm.kwfl,MAXCHARS, kfile,MCPFN+1, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    cmdfm.nwfl = cmdfm.nwfl + 1;
		}
		cmdfm.lovrrq = FALSE;
		lexpnd = TRUE;
	    }

	    /* -- "PREPEND string": prepend string to filenames from READ command. */
	    else if( lkcharExact( "PREPEND#$",10, MCPFN, kstring,MCPFN+1, &nstring ) ){
		memset ( kmdfm.kwfl , ' ' , MAXCHARS - 1 );
		cmdfm.nwfl = 0;
		ic1 = 0;
		ic2 = 0;
		while ( lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 ) ){
		    strtemp1 = malloc(nstring+1);
		    strtemp2 = malloc(ic2-ic1+2);
		    strncpy(strtemp1,kstring,nstring);
		    strtemp1[nstring] = '\0';
		    strncpy(strtemp2,kmdfm.kdfl+ic1-1, ic2-ic1+1);
		    strtemp2[ic2-ic1+1] = '\0';

		    prependstring( strtemp1, nstring+1, strtemp2, ic2-ic1+2, kfile,MCPFN+1 );

		    free(strtemp1);
		    free(strtemp2);

		    putcl( kmdfm.kwfl,MAXCHARS, kfile,MCPFN+1, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    cmdfm.nwfl = cmdfm.nwfl + 1;
		}
		cmdfm.lovrrq = FALSE;
		lexpnd = TRUE;
	    }

	    /* -- "DELETE string": delete string from filenames from READ command. */
	    else if( lkcharExact( "DELETE#$",9, MCPFN, kstring,MCPFN+1, &nstring ) ){
		memset ( kmdfm.kwfl , ' ' , MAXCHARS - 1 );
		cmdfm.nwfl = 0;
		ic1 = 0;
		ic2 = 0;
		while ( lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 ) ){
		    strtemp1 = malloc(nstring+1);
		    strtemp2 = malloc(ic2-ic1+2);
		    strncpy(strtemp1,kstring,nstring);
		    strncpy(strtemp2,kmdfm.kdfl+ic1-1,ic2-ic1+1);
		    strtemp1[nstring] = '\0';
		    strtemp2[ic2-ic1+1] = '\0';

		    deletestring( strtemp1, nstring+1, strtemp2, ic2-ic1+2, kfile,MCPFN+1 );

		    free(strtemp1);
		    free(strtemp2);

		    putcl( kmdfm.kwfl,MAXCHARS, kfile,MCPFN+1, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    cmdfm.nwfl = cmdfm.nwfl + 1;
		}
		cmdfm.lovrrq = FALSE;
		lexpnd = TRUE;
	    }

	    /* -- "CHANGE string1 string2": change string1 to string2 in READ filenames. */
	    else if( lkcharExact( "CHANGE#$",9, MCPFN, kstring,MCPFN+1, &nstring ) ){
		lcchar( MCPFN, kchange,MCPFN+1, &nchange );
		memset ( kmdfm.kwfl ,' ' , MAXCHARS - 1 );
		cmdfm.nwfl = 0;
		ic1 = 0;
		ic2 = 0;
		while ( lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 ) ){
		    nstr = indexb( kstring,MCPFN+1 );
		    nchg = indexb( kchange,MCPFN+1 );

		    strtemp1 = malloc(nstr+1);
		    strtemp2 = malloc(nchg+1);
		    strtemp3 = malloc(ic2-ic1+2);
		    strncpy(strtemp1,kstring,nstr);
		    strncpy(strtemp2,kchange,nchg);
		    strncpy(strtemp3,kmdfm.kdfl+ic1-1, ic2-ic1+1);
		    strtemp1[nstr] = '\0';
		    strtemp2[nchg] = '\0';
		    strtemp3[ic2-ic1+1] = '\0';

		    changestring( strtemp1, nstr+1, strtemp2, nchg+1,
				  strtemp3, ic2-ic1+2, kfile,MCPFN+1 );

		    free(strtemp1);            
		    free(strtemp2);
		    free(strtemp3);

		    putcl( kmdfm.kwfl,MAXCHARS, kfile,MCPFN+1, nerr );
		    if( *nerr != 0 )
			goto L_8888;
		    cmdfm.nwfl = cmdfm.nwfl + 1;
		}
		cmdfm.lovrrq = FALSE;
		lexpnd = TRUE;
	    }

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
	    else if( lcdfl( kmdfm.kwfl,MAXCHARS, &cmdfm.nwfl ) ){
		cmdfm.lovrrq = FALSE;
		lexpnd = FALSE;
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

	/* CHECKING PHASE: */

	/* - Check for null write filelist. */

	if( cmdfm.nwfl <= 0 ){
	    *nerr = 1311;
	    setmsg( "ERROR", *nerr );
	    goto L_8888;
	}

	/* - Make sure the write filelist has as many entries as read filelist. */

	if( cmdfm.nwfl != cmdfm.ndfl ){
	    *nerr = 1312;
	    setmsg( "ERROR", *nerr );
	    apimsg( cmdfm.nwfl );
	    apimsg( cmdfm.ndfl );
	    goto L_8888;
	}

	/* EXECUTION PHASE: */

        /* - Commit or rollback data according to cmdfm.icomORroll */
	alignFiles ( nerr ) ;
	if ( *nerr )
	    return ;


	/* - Echo expanded filelist if requested. */

	if( cmdfm.lechof && lexpnd ){
	    setmsg( "OUTPUT", 0 );
	    ic1 = 0;

	    while ( lnxtcl( kmdfm.kwfl,MAXCHARS, &ic1, &ic2 ) ){

		/* -- Break pathname into directory and filename parts. */
		strtemp1 = malloc(ic2-ic1+2);
		strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
		strtemp1[ic2-ic1+1] = '\0';

		getdir( strtemp1, ic2-ic1+2, kcdir,9, kfile,MCPFN+1 );

		free(strtemp1);

		/* -- Echo the filename part if there is no directory part. */
		if( strcmp(kcdir,"        ") == 0 )
			apcmsg( kfile,MCPFN+1 );

		/* -- Prepend the filename part with some special characters if
                 *    directory part is same as that of the previous file. */
		else if( memcmp(kcdir,kpdir,min(strlen(kcdir),strlen(kpdir)))
                         == 0 ){
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
	    }
	    wrtmsg( MUNOUT );
	}

	/* - Write each file in memory to disk. */

	nwrdir = indexb( kmdfm.kwrdir,MCPFN+1 );
	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
	    /* -- Get file from memory manager. */
	    getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
	    if( *nerr != 0 )
		goto L_8888;

	    /* isolate file name */
	    lnumcl( kmdfm.kwfl,MAXCHARS, jdfl, &ic1, &ic2 );

	    /* -- Check overwrite-protect flag in header record. */
	    if( cmdfm.lovrrq && !*lovrok ){
		*nerr = 1303;
		setmsg( "ERROR", *nerr );
		apcmsg2(&kmdfm.kwfl[ic1 - 1],ic2-ic1+1);
		outmsg () ;
		clrmsg () ;
		goto L_8888;
	    }

	    /* -- Prepare output file name:
	     * --- If directory option is ON (lwrdir=.TRUE. and nwrdir>0), 
	     *     concatenate directory name with file name part of write file list.
	     * --- If directory option is CURRENT (lwrdir=.TRUE. and nwrdir=0), 
	     *     use file name part of write file list.
	     * --- If directory option is OFF, use write file list. */
	    if( lwrdir ){
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
		}
		else{
		    fstrncpy( kfile, MCPFN, " ", 1);

		    strtemp1 = malloc(ic2-ic1+2);     
		    strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
		    strtemp1[ic2-ic1+1] = '\0';

		    getdir( strtemp1, ic2-ic1+2, kdirpart,MCPFN+1, kfile,MCPFN+1 );

		    free(strtemp1);
		}
	    }
	    else
		fstrncpy( kfile, MCPFN, kmdfm.kwfl+ic1 - 1, ic2 - ic1 + 1);

	    /* -- Write file in appropriate format. */
	    if( cmdfm.iwfmt == 2 )
		wrci( jdfl, kfile,MCPFN+1, "%#15.7g", nerr );

	    else if( cmdfm.iwfmt == 3 )
		wrsdd( jdfl, kfile,MCPFN+1, TRUE, nerr );

	    else if( cmdfm.iwfmt == 4 )
		wrxdr( jdfl, kfile,MCPFN+1, TRUE, nerr );

	    else if( cmdfm.iwfmt == 5 )
		wrsegy( jdfl , kfile , nerr ) ;

	    else
		wrsac( jdfl, kfile,MCPFN+1, TRUE, 0, nerr );

	    if( *nerr != 0 )
		goto L_8888;

	} /* end for ( jdfl ) */

L_8888:

	return;

} /* end of function */

