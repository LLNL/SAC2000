#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include <string.h>
#include <errno.h>
#define MALPHA  100
#define MAXCH   40
#define MBLKSZ  500
#define MENTRY  40

#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"
#include "extfunc.h"

void alignFiles ( int *nerr );
void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);
int lkcharExact(char* kkey, int kkey_s, int mchar, char* kchar, int kchar_s, int* nchar);
int lckeyExact(char* kkey,int kkey_s);
void apcmsg2(char* kalpha, int kalpha_s);




void /*FUNCTION*/ xwtab( int *nerr )
{
        char slash[2], kchange[ MCPFN+1 ], 
	     kfile[ MCPFN+1 ], kcdir[ MCPFN+1 ], 
	     kstring[ MCPFN+1 ], kdirpart[ MCPFN+1 ],
	     *cattemp, *strtemp1, *strtemp2, *strtemp3, junk[ 7 ] ;
	int nstr, nchg, nchange, nwrdir, nlen, ndx1, ndx2, nderr ;
        int lexpnd, liftype, lheader, lwrdir ;
        int idx, ic1, ic2, jdfl, nchar, ndform,
                 nstring;
        FILE *nun;


        /*=====================================================================
         * PURPOSE:  To execute the action command WRITETABLE (see READTABLE)
         *           This command writes columns of data from SAC's memory.
         *=====================================================================
         * INPUT ARGUMENTS:
         *=====================================================================
         * OUTPUT ARGUMENTS:
         *    NERR:    Error flag. Set to 0 if no error occurred.
         *=====================================================================
         * MODULE/LEVEL: DFM/2
         *=====================================================================
         * GLOBAL INPUT:
         *    MACH:
         *=====================================================================
         * GLOBAL OUTPUT:
         *=====================================================================
         * SUBROUTINES CALLED:
         *    plalpha
         *=====================================================================
         * LOCAL VARIABLES:
         *    NSNDFL:  Save count of files in working storage [i].
         *=====================================================================
         * MODIFICATION HISTORY:
         *    000419:  Original version, plagerized from xrtab.c.
         *=====================================================================
         * DOCUMENTED/REVIEWED:
         *=====================================================================
*/
        /* PROCEDURE: */
	cmdfm.nwfl = 0 ;
        *nerr = 0;
	lwrdir = 0; /* This is set to zero to remove a lint warning about uninitialized. */

        /* PARSING PHASE: */

        /* - Loop on each token in command: */

        while ( lcmore( nerr ) ){

            /* -- "DIR CURRENT|name":  set name of the default subdirectory. */
            if( lkchar( "DIR#$",6, MCPFN, kmdfm.kwrdir,MCPFN+1, &nchar ) ){
                if(memcmp(kmdfm.kwrdir,"CURRENT",7) == 0 || memcmp(kmdfm.kwrdir,
                 "current",7) == 0 ){
                    fstrncpy( kmdfm.kwrdir, MCPFN, " ", 1);
                }
                else if( kmdfm.kwrdir[nchar - 1] != KDIRDL ){
                    slash[0] = KDIRDL;
                    slash[1] = '\0';
                    subscpy( kmdfm.kwrdir, nchar, -1, MCPFN, slash );
                }
            }

            /* -- "IFTYPE": provide the data type, default is off. */
            else if( lklog( "&IFTYPE$",9, &liftype ) ) {
                cmdfm.liftype = liftype ;
            }

            /* -- "HEADER": prints begin and delta on a line at the top. */
            else if( lklog( "&HEADER$",9 , &lheader ) ) {
                cmdfm.lheader = lheader ;
            }


/*
            * -- "FREE":  use free-field rather than formatted input. *
            else if( lckey( "&FREE$",7 ) )
                cmdfm.ldfree = TRUE;

            * -- "FORMAT string": use formatted input & set default format. *
            else if(lkchar("&FORMAT$",9, MCMSG, kmdfm.kdform,MCMSG+1, &ndform)){
                cmdfm.ldfree = FALSE;
                modcase( TRUE, kmdfm.kdform, ndform, kmdfm.kdform );
            }
*/


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

            /* generate names from the KSTCMP header field */
            else if( lckeyExact( "KSTCMP#$",9 ) ){
                lexpnd = FALSE;
                cmdfm.nwfl = cmdfm.ndfl;
                gennames("KSTCMP ",7,kmdfm.kwfl,MAXCHARS,cmdfm.nwfl,nerr);
                if(*nerr != 0)
                    goto L_8888;
            }

            /* -- "APPEND string": append string to filenames in kmdfm.kdfl */
            else if( lkcharExact( "APPEND#$",9, MCPFN, kstring,MCPFN+1,
                     &nstring ) ){
                memset ( kmdfm.kwfl , ' ' , MAXCHARS - 1 );
                cmdfm.nwfl = 0;
                ic1 = 0;
                ic2 = 0;
                while ( lnxtcl( kmdfm.kdfl,MAXCHARS, &ic1, &ic2 ) ){
                    strtemp1 = (char *)malloc(ic2-ic1+2);
                    strncpy(strtemp1,kmdfm.kdfl+ic1-1,ic2-ic1+1);
                    strtemp1[ic2-ic1+1] = '\0';

                    appendstring( kstring,MCPFN+1, strtemp1, ic2-ic1+2,
                                  kfile,MCPFN+1 );

                    free(strtemp1);

                    putcl( kmdfm.kwfl,MAXCHARS, kfile,MCPFN+1, nerr );
                    if( *nerr != 0 )
                        goto L_8888;
                    cmdfm.nwfl = cmdfm.nwfl + 1;
                }
                cmdfm.lovrrq = FALSE;
                lexpnd = TRUE;
            }

            /* -- "PREPEND string": prepend string to filenames in kmdfm.kdfl */
            else if( lkcharExact( "PREPEND#$",10, MCPFN, kstring,
                     MCPFN+1, &nstring ) ){
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

                    prependstring( strtemp1, nstring+1, strtemp2,
                                   ic2-ic1+2, kfile,MCPFN+1 );

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

            /* -- "DELETE string": delete string from filenames in kmdfm.kdfl */
            else if( lkcharExact( "DELETE#$",9, MCPFN, kstring,MCPFN+1,
                     &nstring ) ){
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

                    deletestring( strtemp1, nstring+1, strtemp2,
                                  ic2-ic1+2, kfile,MCPFN+1 );

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

            /* -- "CHANGE string1 string2": change string1 to string2 in
                   filenames from kmdfm.kdfl. */
            else if( lkcharExact( "CHANGE#$",9, MCPFN, kstring,MCPFN+1,
                     &nstring ) ){
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


            /* -- "filelist":  define a new filelist (or add to old filelist) */
            else if( lcdfl( kmdfm.kwfl,MCMSG+1, &cmdfm.nwfl ) )
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

        /* - Make sure the write filelist has as many entries as read filelist*/

        if( cmdfm.nwfl != cmdfm.ndfl ){
            *nerr = 1312;
            setmsg( "ERROR", *nerr );
            apimsg( cmdfm.nwfl );
            apimsg( cmdfm.ndfl );
            goto L_8888;
        }


        /* EXECUTION PHASE: */

        /* - Commit or rollback data according to cmdfm.icomORroll */
        alignFiles( nerr ) ;
        if( *nerr )
            return ;

        if( cmdfm.lechof && lexpnd ){
            setmsg( "OUTPUT", 0 );
            ic1 = 0;

            while ( lnxtcl( kmdfm.kwfl,MAXCHARS, &ic1, &ic2 ) ){

                /* -- Break pathname into directory and filename parts. */
                strtemp1 = malloc(ic2-ic1+2);
                strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
                strtemp1[ic2-ic1+1] = '\0';

                getdir( strtemp1, ic2-ic1+2, kcdir,MCPFN , kfile,MCPFN+1 );

                free(strtemp1);

                /* -- Echo the filename part if there is no directory part. */
                if( strcmp(kcdir,"        ") == 0 )
                        apcmsg( kfile,MCPFN+1 );

                /* -- Prepend the filename part with some special characters if
                 *    directory part is same as that of the previous file. */
                else if( kcdir[ 0 ] == '\0' ) {
                    cattemp = malloc(3+strlen(kfile)+1);
                    strcpy(cattemp, "...");
                    strcat(cattemp,kfile);
                    apcmsg( cattemp, 3+strlen(kfile)+1 );
                    free(cattemp);
                }
                /* -- Echo complete pathname if directory part is different. */
                else{
                    apcmsg2(&kmdfm.kwfl[ic1 - 1],ic2-ic1+1);
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
             *     concatenate directory name with file name part of write file
             *     list.
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

                    getdir( strtemp1, ic2-ic1+2, kdirpart, MCPFN+1,
                            strtemp2,-(nwrdir+1)+130);
                    subscpy(kfile,nwrdir,-1,MCPFN,strtemp2);

                    free(strtemp1);
                    free(strtemp2);
                }
                else{
                    fstrncpy( kfile, MCPFN, " ", 1);

                    strtemp1 = malloc(ic2-ic1+2);
                    strncpy(strtemp1,kmdfm.kwfl+ic1-1, ic2-ic1+1);
                    strtemp1[ic2-ic1+1] = '\0';

                    getdir( strtemp1, ic2-ic1+2, kdirpart,MCPFN+1, kfile,MCPFN+1
 );

                    free(strtemp1);
                }
            }
            else
                fstrncpy( kfile, MCPFN, kmdfm.kwfl+ic1 - 1, ic2 - ic1 + 1);

            /* - Create file. */

            zdest( kfile, strlen( kfile ) , &nderr );
            znfiles( &nun, kfile, strlen( kfile ) , "TEXT",5, nerr );
            if( *nerr != 0 )
                goto L_8888;

            if( cmdfm.liftype ) {
                /* Write the data type. */
                switch( *iftype ) {
                  case ITIME:  fprintf( nun, "Time Series\n" ) ;
                               break ;
    
                  case IRLIM:  fprintf( nun, "Spectral:  real/imaginary\n" ) ;
                               break ;
    
                  case IAMPH:  fprintf( nun, "Spectral:  amplitude/phase\n" ) ;
                               break ;
    
                  case IXY:    fprintf( nun, "XY file\n" ) ;
                               break ;
    
                  case IXYZ:   fprintf( nun, "XYZ file\n" ) ;
                               break ;
    
                  default:     if( ndx2 <= 1 )
                                   fprintf( nun, "No datatype specified:  treated as Time Series\n" ) ;
                               else
                                   fprintf( nun, "No datatype specified: treated as XY data\n" ) ;
                               break ;
                } /* end switch */
            } /* end if( cmdfm.liftype ) */
            
            if( cmdfm.lheader ) {
                /* if leven, write begin and delta */
                if( *leven ) {
                    fprintf( nun, "Begin time: %f ;  Delta time: %f\n" ,
                             *begin , *delta ) ;
                }
                else
                    fprintf( nun, "Unevenly spaced data.\n" ) ;

                if( *iftype == ITIME && *leven == TRUE ) {
		    int check ;
                    for( idx = 0 ; idx < *npts ; idx++ ) {
                        check = fprintf( nun , "%0 13.6e\n" , cmmem.sacmem[ndx1][idx] );
                    }
		    fflush( nun ) ;
                }
                else {
                    for( idx = 0 ; idx < *npts ; idx++ ) {
                        fprintf( nun , "%0 13.6e\t%0 13.6e\n" ,
                                 cmmem.sacmem[ndx1][idx],
                                 cmmem.sacmem[ndx2][idx] );
                    }
		    fflush( nun ) ;
                }
            } /* end if( cmdfm.lheader ) */

            else if( *leven ){
               switch( *iftype ) {
                 case ITIME : /* one calculated column, one read column */
                   if( *begin == cmhdr.fundef || *delta == cmhdr.fundef ) {
                       /* error */
                       *nerr = 1393 ;
                       setmsg( "WARNING" , *nerr ) ;
                       sprintf( junk , " %d" , jdfl ) ;
                       apcmsg( junk , strlen( junk ) + 1 ) ;
                       outmsg() ;
                       clrmsg() ;
                       *nerr = 0 ;
                   } 
                   for( idx = 0 ; idx < *npts ; idx++ ) {
                       fprintf( nun , "%0 13.6e\t%0 13.6e\n" , 
                                *begin + ( *delta * (float) idx ) ,
                                cmmem.sacmem[ndx1][idx] ) ;
                   }
                   fflush( nun ) ;
                   break ;

                 case IRLIM : case IAMPH : case IXYZ : /* one calc, two read */
                   if( *begin == cmhdr.fundef || *delta == cmhdr.fundef ) {
                       /* error */
                       *nerr = 1393 ;
                       setmsg( "WARNING" , *nerr ) ;
                       sprintf( junk , " %d" , jdfl ) ;
                       apcmsg( junk , strlen( junk ) + 1 ) ;
                       outmsg() ;
                       clrmsg() ;
                       *nerr = 0 ;
                   }
                   for( idx = 0 ; idx < *npts ; idx++ ) {
                       fprintf( nun , "%0 13.6e\t%0 13.6e\t%0 13.6e\n" , 
                                *begin + ( *delta * (float) idx ) ,
                                cmmem.sacmem[ndx1][idx] ,
                                cmmem.sacmem[ndx2][idx] ) ;
                   }
                   fflush( nun ) ;
                   break ;

                 case IXY :  /* two read columns */
                   for( idx = 0 ; idx < *npts ; idx++ ) {
                       fprintf( nun , "%0 13.6e\t%0 13.6e\n" , 
                                cmmem.sacmem[ndx1][idx] ,
                                cmmem.sacmem[ndx2][idx] ) ;
                   }
                   fflush( nun ) ;
                   break ;

                 default : /* error */
                           *nerr = 1393 ;
                           setmsg( "WARNING" , *nerr ) ;
                           sprintf( junk , " %d" , jdfl ) ;
                           apcmsg( junk , strlen( junk ) + 1 ) ;
                           outmsg() ;
                           clrmsg() ;
                           *nerr = 0 ;
                           break ;
               } /* end switch */
            } /* end elseif( *leven ) */

            else {  /* lheader if false, *leven is false */
                switch( *iftype ) {
                  case ITIME : case IXY :  /* write 2 read columns */
                   for( idx = 0 ; idx < *npts ; idx++ ) {
                       fprintf( nun , "%0 13.6e\t%0 13.6e\n" ,
                                cmmem.sacmem[ndx1][idx] ,
                                cmmem.sacmem[ndx2][idx] ) ;
                   }
                   fflush( nun ) ;
                   break ;

                  case IRLIM: case IAMPH: case IXYZ: /* pretend it's leven */
                    if( *begin == cmhdr.fundef || *delta == cmhdr.fundef ) {
                        /* error */
                       *nerr = 1393 ;
                       setmsg( "WARNING" , *nerr ) ;
                       sprintf( junk , " %d" , jdfl ) ;
                       apcmsg( junk , strlen( junk ) + 1 ) ;
                       outmsg() ;
                       clrmsg() ;
                       *nerr = 0 ;
                    }
                    for( idx = 0 ; idx < *npts ; idx++ ) {
                       fprintf( nun , "%0 13.6e\t%0 13.6e\t%0 13.6e\n" , 
                                *begin + ( *delta * (float) idx ) ,
                                cmmem.sacmem[ndx1][idx] ,
                                cmmem.sacmem[ndx2][idx] ) ;
                    }
                    fflush( nun ) ;
                    break ;

                  default: /* error */
                           *nerr = 1393 ;
                           setmsg( "WARNING" , *nerr ) ;
                           sprintf( junk , " %d" , jdfl ) ;
                           apcmsg( junk , strlen( junk ) + 1 ) ;
                           outmsg() ;
                           clrmsg() ;
                           *nerr = 0 ;
                           break ;
                } /* end switch */
            }
        } /* end for ( jdfl ) */

L_8888:

        zcloses( &nun, &nderr );

        return;

}

