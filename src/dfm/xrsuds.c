#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/com.h"
#include "../../inc/dfm.h"


void alignFiles ( int *nerr );
int lckeyExact(char* kkey,int kkey_s);
void readsuds( int lmore, char* kdirin, int kdirin_s, char* kdflin, int kdflin_s, int ndflin, int Verbose, int isASCII, float MaxMem, int* nerr);



void /*FUNCTION*/ xrsuds(nerr)
int *nerr;
{
	char _c0[2], kvers_in[9];
        static char kdflin[MCMSG+1], kdflinsave[MCMSG+1];
	int llocal, lmore, lshift, lscale, larray ;
	int jcdflbeg, jcdflend, jcparmbeg, nchar, icomORroll;
        static int ndflin;
        static int Verbose = 0;
        static int ibinORasc;

	char kmag[4] ;	/* magnitude type: mb, ms, or ml. maf 970206 */

	/*=====================================================================
	 * PURPOSE:  To execute the action command READSUDS.
	 *           This command reads data from disk into SAC's memory.
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
	 *    IVCSS:   Flag used to relay version of CSS format desired. [i]
	 *    LMORE:   Flag used when adding files to current DFL. [l]
	 *    LDATA:   Flag used to tell READFL that  headers and data are
	 *             to be read into memory. [l]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    970206:  New option to specify which magnitude to read.  maf
	 *    910402:  New code.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Parse position-dependent tokens: */

	lmore = FALSE;
	/* - Use these to mark the beg/end of dfl, which could be followed by
	 *   other keyworded parameters */
	jcdflbeg = 0;
	jcdflend = 0;

	while ( lcmore( nerr ) ){

		jcparmbeg = cmcom.jcom;

		/* -- "MORE":  signifies addition of more files to current read filelist
		 *             rather than replacement of current list with new one. */
		if( lckey( "MORE#$",7 ) && cmdfm.ndfl > 0 ){
			lmore = TRUE;
			continue ;
		}

                /* -- "VERBOSE ON|OFF":  turn Verbose mode on or off. */
                else if( lklog( "VER$BOSE",9, &Verbose ) ){
			if( jcdflbeg > 0 && jcdflend == 0 )
				jcdflend = jcparmbeg - 1;
			continue ;
                }
                /* -- "SHIFT ON|OFF":  turn calibration on or off. */
                else if( lklog( "SHIFT$",7, &lshift ) ){
			cmdfm.lshift = lshift ;
			if( jcdflbeg > 0 && jcdflend == 0 )
				jcdflend = jcparmbeg - 1;
			continue ;
                }

                /* -- "SCALE ON|OFF":  turn time shift on or off. */
                else if( lklog( "SCALE$",7, &lscale ) ){
                        cmdfm.lscale = lscale ;
		/*	if ( lscale ) {
				setmsg ( "WARNING" , 2119 ) ;
				outmsg () ;
				clrmsg () ;
			}*/
			if( jcdflbeg > 0 && jcdflend == 0 )
				jcdflend = jcparmbeg - 1;
                        continue ;
                }

		/* -- "MAXMEM v":  change maximum fractional memory used by SeisMgr. */
                else if( lkreal( "MAX#MEM$",9, &MaxMem ) )
                { /* do nothing */ }

		/* -- "DIR CURRENT|name":  set the name of the default subdirectory. */
		else if( lkchar( "DIR#$",6, MCPFN, kmdfm.krdcssdir,MCPFN+1, &nchar ) ){
			if( memcmp(kmdfm.krdcssdir,"CURRENT",7) == 0 || memcmp(kmdfm.krdcssdir
			 ,"current",7) == 0 ){
				cfmt( "ILLEGAL PARAM VALUE: current$",30 );
				cresp();
				return ;
			}
			else if( kmdfm.krdcssdir[nchar - 1] != KDIRDL ){
                                _c0[0] = KDIRDL;
                                _c0[1] = '\0';
				subscpy( kmdfm.krdcssdir, nchar, -1, MCPFN, _c0 );
			}
			if( jcdflbeg > 0 && jcdflend == 0 )
				jcdflend = cmcom.jcom - 1;
			continue;
		}

		/* -- "MAGNITUDE|mb|ms|ml|def":  specify a field for magnitude, or if def
			is found, use the algorithm to determine which magnitude to read.
			maf 970206. */
		else if ( lkchar ( "MAG#NITUDE$", 12 , 4 , kmag , 4 , &nchar ) ) {
			if ( kmag [ 0 ] == 'm' || kmag[ 0 ] == 'M' ) {
			    if ( kmag [ 1 ] == 'b' || kmag [ 1 ] == 'B' ) 
				cmdfm.nMagSpec = MbMag ;
			    else if ( kmag [ 1 ] == 's' || kmag [ 1 ] == 'S' )
				cmdfm.nMagSpec = MsMag ;
			    else if ( kmag [ 1 ] == 'l' || kmag [ 1 ] == 'L' )
				cmdfm.nMagSpec = MlMag ;
			    else {
                                cfmt( "ILLEGAL PARAM VALUE:$",22 );
                                cresp();
                                return ;
                            }
			} /* end if ( kmag [ 0 ] == 'm' ... ) */
			else if ( strncmp ( kmag , "def" , 3 ) == 0 || 
				  strncmp ( kmag , "DEF" , 3 ) == 0 )
			    cmdfm.nMagSpec = Any ;
			else {
                            cfmt( "ILLEGAL PARAM VALUE:$",22 );
                            cresp();
                            return ;
                        }
		} /* end if ( lkchar ( "MAG#NITUDE$", ... ) */

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



		/* -- Else assume it to be beginning/continuing dfl, if not recognized as above
		 *    Use kdflin as dummy buffer to skip a string  */
		else{
			if( jcdflbeg == 0 ){
				jcdflbeg = jcparmbeg;
			}
			else if( jcdflend == 0 )
			{ /* do nothing */ }
			else{
				cfmt( "Duplicate DFL, check spelling of prior keywds:$"
				 ,48 );
				cresp();
				return ;
			}
			lcchar( MCPFN, kdflin,MCMSG+1, &ndflin );
			continue;
		} /* end else */
	} /* end while ( lcmore( nerr ) ) */

	if( jcdflbeg > 0 && jcdflend == 0 )
		jcdflend = cmcom.ncom;

	/* - Parse position-independent tokens: */

	/* - Loop on each token in command: */

	if( jcdflbeg == 0 && jcdflend == 0 ){
            fstrncpy(kdflin,MCMSG,kdflinsave,MCMSG);
	}
	else{
		cmcom.jcom = jcdflbeg;
		cmcom.ncom = jcdflend;

		while ( lcmore( nerr ) ){
			/* -- "filelist":  define a new input filelist. */
			if( lcdfl( kdflin,MCMSG+1, &ndflin ) ){
                           fstrncpy(kdflinsave,MCMSG,kdflin,MCMSG);
				/* -- Bad syntax. */
			}
			else{
				cfmt( "ILLEGAL OPTION:$",17 );
				cresp();
			}
		} /* end while */
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		return ;

	if( ndflin == 0 ){
		ndflin = 1;
		fstrncpy( kdflin, MCMSG, " *", 2);
	}

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


	/* - Expand the filelist and read the files into memory.
	 * -- Parameter kstation, kband, korient are picked up in readcfl from
	 *    ../../inc/dfm */
	
	cmdfm.nreadflag = LOW ;
	readsuds( lmore, kmdfm.krdcssdir,MCPFN+1, kdflin,MCMSG+1, ndflin, Verbose, 
                cmdfm.lrascii, MaxMem, nerr );
		
	return;

} /* end of function */

