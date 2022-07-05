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
#include "sam.h"

void alignFiles ( int *nerr );
int lckeyExact(char* kkey,int kkey_s);

void /*FUNCTION*/ xwsp(nerr)
int *nerr;
{
	char kwspnm[MCPFN+1];
	int lconv;
	int _l1, ic1, ic2, icomORroll, index, ispectype, jdfl, nderr, 
	 ndx1, ndx2, nfreq, nlen, nun, nwspnm;
	float temp[MKMHDR];
	void zputc(), zwabs();
        float *buf1, *buf2;
        int memerr;
        int writehdr=0;

	/*=====================================================================
	 * PURPOSE:  To execute the action command WRITESP.
	 *           This command writes spectral files in memory to disk
	 *           as two separate files.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *      NERR:  Error return flag
	 *=====================================================================
	 * MODULE/LEVEL:  SAM/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL, KDFL
	 *    HDR:     IFTYPE, IRLIM, IAMPH, NPTS, DELTA, B, E,
	 *             DEPMIN, DEPMAX, DEPMEN
	 *    SAM:     NWSPFL, KWSPFL, LWAMPH, LWRLIM, KWSPTP,
	 *             KWSPS1, KWSPS2, LWSPC1, LWSPC2
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    SAM:     KWSPS1, KWSPS2
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  LCMORE, CFMT, CRESP, LCLIST, LCKEY, LCDFL,
	 *             VFLIST, VFSPEC,
	 *             GTOUTM, GETFIL, TOAMPH, TORLIM, INDEXB,
	 *             ZDEST, ZNFILE, ZWABS, ZCLOSE, ZPUTC
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    KWSPNM:  Name of disk file being written.
	 *    LCONV:   .TRUE. if spectral file needs to be temporarily
	 *             converted from one type to another.
	 *    TEMP:    Scratch space used to store character information
	 *             before writing it to disk.
	 *=====================================================================
	 * KNOWN ERRORS:
	 * - POWER option not implemented.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
	/* PARSING PHASE: */

	/* - Loop on each token in command: */

	while ( lcmore( nerr ) ){

		/* -- "ASIS/RLIM/AMPH/RL/IM/AM/PH/POWER":  select components to write. */
		if( lclist( (char*)kmsam.ksptpl,9, cmsam.nsptpl, &index ) ){
			strcpy( kmsam.kwsptp, kmsam.ksptpl[index - 1] );
			/* Case branch:   ASIS,RLIM,AMPH,RL  ,IM  ,AM  ,PH  ,POWER */
			switch( index ){
			    case 1:
				cmsam.lwamph = FALSE;
				cmsam.lwrlim = FALSE;
				cmsam.lwspc1 = TRUE;
				cmsam.lwspc2 = TRUE;
				break ;
			    case 2:
				cmsam.lwamph = FALSE;
				cmsam.lwrlim = TRUE;
				cmsam.lwspc1 = TRUE;
				cmsam.lwspc2 = TRUE;
				strcpy( kmsam.kwsps1, ".rl     " );
				strcpy( kmsam.kwsps2, ".im     " );
				break ;
			    case 3:
				cmsam.lwamph = TRUE;
				cmsam.lwrlim = FALSE;
				cmsam.lwspc1 = TRUE;
				cmsam.lwspc2 = TRUE;
				strcpy( kmsam.kwsps1, ".am     " );
				strcpy( kmsam.kwsps2, ".ph     " );
				break ;
			    case 4:
				cmsam.lwamph = FALSE;
				cmsam.lwrlim = TRUE;
				cmsam.lwspc1 = TRUE;
				cmsam.lwspc2 = FALSE;
				strcpy( kmsam.kwsps1, ".rl     " );
				strcpy( kmsam.kwsps2, ".na     " );
				break ;
			    case 5:
				cmsam.lwamph = FALSE;
				cmsam.lwrlim = TRUE;
				cmsam.lwspc1 = FALSE;
				cmsam.lwspc2 = TRUE;
				strcpy( kmsam.kwsps1, ".na     " );
				strcpy( kmsam.kwsps2, ".im     " );
				break ;
			    case 6:
				cmsam.lwamph = TRUE;
				cmsam.lwrlim = FALSE;
				cmsam.lwspc1 = TRUE;
				cmsam.lwspc2 = FALSE;
				strcpy( kmsam.kwsps1, ".am     " );
				strcpy( kmsam.kwsps2, ".na     " );
				break ;
			    case 7:
				cmsam.lwamph = TRUE;
				cmsam.lwrlim = FALSE;
				cmsam.lwspc1 = FALSE;
				cmsam.lwspc2 = TRUE;
				strcpy( kmsam.kwsps1, ".na     " );
				strcpy( kmsam.kwsps2, ".ph     " );
				break ;
			    case 8:
				*nerr = 1012;
				setmsg( "ERROR", *nerr );
				apcmsg( kmsam.kwsptp,9 );
				cmsam.lwamph = TRUE;
				cmsam.lwrlim = FALSE;
				cmsam.lwspc1 = TRUE;
				cmsam.lwspc2 = FALSE;
				strcpy( kmsam.kwsps1, ".po     " );
				strcpy( kmsam.kwsps2, ".na     " );
			} /* end switch */
		} /* end if ( lclist ) */

		/* -- "OVER":  overwrite data file list. */
		else if( lckey( "OVER$",6 ) )
			cmsam.lwspov = TRUE;

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


		/* -- Parse list of file names for write. */
		else if( lcdfl( kmsam.kwspfl,MAXCHARS, &cmsam.nwspfl ) )
			cmsam.lwspov = FALSE;

		/* -- Bad syntax. */
		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();
		}
	}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

	if( *nerr != 0 )
		goto L_8888;

	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Check to make sure all files are spectral files. */

	vfspec( nerr );
	if( *nerr != 0 )
		goto L_8888;
	ispectype = *iftype;

	/* - Check length of write file list vs data file list. */

	if( !cmsam.lwspov ){
		if( cmsam.nwspfl != cmdfm.ndfl ){
			*nerr = 1312;
			setmsg( "ERROR", *nerr );
			apimsg( cmsam.nwspfl );
			apimsg( cmdfm.ndfl );
			goto L_8888;
		}
	}

	/* EXECUTION PHASE: */

        /* - Commit or rollback data according to lmore and cmdfm.icomORroll */
        alignFiles ( nerr ) ;
	if ( *nerr )
	    return ;


	/* - Perform the requested function on each file in DFL. */

	for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

		/* -- Get the next file in DFL, moving header to CMHDR. */

		getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr );
		if( *nerr != 0 )
			goto L_8888;

		/* -- Convert spectral file type if needed. */

		if( cmsam.lwamph && ispectype == *irlim ){
			toamph( cmmem.sacmem[ndx1], cmmem.sacmem[ndx2], *npts, cmmem.sacmem[ndx1], 
			 cmmem.sacmem[ndx2] );
			lconv = TRUE;
			ispectype = *iamph;
		}
		else if( cmsam.lwrlim && ispectype == *iamph ){
			torlim( cmmem.sacmem[ndx1], cmmem.sacmem[ndx2], *npts, cmmem.sacmem[ndx1], 
			 cmmem.sacmem[ndx2] );
			lconv = TRUE;
			ispectype = *irlim;
		}
		else{
			lconv = FALSE;
		}

		/* -- Determine character length of output file name. */

		if( cmsam.lwspov ){
			lnumcl( kmdfm.kdfl,MAXCHARS, jdfl, &ic1, &ic2 );
			fstrncpy( kwspnm, MCPFN, kmdfm.kdfl+ic1 - 1,min(ic2,MAXCHARS-1) - 
			 ic1 + 1);
		}
		else{
			lnumcl( kmsam.kwspfl,MAXCHARS, jdfl, &ic1, &ic2 );
			fstrncpy( kwspnm, MCPFN, kmsam.kwspfl+ic1 - 1,min(ic2,MAXCHARS-1) - 
			 ic1 + 1);
		}
		nwspnm = min( ic2 - ic1 + 1, MCPFN - 3 );

		/* -- Determine suffixes if KWSPTP is 'ASIS'. */

		if( strcmp(kmsam.kwsptp,"ASIS    ") == 0 ){
			if( ispectype == *irlim ){
				strcpy( kmsam.kwsps1, ".rl     " );
				strcpy( kmsam.kwsps2, ".im     " );
			}
			else{
				strcpy( kmsam.kwsps1, ".am     " );
				strcpy( kmsam.kwsps2, ".ph     " );
			}
		}

		/* -- Adjust header for writes. */

		nfreq = *npts/2 + 1;
		*npts = nfreq;
		*b = 0.;
		*e = *delta*(float)( nfreq - 1 );
		*iftype = *ixy;

		/* -- Write first spectral component if requested. */

		if( cmsam.lwspc1 ){

			/* --- Prepare new name */
                        fstrncpy( kwspnm, MCPFN, kwspnm, min(nwspnm,MCPFN));
                        fstrncpy( kwspnm+min(nwspnm,MCPFN), MCPFN-min(nwspnm,MCPFN),
                                   kmsam.kwsps1, 3);

			/* --- Create file. */
			zdest( kwspnm,MCPFN+1, &nderr );
			znfile( &nun, kwspnm,MCPFN+1, "DATA",5, nerr );
			if( *nerr != 0 )
				goto L_8888;
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

			/* --- Adjust header for component specific values. */
			extrma( cmmem.sacmem[ndx1], 1, nfreq, depmin, depmax, depmen );
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

			/* --- Write header. */

			zputc( kmhdr.khdr,9, temp, (MCPW+1)*MKHDR );
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

                        if((buf1=(float *)malloc(MHDR * sizeof(float))) == NULL){
                          printf("error allocating file buffer-xwsp\n");
                          *nerr = 115;
                          goto L_8888;
		        }

                        if((buf2=(float *)malloc(MHDRFILE * sizeof(float))) == NULL){
                          printf("error allocating memory buffer-xwsp\n");
                          *nerr = 115;
                          goto L_8888;
		        }
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

                        memcpy((char *)buf1,(char *)&Fhdr[1],MCMHDR*sizeof(float));
                        memcpy((char *)(buf1 + (MCMHDR)),(char *)temp,
                                                     sizeof(float)*MKMHDR);
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif
                        writehdr=0;
                        map_hdr_out(buf1,buf2, &writehdr);
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

                        _l1 = 0;
                        zwabs(&nun,buf2,MHDRFILE,&_l1,nerr);
                        if( *nerr != 0 ) goto L_8888;
#ifdef DEBUG
        memerr = (int)malloc_verify();
#endif

                        free(buf1);
                        free(buf2);

			/* --- Write data. */
                        _l1 = MHDRFILE;
			zwabs( &nun, cmmem.sacmem[ndx1], nfreq, &_l1, nerr );
			if( *nerr != 0 )
				goto L_8888;

			/* --- Close file. */
			zclose( &nun, nerr );
			if( *nerr != 0 )
				goto L_8888;

		}

		/* -- Write second spectral component if requested. */

		if( cmsam.lwspc2 ){

			/* --- Prepare new name */
                        fstrncpy( kwspnm, MCPFN, kwspnm, min(nwspnm,MCPFN));
                        fstrncpy( kwspnm+min(nwspnm,MCPFN), MCPFN-min(nwspnm,MCPFN),
                                  kmsam.kwsps2, 3);

			/* --- Create file. */
			zdest( kwspnm,MCPFN+1, &nderr );
			znfile( &nun, kwspnm,MCPFN+1, "DATA",5, nerr );
			if( *nerr != 0 )
				goto L_8888;

			/* --- Adjust header for component specific values. */
			extrma( cmmem.sacmem[ndx2], 1, nfreq, depmin, depmax, depmen );

			/* --- Write header. */
                        if((buf1=(float *)malloc(MHDR * sizeof(float))) == NULL){
                          printf("error allocating file buffer-xwsp\n");
                          *nerr = 115;
                          goto L_8888;
		        }

                        if((buf2=(float *)malloc(MHDRFILE * sizeof(float))) == NULL){
                          printf("error allocating memory buffer-xwsp\n");
                          *nerr = 115;
                          goto L_8888;
		        }

                        memcpy((char *)buf1,(char *)&Fhdr[1],MCMHDR*sizeof(float));
                        memcpy((char *)(buf1 + (MCMHDR)),kmhdr.khdr[0],
                                                     sizeof(float)*MKMHDR);
                        writehdr=0;
                        map_hdr_out(buf1,buf2,&writehdr);

                        _l1 = 0;
                        zwabs(&nun,buf2,MHDRFILE,&_l1,nerr);
                        if( *nerr != 0 ) goto L_8888;

                        free(buf1);
                        free(buf2);

			/* --- Write data. */
                        _l1 = MHDRFILE;
			zwabs( &nun, cmmem.sacmem[ndx2], nfreq, &_l1, nerr );
			if( *nerr != 0 )
				goto L_8888;

			/* --- Close file. */
			zclose( &nun, nerr );
			if( *nerr != 0 )
				goto L_8888;

		}

		/* -- Convert file back to original type if necessary. */

		if( lconv && ispectype == *irlim ){
			toamph( cmmem.sacmem[ndx1], cmmem.sacmem[ndx2], *npts, cmmem.sacmem[ndx1], 
			 cmmem.sacmem[ndx2] );
			ispectype = *iamph;
		}
		else if( lconv && ispectype == *iamph ){
			torlim( cmmem.sacmem[ndx1], cmmem.sacmem[ndx2], *npts, cmmem.sacmem[ndx1], 
			 cmmem.sacmem[ndx2] );
			ispectype = *irlim;
		}

	}

L_8888:

	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *    910417:  Transfter iftype value to ispectype, immed. after vfspec
	 *             to rid bug caused by "iftype=ixy" before component write
	 *    850328:  Added call to ZPUTC to copy char. info before writing.
	 *    821122:  Deleted use of temporary arrays.
	 *    820927:  Mod due to changes in HDR storage format.
	 *    820331:  Combined "parse" and "control" modules.
	 *    810327:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850124
	 *===================================================================== */

} /* end of function */

