#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"


void apcmsg2(char* kalpha, int kalpha_s);


int /*FUNCTION*/ rdhdr(idfl, nun, nerr)
int idfl, *nun, *nerr;
{
        int ic1, ic2, ncerr, ndaerr, nlcdsk, nlcmem, numrd;
	void zgetc(), zrabs(), zwabs();
        char *strtemp;
        float *buffer;
        int *hdrVer, lswap = 0 , writehdr = 0;
        const int versionLocation = 76 ;


	/*=====================================================================
	 * PURPOSE: To read header from currently open SAC file into memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    NUN:     Fortran file unit on which data file is open. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred. [i]
	 *             Potential error numbers: 1317, 1318.
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:    MCPW
	 *    HDR:     MCMHDR, MKHDR, NVHDRC, FUNDEF
	 *    DFM:     NDXHDR, KDFL
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     All
	 *    DFM:     SACMEM
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZRABS, COPY, ZGETC, ZPUTC, NEWHDR, PUTFIL, UPDHDR,
	 *             ZWABS, GTOUTM, DISTAZ, NEWSTN
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NUMRD:   Number of words to read from disk file. [i]
	 *    NLCDSK:  Location in disk file to read data from. [i]
	 *    NLCMEM:  Location in SACMEM array to read data into. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870803:  Header update logic changed due to opening files 'RODATA'.
	 *    860130:  Changed to new message handling package.
	 *    850415:  Changes due to restructuring of DFM common block.
	 *    821122:  Added recomputation of ENND every time file is read.
	 *    821001:  Added check of LCALDA before calculating distance/azimuth.
	 *    820909:  Mod due to placing KHDR in its own common block.
	 *    820901:  Fixed bug that was causing NZYEAR to be clobbered.
	 *    811026:  Documented subroutine.
	 *    810924:  Included current century in NZYEAR if it was lt 100.
	 *    810120:  Changed to output message retrieval from disk.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850415
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Read header into memory. */

	nlcmem = Ndxhdr[idfl];
        numrd = MHDRFILE;

	nlcdsk = 0;
	
	/* These were set to 1 because lint revealed they were being used before being set. No idea if 1 is correct. */
	ic1 = 1;
	ic2 = 1;

        if((buffer=(float *)malloc(MHDRFILE * 4)) == NULL){
            printf("error allocating input buffer-rdhdr\n");
            *nerr = 114;
            goto L_8888;
	}

        /* read raw header data into buffer */
	zrabs( nun, buffer, numrd, &nlcdsk, nerr );

        /* determine if the data needs to be swapped. */
        hdrVer = (int *)( buffer + versionLocation ) ;
        if( *hdrVer < 1 || *hdrVer > cmhdr.nvhdrc ){
            byteswap( (void *)hdrVer, 4 ) ;
            if( *hdrVer < 1 || *hdrVer > cmhdr.nvhdrc ){
                *nerr = 1317 ;
                setmsg( "ERROR", *nerr ) ;
                apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
                aplmsg( "not in sac format, nor byteswapped sac format.", 62 );
                goto L_8888 ;
            }
            else{
                byteswap( (void *)hdrVer, 4 ) ; /* swap back, so it can be */
                lswap = 1 ;                     /* swapped again with the rest. */
            }
        }

        /* move raw data into header location, byteswapping numeric headers
           if appropriate. */
        map_hdr_in(cmmem.sacmem[nlcmem],buffer, lswap );

        free(buffer);

	if( *nerr != 0 )
	    goto L_8888;

	/* - Copy header from working memory into header common. */

	copy( (int*)cmmem.sacmem[nlcmem], (int*)&Fhdr[1], MCMHDR );
	zgetc( cmmem.sacmem[nlcmem]+MCMHDR, kmhdr.khdr[0], (MCPW+1)* MKHDR );

	/* - Update the header if it is in an old format:
	 *   (1) Close file and open it for writing.
	 *   (2) Write updated header.
	 *   (3) Close file again and open it for reading only.
	 *   (4) Send a warning message to inform user of all of this. */

	if( *nvhdr > 0 && *nvhdr < cmhdr.nvhdrc ){
	    lnumcl( kmdfm.kdflrq,MAXCHARS , idfl, &ic1, &ic2 );
	    updhdr( nerr );
	    if( *nerr == 0 )
		putfil( idfl, nerr );
	    if( *nerr != 0 ){
		*nerr = 1318;
		setmsg( "ERROR", 1318 );
                apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
		aplmsg( "File is bad. Header could not be updated.",42 );
		goto L_8888;
	    }
	    setmsg( "WARNING", 1318 );
            apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
	    zclose( nun, &ncerr );

            strtemp = malloc(ic2-ic1+2);
            strncpy(strtemp,kmdfm.kdflrq+ic1 - 1,ic2-ic1+1);
            strtemp[ic2-ic1+1] = '\0';

	    zopen_sac( nun, strtemp, ic2 - ic1 + 2, "DATA",5, nerr );

            free(strtemp);

	    if( *nerr != 0 ){
		*nerr = 0;
		aplmsg( "Insufficient access rights to update disk file." ,48 );
		aplmsg( "Header in memory has been updated.",35 );
		goto L_4000;
	    }
	    nlcdsk = 0;

            if((buffer=(float *)malloc(MHDRFILE * 4)) == NULL){
                printf("error allocating output buffer-rdhdr\n");
                *nerr = 115;
                goto L_8888;
	    }

            map_hdr_out(cmmem.sacmem[nlcmem],buffer, &writehdr);

	    zwabs( nun, buffer, MHDRFILE, &nlcdsk, nerr );

            free(buffer);

	    if( *nerr != 0 ){
		*nerr = 0;
		aplmsg( "Could not update header in disk file.",38 );
		aplmsg( "Header in memory has been updated.",35 );
		zclose( nun, &ncerr );
		goto L_4000;
	    }
	    zclose( nun, &ncerr );
	    aplmsg( "Headers in memory and on disk have been updated." ,49 );
L_4000:
	    outmsg();
	    clrmsg();

            strtemp = malloc(ic2-ic1+2);
            strncpy(strtemp,kmdfm.kdflrq+ic1 - 1,ic2-ic1+1);
            strtemp[ic2-ic1+1] = '\0';

	    zopen_sac( nun, strtemp, ic2 - ic1 + 2, "RODATA",7, &ncerr );

            free(strtemp);
	} /* end if( *nvhdr > 0 && *nvhdr < cmhdr.nvhdrc ) */
	else if( *nvhdr <= 0 || *nvhdr > cmhdr.nvhdrc ){
	    *nerr = 1318;
	    setmsg( "ERROR", *nerr );
	    lnumcl( kmdfm.kdflrq,MAXCHARS , idfl, &ic1, &ic2 );
            apcmsg2(&kmdfm.kdflrq[ic1 - 1],ic2-ic1+1);
	    aplmsg( "Header version number is incorrect.",36 );
	    goto L_8888;
	} /* end else if( *nvhdr <= 0 || *nvhdr > cmhdr.nvhdrc ) */

	/* - Compute distance, azimuth, etc. if proper header fields are present. */

	if( (((*stla != cmhdr.fundef && *stlo != cmhdr.fundef) && *evla != 
	 cmhdr.fundef) && *evlo != cmhdr.fundef) && *lcalda ){
            if ((fabs(*stla - *evla) < RNDOFF ) && (fabs(*stlo - *evlo) < RNDOFF)) {
		*dist = cmhdr.fundef;
		*az = cmhdr.fundef;
		*baz = cmhdr.fundef;
		*gcarc = cmhdr.fundef;
	    }else {
		*az = 0.;
		*baz = 0.;
		*gcarc = 0.;
		*dist = 0.;
		distaz( *evla, *evlo, (float*)stla, (float*)stlo, 1, (float*)dist, 
		 (float*)az, (float*)baz, (float*)gcarc, &ndaerr );
		if( ndaerr != 0 ){
		    *dist = cmhdr.fundef;
		    *az = cmhdr.fundef;
		    *baz = cmhdr.fundef;
		    *gcarc = cmhdr.fundef;
		}
	    }
	} /* end if( (((*stla != cmhdr.fundef ... ) */

	/* - Update station name if necessary. */

	newstn( kstnm, kstnm,9 );

	/* - Adjust reference year if necessary. */

	if( *nzyear >= 0 && *nzyear <= 99 )
	    *nzyear = *nzyear + 1900;

	/* - Compute end time if evenly-spaced file. */

	if( *leven )
	    *ennd = *begin + *delta*(float)( *npts - 1 );

	/* - Copy header back to its location in working memory. */

	putfil( idfl, nerr );
	if( *nerr != 0 )
	    goto L_8888;

L_8888:
	return lswap ;

} /* end of function */

