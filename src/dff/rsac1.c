

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "hdr.h"

/* prototype */
int byteswap( void* swappee, int Nbytes ) ;

void /*FUNCTION*/ rsac1(kname1, yarray, nlen, beg, del, max_, nerr, kname_s)
char *kname1;   int kname_s;
float yarray[];
int *nlen;
float *beg, *del;
int *max_, *nerr;

{
	int  ncerr, nlcdsk, nun;
	float temp[MKMHDR], temp2[FILEMKMHDR];
        const int versionLocation = 76 ;
        int idx, *hdrVer, lswap = 0 ;
	void zgetc(), zrabs();
	char kname0[500], *kname;
	
	int i=0;
	while (*kname1 != '\0')
		kname0[i++]=*kname1++;
	kname0[i++]='\0';
	
	kname=&kname0[0];
	

	/*=====================================================================
	 * PURPOSE: To read an evenly spaced SAC file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KNAME:   Name of disk file to read. [c]
	 *             The name should be blank filled.
	 *    MAX:     Size of YARRAY. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    YARRAY:  Contains the data from the file. [fa]
	 *    NLEN:    Number of data points read. [i]
	 *             Will be less than or equal to MAX.
	 *    BEG:     Beginning value of independent variable. [f]
	 *    DEL:     Sampling interval of the independent variable. [f]
	 *    NERR:    Error return flag 0 if no error occurred. [i]
	 *             Possible values for this subroutine are:
	 *             =  801 if file is not evenly spaced.
	 *             = -803 if number of pitns in file is greater than MAX.
	 *             In this case, the first MAX points are read.
	 *=====================================================================
	 * MODULE/LEVEL: DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    HDR:     FUNDEF, MCMHDR, MKMHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     All.
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INIHDR, INILHF, ZGTFUN, ZOPEN, ZRABS, ZGETC, ZCLOSE,
	 *             GTOUTM, WRTXTD
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870902:  Added calls to INILHF and INIMSG as part of initialization.
	 *    870513:  Changed call to wrtxtd to wrtmsg.
	 *    830125:  Changes due to modified header common block.
	 *    810212:  Changed to output message retrieval from disk.
	 *    800919:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870902
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;
	

	/* - Initialize some common blocks if not already done. */

	if( cmhdr.fundef != -12345. ){
	    /*initsac();*/
	    initblkdata();
	    inihdr();
	    inilhf();
	    inimsg();
	}

	/* - Open the file. */

	zopen_sac( &nun, kname,kname_s, "RODATA",7, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Read the header into its common block. */

	nlcdsk = 0;
	zrabs( &nun, cmhdr.fhdr, MCMHDR, &nlcdsk, nerr );
	if( *nerr != 0 )
	    goto L_8888;

        /* determine if the data needs to be swapped. */
        hdrVer = (int *)( cmhdr.fhdr + versionLocation ) ;
        if( *hdrVer < 1 || *hdrVer > cmhdr.nvhdrc ){
            byteswap( (void *)hdrVer, 4 ) ;
            if( *hdrVer < 1 || *hdrVer > cmhdr.nvhdrc ){
                *nerr = 1317 ;
                setmsg( "ERROR", *nerr ) ;
                aplmsg( "not in sac format, nor byteswapped sac format.", 62 );
                goto L_8888 ;
            }
            else{
                byteswap( (void *)hdrVer, 4 ) ; /* swap back, so it can be */
                lswap = 1 ;                     /* swapped again with the rest. */
            }
        }

        if( lswap ){     /* byteswap all the non-character header elements. */
            float *ptr ;
            for( idx = 0, ptr = cmhdr.fhdr ; idx < MCMHDR ; idx++, ptr++ ){
                byteswap( (void*)ptr, 4 ) ;
            }
        }

	nlcdsk = nlcdsk + MCMHDR;
	zrabs( &nun, temp2, FILEMKMHDR, &nlcdsk, nerr );
	if( *nerr != 0 )
	    goto L_8888;
        map_chdr_in(temp,temp2);

	zgetc( temp, kmhdr.khdr, (MCPW+1)*MKHDR );

	/* - Make sure file is evenly spaced. */

	if( *leven ){
	    if( *npts <= *max_ ){
		*nlen = *npts;
	    }
	    else{
		*nlen = *max_;
		*nerr = -803;
	    }
	    *beg = *begin;
	    *del = *delta;
	}
	else{
	    *nerr = 801;
	    goto L_8888;
	}

	/* - Read in the data. */

	nlcdsk = nlcdsk + FILEMKMHDR;
	zrabs( &nun, yarray, *nlen, &nlcdsk, nerr );
	if( *nerr != 0 )
		goto L_8888;

        if( lswap ){        /* byteswap all the data points. */
            float *ptr ;
            for( idx = 0, ptr = yarray ; idx < *nlen ; idx++, ptr++ ){
                byteswap( (void*)ptr, 4 ) ;
            }
        }

	/* - Adjust several header fields. */

	*npts = *nlen;
	*ennd = *begin + (*npts - 1)**delta;

	/* - Print any error message and close the disk file. */

L_8888:

	zclose( &nun, &ncerr );
	
	return;

} /* end of function */




/* Wrapper to make the function more convenient for FORTRAN programmers. */

void /*FUNCTION*/ rsac1_ (kname1, yarray, nlen, beg, del, max_, nerr, kname_s)
char *kname1;   int kname_s;
float yarray[];
int *nlen;
float *beg, *del;
int *max_, *nerr;
{
	rsac1 ( kname1 , yarray , nlen , beg , del , max_ , nerr , kname_s ) ;
}
