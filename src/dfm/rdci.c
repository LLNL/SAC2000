#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

void /*FUNCTION*/ rdci(idfl, kname, kname_s, nlen, ndx1, ndx2, nerr)
int idfl;
char *kname;   int kname_s;
int *nlen, *ndx1, *ndx2, *nerr;
{
	int ilhdr[MLHDR], jdx, jj, jjj, ncards, nlcmem, nremdr;
        FILE *nun;
        char kiline[MCMSG+1];
        char *kiptr;

        float *Sacmem;

	int *const Ilhdr = &ilhdr[0] - 1;



	/*=====================================================================
	 * PURPOSE:  To read a SAC "card-image" data file into memory.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    KNAME:   Name of disk file to read. [c]
	 *    LDTA:    Set to .TRUE. if header and data are to be read. [l]
	 *             Set to .FALSE. if only header is to be read.
	 *    KFMT:    Format statement to use when reading data. [c]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NLEN:    Number of data points read. [i]
	 *    NDXH:    Index in SACMEM array of header. [i]
	 *    NDX1:    Index in SACMEM array of first data component. [i]
	 *    NDX2:    Index of second data component, if any. [i]
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:  
	 *    DFM:
	 *    HDR:     MFHDR, MNHDR, MIHDR, MLHDR, MKHDR, 
	 *             NVHDR, NUNDEF, FUNDEF, MHDR
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    HDR:     All.
	 *    DFM:     NDXHDR, NLNDTA, NDXDTA, NCOMP, KDFL
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZGTFUN, ZOPEN, UPDHDR, GTOUTM, PUTFIL, ZCLOSE
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    ILHDR:   Used to convert integer to logical header fields. [i]
	 *    NLNFIL:  Length of data file. [i] {UNUSED}
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    850730:  Changes due to new memory manager.
	 *             CHANGED NUMBER AND ORDER OF ARGUMENTS.
	 *    830607:  Fixed bug causing extra data point to be read.
	 *    810728:  Added argument specifying data format..
	 *    810120:  Changed to output message retrieval from disk.
	 *    800109:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  850730
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Open file. */

	zopens( &nun, kname,kname_s, "ROTEXT",7, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Read header. */

	jdx = 1;
	for( jj = 1; jj <= (MFHDR/5); jj++ ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }
	    for( jjj = jdx; jjj <= (jdx + 4); jjj++ ){
		kiptr = strtok ( jjj == jdx ? kiline : NULL , " " ) ;
		if ( !kiptr )
		    break ;
		Fhdr[jjj] = atof(kiptr);
	    }
	    jdx = jdx + 5;
	}

	jdx = 1;

	for( jj = 1; jj <= (MNHDR/5); jj++ ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }
	    for( jjj = jdx; jjj <= (jdx + 4); jjj++ ){
		kiptr = strtok ( jjj == jdx ? kiline : NULL , " " ) ;
		if ( !kiptr )
		    break ;
		Nhdr[jjj] = atol(kiptr);
	    }
	    jdx = jdx + 5;
	}

	jdx = 1;
	for( jj = 1; jj <= (MIHDR/5); jj++ ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }
	    for( jjj = jdx; jjj <= (jdx + 4); jjj++ ){
		kiptr = strtok ( jjj == jdx ? kiline : NULL , " " ) ;
		if ( !kiptr )
		    break ;
		Ihdr[jjj] = atol(kiptr);
	    }
	    jdx = jdx + 5;
	}

	jdx = 1;
	for( jj = 1; jj <= (MLHDR/5); jj++ ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }

	    for( jjj = 1; jjj <= 5; jjj++ ){
		kiptr = strtok ( jjj == jdx ? kiline : NULL , " " ) ;
		if ( !kiptr )
		    break ;
		Ilhdr[jjj] = atol(kiptr);
	    }

	    for( jjj = 1; jjj <= 5; jjj++ ){
		if( Ilhdr[jjj] == 1 ){
		    Lhdr[jdx] = TRUE;
		}
		else{
		    Lhdr[jdx] = FALSE;
		}
		jdx = jdx + 1;
	    }
	}

	/* read the character header values */
        if(fgets(kiline,MCMSG,nun)==NULL){
	    *nerr = 1319;
	    goto L_8888;
        }

        strncpy(kmhdr.khdr[0],kiline,8);
        kmhdr.khdr[0][8] = '\0';
        strncpy(kmhdr.khdr[1],kiline+8,8); /*  fails here was 8,17*/
        kmhdr.khdr[2][8] = '\0';

	for( jj = 4; jj <= MKHDR; jj += 3 ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }
	    for( jjj = jj; jjj <= (jj + 2); jjj++ ){
		strncpy(kmhdr.khdr[jjj-1],kiline+((jjj-jj)*8),8);
		kmhdr.khdr[jjj-1][8] = '\0';
	    }
	}

	/* - Update the header if necessary. */

	if( *nvhdr < cmhdr.nvhdrc ){
	    updhdr( nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - Make sure the most important header values are defined. */

	if( *npts == cmhdr.nundef || *begin == cmhdr.fundef ){
	    *nerr = 1319;
	    setmsg( "ERROR", *nerr );
	    goto L_8888;
	}

	/* - Move header to SACMEM array. */

	allamb( &cmmem, MHDR, &Ndxhdr[idfl], nerr );
	if( *nerr != 0 )
	    goto L_8888;
	putfil( idfl, nerr );

	/* - Set up data space. */

	putcl( kmdfm.kdfl,MAXCHARS, kname,kname_s, nerr );
	if( *nerr != 0 )
	    goto L_8888;
	Nlndta[idfl] = *npts;
	*nlen = *npts;
	allamb( &cmmem, *npts, &cmdfm.ndxdta[idfl - 1][0], nerr );
	if( *nerr != 0 )
	    goto L_8888;


	if( (*iftype == *itime || *iftype == *ixy) || *iftype == *iunkn ){
	    if( *leven ){
		Ncomp[idfl] = 1;
	    }
	    else{
		Ncomp[idfl] = 2;
	    }
	}
	else if( *iftype == *ixyz ){
	    Ncomp[idfl] = 1;
	}
	else{
	    Ncomp[idfl] = 2;
	}

	if ( Ncomp[ idfl ] == 2 ) {
	    allamb( &cmmem, *npts, &cmdfm.ndxdta[idfl - 1][1], nerr );
	    if( *nerr != 0 )
		goto L_8888;
	}

	/* - Read first data component. */

	ncards = *npts/5;
	nremdr = *npts - 5*ncards;
	nlcmem = cmdfm.ndxdta[idfl - 1][0];
	*ndx1 = nlcmem;
	for( jj = 1; jj <= ncards; jj++ ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }
	    Sacmem = cmmem.sacmem[*ndx1]+(nlcmem-*ndx1);
	    for( jjj = nlcmem; jjj <= (nlcmem + 4); jjj++ ){
		kiptr = strtok ( jjj == nlcmem ? kiline : NULL , " " ) ;
		if ( !kiptr )
		    break ;
		*Sacmem++ = atof(kiptr);
	    }
	    nlcmem = nlcmem + 5;
	}
	if( nremdr > 0 ){
	    if(fgets(kiline,MCMSG,nun)==NULL){
		*nerr = 1319;
		goto L_8888;
	    }
	    Sacmem = cmmem.sacmem[*ndx1]+(nlcmem-*ndx1);
	    for( jjj = nlcmem; jjj <= (nlcmem + nremdr - 1); jjj++ ){
		kiptr = strtok ( jjj == nlcmem ? kiline : NULL , " " ) ;
		if ( !kiptr )
		    break ;
		*Sacmem++ = atof(kiptr);
	    }
	}

	/* - Read second data component if present. */

	if( Ncomp[idfl] == 2 ){
	    nlcmem = cmdfm.ndxdta[idfl - 1][1];
	    *ndx2 = nlcmem;
	    for( jj = 1; jj <= ncards; jj++ ){
		if(fgets(kiline,MCMSG,nun)==NULL){
		    *nerr = 1319;
		    goto L_8888;
		}
		Sacmem = cmmem.sacmem[*ndx2]+(nlcmem-*ndx2);
		for( jjj = nlcmem; jjj <= (nlcmem + 4); jjj++ ){
		    kiptr = strtok ( jjj == nlcmem ? kiline : NULL , " " ) ;
		    if ( !kiptr )
			break ;
		    *Sacmem++ = atof(kiptr);
		}
		nlcmem = nlcmem + 5;
	    }
	    if( nremdr > 0 ){
		if(fgets(kiline,MCMSG,nun)==NULL){
		    *nerr = 1319;
		    goto L_8888;
		}
		Sacmem = cmmem.sacmem[*ndx2]+(nlcmem-*ndx2);
		for( jjj = nlcmem; jjj <= (nlcmem + nremdr - 1); jjj++ ){
		    kiptr = strtok ( jjj == nlcmem ? kiline : NULL , " " ) ;
		    if ( !kiptr )
			break ;
		    *Sacmem++ = atof(kiptr);
		}
	    }
	}
	else{
	    *ndx2 = 0;
	}

	/* - Close file and return. */

	zcloses( &nun, nerr );

L_8888:

	return;

} /* end of function */

