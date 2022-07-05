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
void /*FUNCTION*/ wrsdd(idfl, kname, kname_s, ldta, nerr)
int idfl;
char *kname;   int kname_s;
int ldta;
int *nerr;
{
	int idx, idd, ideg, ifrac, ihh, ijday, imm, imsec, 
	 iss, itm, jcomp, kundef_len, ncerr, nlcdsk, nlcmem, nptwr, 
	 nun;
	float frac;
	void zgetc(), zwabs();
        char *strtemp;
        int *Isacmem;
        float *Sacmem;

	kschan[12]='\0';
	kschdr[80]='\0';
	ksclas[4]='\0';
	kscom[40]='\0';
	ksevnm[8]='\0';
	ksfrmt[8]='\0';
	ksstnm[8]='\0';

	/*=====================================================================
	 * PURPOSE:  To write a SDD data file from memory to disk.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    IDFL:    Data file list index number. [i]
	 *    KNAME:   Name of disk file to write. [c]
	 *    LDTA:    Set to .TRUE. if header and data are to be written. [l]
	 *             Set to .FALSE. if only header is to be written.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NCOMP, NLNDTA, NDXHDR, NDXDTA
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  ZGTFUN, ZDEST, ZNFILE, ZOPEN, ZWABS, ZCLOSE
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NUN:     Fortran file unit used to write file. [i]
	 *    NLNFIL:  Length of disk file to open or create. [i] {UNUSED}
	 *    NLCDSK:  Location in disk file to write to. [i]
	 *    NLCMEM:  Location in SACMEM array to write from. [i]
	 *    NPTWR:   Number of words to write. [i]
	 *    NCERR:   Error flag returned by ZCLOSE. [i] {UNUSED}
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    910826:  Changed nint(x) to int(x + .5) to improve portability to
	 *             DEC 5000 workstation per Gyu-sang Jang @ UC Davis.
	 *    870730:  Added logic to check file permissions before writing.
	 *    850731:  Changes due to new memory manager.
	 *    840118:  Deleted call to ZTRUNC.
	 *    800510:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  870730
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - If header and data is to be written, a new file is created.
	 *   If header only is to be written, the old file is opened. */

	if( ldta ){
		znfile( &nun, kname,kname_s, "DATA",5, nerr );
		if( *nerr != 0 )
			goto L_8888;
	}
	else{
		zopen_sac( &nun, kname,kname_s, "DATA",5, nerr );
		if( *nerr != 0 )
			goto L_8888;
	}

	/* - Copy header from working memory into header common. */

	nlcmem = Ndxhdr[idfl];
	copy( (int*)cmmem.sacmem[nlcmem], (int*)&Fhdr[1], MCMHDR );
	zgetc( cmmem.sacmem[nlcmem]+MCMHDR, kmhdr.khdr[0], (MCPW+1)* MKHDR );

	/* - Initialize fields in SDD header */

	fstrncpy( kschdr, 80, " ", 1);

	for( idx = 21; idx <= MWSHDR; idx++ ){
		Ishdr[idx] = 0;
	}

	/* - Convert header from SAC format to SDD format */
        kundef_len = strlen(kmhdr.kundef);

	if( memcmp(kevnm,kmhdr.kundef,kundef_len) != 0 )
		strscpy( ksevnm, kevnm, 8 );
	if( memcmp(kstnm,kmhdr.kundef,kundef_len) != 0 )
		strcpy( ksstnm, kstnm );
	if( memcmp(kcmpnm,kmhdr.kundef,kundef_len) != 0 ) {
                strtemp = malloc(9);
                strncpy(strtemp,kcmpnm,8);
                strtemp[8] = '\0';
		subscpy( kschan, 0, 7, 12, strtemp );
                free(strtemp);
	}
	if( memcmp(kinst,kmhdr.kundef,kundef_len) != 0 ) {
                strtemp = malloc(5);
                strncpy(strtemp,kinst,4);      
                strtemp[4] = '\0';
		subscpy( kschan, 8, 11, 12, strtemp );
                free(strtemp);
	}
	if( *delta != cmhdr.fundef )
		*isdelt = (int)( (1.0/ *delta)*100.0 + .5 );
	*isnpts = *npts;
	if( *stel != cmhdr.fundef )
		*issel = (int)( *stel*100.0 + .5 );
	if( *stdp != cmhdr.fundef )
		*issdep = (int)( *stdp*100.0 + .5 );

	/* - Pack date and time into one word */

	ijday = *nzjday;
	itm = *nzmsec + (((*nzhour*60 + *nzmin)*60) + *nzsec)*1000;
	if( *begin != cmhdr.fundef && *begin != 0.0 ){
		itm = itm + (int)( *begin*1000.0 + .5 );
		if( itm < 0 ){
			ijday = ijday - 1;
			itm = itm + 8640000;
			if( ijday < 0 ){
				*nerr = 1377;
				goto L_8888;
			}
		}
		imsec = itm%1000;
		itm = itm/1000;
		iss = itm%60;
		itm = itm/60;
		imm = itm%60;
		ihh = itm/60;
		*istime = imsec + ((ihh*100 + imm)*100 + iss)*1000;
	}
	else{
		*istime = *nzmsec + ((*nzhour*100 + *nzmin)*100 + *nzsec)*
		 1000;
	}
	kidate( *nzyear, ijday, &imm, &idd, nerr );
	*isdate = idd + (*nzyear*100 + imm)*100;

	/* - Convert lat/lon back from fraction to minutes/seconds */

	if( *stla != cmhdr.fundef ){
		ideg = *stla;
		frac = *stla - (float)( ideg );
		imm = frac*60.0;
		frac = frac*60.0 - (float)( imm );
		iss = frac*60.0;
		frac = frac*60.0 - (float)( iss );
		ifrac = (int)( frac*100.0 + .5 );
		*issla = ifrac + ((ideg*100 + imm)*100 + iss)*100;
	}

	if( *stlo != cmhdr.fundef ){
		ideg = *stlo;
		frac = *stlo - (float)( ideg );
		imm = frac*60.0;
		frac = frac*60.0 - (float)( imm );
		iss = frac*60.0;
		frac = frac*60.0 - (float)( iss );
		ifrac = (int)( frac*100.0 + .5 );
		*isslo = ifrac + ((ideg*100 + imm)*100 + iss)*100;
	}

	/* - Copy extra SDD information, if exists, to header */

	nlcmem = Nxsdd[idfl];
	if( nlcmem != 0 ){
                Isacmem = (int *)cmmem.sacmem[nlcmem];
		*isclas = *(Isacmem++);
		*isfrmt = *(Isacmem++);
		*iscalg = *(Isacmem++);
		nlcmem = nlcmem + 3;

		for( idx = 1; idx <= MSCOM; idx++ ){
			Iscom[idx] = *(Isacmem++);
		}

		for( idx = 1; idx <= MSREP; idx++ ){
			Isrep[idx] = *(Isacmem++);
		}
	}


	/* - Write the header to disk. */

	nlcdsk = 0;
	nptwr = MWSHDR;
	zwabs( &nun, &Ishdr[1], nptwr, &nlcdsk, nerr );

	/* - Write each data component, if requested. */

	if( ldta ){
		for( jcomp = 0; jcomp < Ncomp[idfl]; jcomp++ ){
			nlcdsk = nlcdsk + nptwr;
			nlcmem = cmdfm.ndxdta[idfl - 1][jcomp];
			nptwr = Nlndta[idfl];
                        Isacmem = (int *)cmmem.sacmem[nlcmem];
                        Sacmem = cmmem.sacmem[nlcmem];

			/*         Convert data to integers before writing */

			for( idx = 0; idx <= (nptwr - 1); idx++ ){
				*(Isacmem++) = *(Sacmem++)*100.0;
			}

			zwabs( &nun, cmmem.sacmem[nlcmem], nptwr, &nlcdsk, nerr );
		}
	}

	/* - Close disk file. */

L_8888:
	zclose( &nun, &ncerr );

	return;

} /* end of function */

