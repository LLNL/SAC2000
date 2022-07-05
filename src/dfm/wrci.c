#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "dfm.h"
#include "hdr.h"
#include "mem.h"

void znfiles(FILE** nfu, char *kname, int kname_s, char *ktype, int ktype_s, int* nerr);

void /*FUNCTION*/ wrci(idfl, kname, kname_s, kfmt, nerr)
int idfl;
char *kname;   int kname_s;
char *kfmt;
int *nerr;
{
	int ilhdr[MLHDR], jdx, jj, jjj, ncards, nderr, ndx1, 
	 ndx2, nlcmem, nlen, nremdr;
        FILE *nun;

        float *Sacmem;

	int *const Ilhdr = &ilhdr[0] - 1;


	/*=====================================================================
	 * PURPOSE:  To write a SAC "card image" data file.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/3
	 *=====================================================================
	 * GLOBAL INPUT:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *=====================================================================
	 * GLOBAL COUPLING:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * KNOWN BUGS:
	 * - LDTA argument currently being ignored.
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    830607:  Fixed bug causing extra data point to be written.
	 *    810728:  Added argument specifying data format.
	 *    800109:  Original version.
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Create file. */

	zdest( kname,kname_s, &nderr );
	znfiles( &nun, kname,kname_s, "TEXT",5, nerr );
	if( *nerr != 0 )
	    goto L_8888;

	/* - Get file from memory manager. */

	getfil( idfl, TRUE, &nlen, &ndx1, &ndx2, nerr );

	/* - Write header. */

	jdx = 1;
	for( jj = 1; jj <= (MFHDR/5); jj++ ){
	    for( jjj = jdx; jjj <= (jdx + 4); jjj++ ){
                        fprintf(nun,kfmt,Fhdr[jjj]);
	    }
	    fprintf(nun,"\n");
	    jdx = jdx + 5;
	}

	jdx = 1;
	for( jj = 1; jj <= (MNHDR/5); jj++ ){
	    for( jjj = jdx; jjj <= (jdx + 4); jjj++ ){
		fprintf(nun,"%10d",Nhdr[jjj]);
	    }
	    fprintf(nun,"\n");
	    jdx = jdx + 5;
	}

	jdx = 1;
	for( jj = 1; jj <= (MIHDR/5); jj++ ){
	    for( jjj = jdx; jjj <= (jdx + 4); jjj++ ){
		fprintf(nun,"%10d",Ihdr[jjj]);
	    }
	    fprintf(nun,"\n");
	    jdx = jdx + 5;
	}

	jdx = 1;
	for( jj = 1; jj <= (MLHDR/5); jj++ ){
	    for( jjj = 1; jjj <= 5; jjj++ ){
		if( Lhdr[jdx] ){
		    Ilhdr[jjj] = 1;
		}
		else{
		    Ilhdr[jjj] = 0;
		}
		jdx = jdx + 1;
	    }
	    for( jjj = 1; jjj <= 5; jjj++ ){
		fprintf(nun,"%10d",Ilhdr[jjj]);
	    }
	    fprintf(nun,"\n");
	}

	/* write out the character header values */
        fprintf(nun,"%8s",kmhdr.khdr[0]);
        fprintf(nun,"%17s\n",kmhdr.khdr[1]);

	for( jj = 4; jj <= MKHDR; jj += 3 ){
	    for( jjj = jj; jjj <= (jj + 2); jjj++ ){
		fprintf(nun,"%8s", kmhdr.khdr[jjj - 1] );
	    }
	    fprintf( nun, "\n" );
	}

	/* - Write first data component. */

	nlcmem = ndx1;
	ncards = *npts/5;
	nremdr = *npts - 5*ncards;
        Sacmem = cmmem.sacmem[ndx1];
	for( jj = 1; jj <= ncards; jj++ ){
	    for( jjj = nlcmem; jjj <= (nlcmem + 4); jjj++ ){
		fprintf(nun,kfmt,*(Sacmem++));
	    }
	    fprintf( nun, "\n" );
	    nlcmem = nlcmem + 5;
	}
	if( nremdr > 0 ){
	    Sacmem = cmmem.sacmem[ndx1]+(nlcmem-ndx1);
	    for( jjj = nlcmem; jjj <= (nlcmem + nremdr - 1); jjj++ ){
		fprintf(nun,kfmt,*(Sacmem++));
	    }
	    fprintf( nun, "\n" );
	    nlcmem = nlcmem + nremdr;
	}

	/* - Write second data component if present. */

	if( Ncomp[idfl] == 2 ){
	    nlcmem = ndx2;
	    Sacmem = cmmem.sacmem[ndx2];
	    for( jj = 1; jj <= ncards; jj++ ){
		for( jjj = nlcmem; jjj <= (nlcmem + 4); jjj++ ){
		    fprintf(nun,kfmt,*(Sacmem++));
		}
		fprintf( nun, "\n" );
		nlcmem = nlcmem + 5;
	    }
	    if( nremdr > 0 ){
		Sacmem = cmmem.sacmem[ndx2]+(nlcmem-ndx2);
		for( jjj = nlcmem; jjj <= (nlcmem + nremdr - 1); jjj++ ){
		    fprintf(nun,kfmt,*(Sacmem++));
		}
		fprintf( nun, "\n" );
		nlcmem = nlcmem + nremdr;
	    }
	}

	/* - Close file and return. */

	zcloses( &nun, nerr );

L_8888:
	return;

} /* end of function */

