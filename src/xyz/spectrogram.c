#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "complex.h"
#include "proto.h"
/*/cdoc 
 * /cdoc
 * /cdoc Name:     SPECTROGRAM
 * /cdoc 
 * /cdoc
 * /cdoc Summmary:  Calculate Spectrograms from SAC files.
 * /cdoc     
 * /cdoc 
 * /cdoc
 * /cdoc Usage/Calling sequence:
 * /cdoc 
 * /cdoc    ierr = SPECTROGRAM (window, sliceint, type, order,
 * /cdoc                        nfiles, filelength, delta, 
 * /cdoc                        specindex, specwidth, speclength)
 * /cdoc 
 * /cdoc
 * /cdoc Arguments:
 * /cdoc
 * /cdoc    window      =:    window size for FFT (int)
 * /cdoc    sliceint    =:    interval for each image slice inseconds (int)
 * /cdoc    type        =:    Type correlation ( `mlm' or 'pds' )
 * /cdoc    order       =:    Order of correlation function (int)
 * /cdoc    nfiles      =:    Number of files read in SAC needed to 
 * /cdoc                      produce spectrogram(int).
 * /codc    filelength  =:    Integer array containing data lengths for
 * /cdoc                      each file in SAC memory.
 * /cdoc    delta       =:    Sampling interval for all the above files(float).
 * /doc     specindex    :=   Sac memory index of spectrogram (int).
 * /doc     specwidth    :=   Width in points of spectrogram. (int).
 * /doc     speclength   :=   Length in points of spectrogram. (int).
 * /doc
 * /cdoc Error Returns:
 * /cdoc    ierr   =  0  no error
 * /cdoc    ierr   >  0  error
 * /cdoc 
 * /cdoc
 * /cdoc  Notes:
 * /cdoc
 * /cdoc   
 * /cdoc       By:    T.M.Quinn
 * /cdoc       On:    1/26/90
 * /cdoc 
 * /cdoc       Updates:
 * /cdoc 
 * /cdoc            By:
 * /cdoc            On:
 * /cdoc            Subj:
 * /cdoc 
 * /cdoc            By:
 * /cdoc            On:
 * /cdoc            Subj:
 * /cdoc 
 * /cdoc  */

#include <string.h>
#include "mach.h"
#include "mem.h"
#include "spectrogram.h"
#include "specdata.h"
int /*FUNCTION*/ spectrogram(window, sliceint, type, order, nfiles, 
	 filelength, delta, specindex, specwidth, speclength, sfft,
         cwinlength, lcnumber, cnumber, cwintype, scale)
double window, sliceint;
char *type;
int *order, nfiles, filelength[];
double delta;
int *specindex, *specwidth, *speclength, sfft;
float cwinlength;
int lcnumber, cnumber;
char *cwintype;
char *scale;
{
	char windowfunc[11];
	int buffersize, bufindex, done, err, err1, err2, idx, iorfft, 
	 length, lfft, nptswndw, pfile, spectrogram_v, windowovrl;
	float signals[MAXLFFT];
        float ridge_regress = .00001;

	int *const Filelength = &filelength[0] - 1;
	float *const Signals = &signals[0] - 1;
  

	/*     * Include Files: */
	/*     * Arguments: */
	/*     * Local Variables: */
	/*     * Externals: */
	/*     * Code Implementation: */
	spectrogram_v = 1;

        /* initialize err flag */
        err = 0;

	/*         Initialize internal counter */
	pfile = 0;

	/*         Initialize SPECDATA common block variables */
	filesinfo.ptrbuffer = MAXBUFSIZE;
	filesinfo.bufferfull = FALSE;
	filesinfo.ptrfiles = 0;
	filesinfo.ifile = 0;
	filesinfo.filesread = TRUE;
	filesinfo.nodata = FALSE;
	filesinfo.lbuff = 0;
	filesinfo.first = TRUE;

	/*         Set windowing function type */
	strcpy( windowfunc, "ra        " );

	/*          Calculate FFT size */
	if( calcfftsize( delta, sliceint, window, &iorfft, &lfft, &nptswndw, 
	 &buffersize, &windowovrl ) != 0 ){
	    fprintf( stdout, "Error calculating fft size(spectrogram).\n" );
	    goto L_8888;
	}
	else{

	    if( err != 0 ){
		fprintf( stdout, "Error releasing previously stored image(spectrogram).\n" );

	    }
	    else{
		/*          Get space for image */
		length = 0;
		for( idx = 1; idx <= nfiles; idx++ ){
		    length = length + Filelength[idx];
		}
		length = (IMGFFT/2)*length*delta/sliceint;
		allamb( &cmmem, (int)(1.5 * length), specindex, &err1 );

		/*           Get space for buffering data */
		allamb( &cmmem, MAXBUFSIZE, &bufindex, &err2 );

		if( !err1 && !err2 ) {
		    /* Loop until all data used */
		    while ( 1 ) {
			/* Zero out signals array */
			for( idx = 1; idx <= MAXLFFT; idx++ ){
			    Signals[idx] = 0.;
			}

			/* Get windowed data */
			done = getdata( nfiles, delta, nptswndw, windowovrl, 
			 windowfunc, buffersize, filelength, bufindex, signals );

			if( done != 0 ){
			    if( done == 1 ) {
				fprintf( stdout, "Error getting data(spectrogram).\n" );
				relamb(cmmem.sacmem,*specindex,&err);
			    }
			    break ;

			}
			else{
			    spcgrm( signals, lfft, iorfft, type, order, delta,
				    nptswndw, sfft, lcnumber, cnumber, cwinlength,
				    cwintype, scale, ridge_regress,  &err );
			    if( err != 0) goto L_8888;
                                        
			    /* Write to memory */
			    for( idx = 1; idx <= (IMGFFT/2); idx++ ){
				/* instability in spectral estimate */
				if( Signals[idx] <= 0.0 ) {
				    /* up the ridge regression factor   */
				    ridge_regress *= 10.0;
				    if(ridge_regress >  .100001){
					printf("Error:  Could not stabilize high res. spectrogram\n");
					goto L_8888;
				    }
				    printf("Instability detected in spectral \
					    estimator, increasing ridge regression \
					    factor to %2.5e.\n",ridge_regress);

				    /* reinitialize and start over */
				    pfile = 0;
				    filesinfo.ptrbuffer = MAXBUFSIZE;
				    filesinfo.bufferfull = FALSE;
				    filesinfo.ptrfiles = 0;
				    filesinfo.ifile = 0;
				    filesinfo.filesread = TRUE;
				    filesinfo.nodata = FALSE;
				    filesinfo.lbuff = 0;
				    filesinfo.first = TRUE;
				    continue ;
				} /* end if( Signals[idx] <= 0.0 ) */
				else{
				    *(cmmem.sacmem[*specindex]+idx-1+pfile) =
				      Signals[idx];
				}
			    } /* end for */
			    pfile = pfile + IMGFFT/2;
			} /* end else associated with if( done != 0 ) */

		    } /* end while ( 1 ) */

		    /* Print image dimensions for reference 
		       if all data was used */
		    if( done == 2 ){
			spectrogram_v = 0;
			*specwidth = IMGFFT/2;
			*speclength = pfile/ *specwidth;
		    }

		} /* end if ( !err1 && !err2 ) */
	    } /* end else associated with if( err != 0 ) */
	} /* end else associated with if ( calcfftsize ... ) */

	if(cmmem.sacmem[bufindex] != NULL) relamb( cmmem.sacmem, bufindex, &err );

L_8888:
	return( spectrogram_v );

} /* end of function */

