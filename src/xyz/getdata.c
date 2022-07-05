#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
/*/cdoc 
 * /cdoc
 * /cdoc Name:     GETDATA
 * /cdoc 
 * /cdoc
 * /cdoc Summmary:    Get data for distribution calculation
 * /cdoc
 * /cdoc 
 * /cdoc
 * /cdoc Usage/Calling sequence:
 * /cdoc 
 * /cdoc    ierr = GETDATA ( nfiles,delta,windwsize,
 * /cdoc                     windwovrl,windwfunc,buffersize,filelength,
 * /cdoc                     bufindex,signals )
 * /cdoc
 * /cdoc Arguments:
 * /cdoc
 * /cdoc    nfiles      =:    length of filelist(integer)
 * /cdoc    delta       =:    time between data in files(real).
 * /cdoc    windwsize   =:    size of window for FFT calc, integer
 * /cdoc    windwovrl   =:    window to window data overlap, integer
 * /cdoc    windwfunc   =:    function to apply to window, char*8
 * /cdoc    buffersize  =:    buffersize
 * /cdoc    filelength  =:    length of each file(integer)
 * /cdoc    bufindex    =:    SACMEM index for buffer
 * /cdoc    signals      :=   real array containing data for each station
 * /cdoc                      length, (<=lfft)
 * /cdoc
 * /cdoc 
 * /cdoc  Returns:
 * /cdoc 
 * /cdoc      GETDATA = 0 no errors but more data, continue
 * /cdoc      GETDATA = 1 error
 * /cdoc      GETDATA = 2 no error and no more data
 * /cdoc
 * /cdoc  Notes:
 * /cdoc
 * /cdoc   
 * /cdoc       By:    T.M.Quinn
 * /cdoc       On:    1/16/90
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
 * /cdoc
 * /cdoc
 * /cdoc
 * /cdoc  bufferfull     Is the input buffer full ?
 * /cdoc  buffersize     Size of input buffer, calculated in calcfftsize
 * /cdoc  filesread      Has the current file been read completely ?
 * /cdoc  ifile          How many files have been opened
 * /cdoc  lbuff          Ending point of data in buffer
 * /cdoc  ptrbuffer      Pointer in buffer to next data to parse out 
 * /cdoc  ptrfiles       Pointer in file where new data exists
 * /codc  nodata         Is there any data left in files not yet used ?
 * /cdoc
 * /cdoc  */


#include "../../inc/mach.h"
#include "../../inc/spectrogram.h"
#include "../../inc/specdata.h"
#include "../../inc/mem.h"
int /*FUNCTION*/ getdata(nfiles, delta, windwsize, windwovrl, windwfunc, 
	 buffersize, filelength, bufindex, signals)
int nfiles;
double delta;
int windwsize, windwovrl;
char *windwfunc;
int buffersize, filelength[], bufindex;
float signals[];
{
	int err, error, getdata_v, i, i_, index, j, j_, lread;
	float dum1, dum2;

	int *const Filelength = &filelength[0] - 1;
	float *const Signals = &signals[0] - 1;


	/*    * Include files: */
	/*     * Local Variables: */
	/*     * Externals: */
	/*     * Code Implementation: */
	getdata_v = 1;

	/*          Initializations */
	error = 0;
	for( i = 1; i <= MAXLFFT; i++ ){
		i_ = i - 1;
		Signals[i] = 0.;
		}

	if( error != 0 ){
		}
	else{

		/*          Check if all data used or last window of data not a full
		 *                window's length */
		if( (filesinfo.nodata) && (filesinfo.lbuff - filesinfo.ptrbuffer < 
		 windwsize) ){
			getdata_v = 2;

			}
		else{

			/*          Check if enough data left in buffer for a window's worth
			 *          If not need to read in more data from files */
			if( filesinfo.lbuff - filesinfo.ptrbuffer >= windwsize ){

				}
			else{

				/*          Move data at end of buffer to front to provide continuous windowing if not first time through */
				if( !filesinfo.first ){
					for( i = 1; i <= windwovrl; i++ ){
						i_ = i - 1;
						*(cmmem.sacmem[bufindex]-1+i) =
                                                      *(cmmem.sacmem[bufindex]-1+buffersize-windwovrl+i);
						}
					filesinfo.lbuff = windwovrl;
					}

				/*          Reset buffer pointer */
				filesinfo.ptrbuffer = 0;
				filesinfo.bufferfull = FALSE;

L_1:
				;
				if( (!(filesinfo.bufferfull) && (!(filesinfo.nodata)
				 )) && (error == 0) ){

					/*          If file completely read... */
					if( filesinfo.filesread ){
						/*          open a new file */
						filesinfo.ifile = filesinfo.ifile + 1;
						filesinfo.filesread = FALSE;
						}

					if( error == 0 ){

						/*          Calculate how much data to read from each station file */
						if( (Filelength[filesinfo.ifile] - filesinfo.ptrfiles) > 
						 (buffersize - filesinfo.lbuff) ){
							lread = buffersize - filesinfo.lbuff;
							filesinfo.bufferfull = TRUE;
							}
						else if( (Filelength[filesinfo.ifile] - filesinfo.ptrfiles) == 
						 (buffersize - filesinfo.lbuff) ){
							lread = buffersize - filesinfo.lbuff;
							filesinfo.bufferfull = TRUE;
							filesinfo.filesread = TRUE;
							}
						else{
							lread = Filelength[filesinfo.ifile] - 
							 filesinfo.ptrfiles;
							filesinfo.filesread = TRUE;
							}

						/*          Get data from SAC file */
						getfil( filesinfo.ifile, TRUE, (int*)&dum1, 
						 &index, (int*)&dum2, &err );
						if( err != 0 ){
							error = 1;
							}
						else{
							for( i = 1; i <= lread; i++ ){
								i_ = i - 1;
								*(cmmem.sacmem[bufindex]+i-1+filesinfo.lbuff) =
                                                                       *(cmmem.sacmem[index]+i-1+filesinfo.ptrfiles);
								}
							putfil( filesinfo.ifile, &err );
							if( err != 0 )
								error = 1;
							}

						/*          Set lbuff - length of new data in buffer */
						filesinfo.lbuff = filesinfo.lbuff + lread;

						filesinfo.ptrfiles = filesinfo.ptrfiles + 
						 lread;

						if( filesinfo.filesread ){
							/*          Reset files pointer */
							filesinfo.ptrfiles = 0;
							if( filesinfo.ifile == nfiles ){
								filesinfo.nodata = TRUE;
								}
							}
						}
					goto L_1;
					}

				}

			if( error != 0 ){
				fprintf( stdout, "Error getting data from file                (getdata).\n" );
				}
			else{


				/*          Load data to return */
				for( j = 1; j <= windwsize; j++ ){
					j_ = j - 1;
					Signals[j] = *(cmmem.sacmem[bufindex]+filesinfo.ptrbuffer+j-1);
					}

				/*          Set buffer pointer to last data passed */
				filesinfo.ptrbuffer = filesinfo.ptrbuffer + windwsize - 
				 windwovrl;

				/*          Apply any window weighting function to data */
/*				if( zwindow( (float(*)[4096])signals, windwfunc, windwsize, 
				 1, (float(*)[4096])signals ) != 0 ){
					fprintf( stdout, "Error applying window weighting function(getdata).\n" );

					}
				else{
*/
					filesinfo.first = FALSE;
					getdata_v = 0;
/*				      }      */
				}
			}
		}


	return( getdata_v );
} /* end of function */



