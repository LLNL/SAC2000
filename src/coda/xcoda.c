#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/scm.h"
#include "../../inc/sam.h"
#include "coda.h"
int get_input(const char *inputfile,struct envelope *envelopes,int *nbands,struct g_params *global_params);
int get_picks(char *inputfile,struct envelope *envelopes, int nbands);
void calc_coda_amplitudes(struct envelope *envelopes,int nbands,float dist,float begin,char* evid,int lcalibrate);
void pickwindows(struct envelope *envelopes,int nbands,float C_begin,int* nerr);
void calc_moment_magnitude(struct envelope *envelopes,int nbands,struct g_params *global_params);
void calc_energy(struct envelope *envelopes,int nbands,char* evid,int lcalibrate,struct g_params *global_params);
int send_output(struct envelope *envelopes,int nbands,char* evid,int lcalibrate,struct g_params *global_params);
void plotspec(struct envelope *envelopes,int nbands,float C_begin,int* nerr);
int calc_envelopes(int nchannels,int ndata,int *file_pointers,struct envelope *envelopes,int* nbands,float begin,
                   float delta,int* npts,float dist,char* evid);


void /*FUNCTION*/ xcoda( index, nerr)

int index, *nerr;
{
	int lhorz, lnpin, lnpout;
	int ic1a, ic1b, ic2a, ic2b, jdfl, jdfl_, ndx1, ndx2, ndxx, 
	 ndxy, nlen, nlen1, nlen2, notused, ncpfn;
	int file_pointers[MAXCHANNELS], file_lengths[MAXCHANNELS];
	int file_name_pointers_a[MAXCHANNELS], file_name_pointers_b[MAXCHANNELS];
	void *_p0;
	int i;
	float C_begin, C_delta, C_dist;
        int npts, nbands;
	struct envelope envelopes[MAXBANDS];
        struct g_params global_params;
        int linteractive=0,namelength,lcalibrate=0;
        char evid[80] = "coda";
        char *p_evid;
	char paramfilename[100], pickfilename[100];

	/*=====================================================================
	 * PURPOSE:  To execute the action command CODA.
	 *           This command applies the Mayeda coda algorithm to data files in memory
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers: 2001, 2002, 2003, 2004, 2010.
	 *=====================================================================
	 * MODULE/LEVEL:  coda
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    mach:
	 *    hdr:
	 *=====================================================================
	 * GLOBAL OUTPUT:
	 *    
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    9908??:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* PARSING PHASE: */

	/* - Loop on each token in command: */
        fprintf(stderr, "Executing CODA command \n");
        sprintf(cmsam.knmfir, "test.input");
        sprintf(pickfilename, "");

L_1000:
	if( lcmore( nerr ) ){

	        if( lkchar( "PARAM#FILE$",12, 80 , paramfilename ,81,  &ncpfn ) )
		  { /* do nothing */ }

	        else if( lkchar( "PICK#FILE$",11, 80 , pickfilename ,81,  &ncpfn ) )
		  { /* do nothing */ }

	        else if( lkchar( "ID$",4, 80 , evid ,81,  &ncpfn ) )
		  { /* do nothing */ }

		else if( lklog( "INT#ERACTIVE$",14, &linteractive ) )
		  { /* do nothing */ }

		else if( lklog( "CAL#IBRATE$",12, &lcalibrate ) )
		  { /* do nothing */ }

		else{
			cfmt( "ILLEGAL OPTION:$",17 );
			cresp();

			}
		goto L_1000;

		}

	/* - The above loop is over when one of two conditions has been met:
	 *   (1) An error in parsing has occurred.  In this case NERR is > 0 .
	 *   (2) All the tokens in the command have been successfully parsed. */

	if( *nerr != 0 )
		goto L_8888;


	/* CHECKING PHASE: */

	/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0 )
		goto L_8888;

	/* - Make sure there is at least 1 data file. */

	if( cmdfm.ndfl < 1 ){
		*nerr = 1301;
		setmsg( "ERROR", *nerr );
		goto L_8888;
		}
	/* - Make sure there aren't too many files. */
	if( cmdfm.ndfl >= MAXCHANNELS ){
		*nerr = 1315;
		setmsg( "ERROR", *nerr );
		fprintf(stderr, "ERROR too many channels in file list: %d %d \n", cmdfm.ndfl, MAXCHANNELS);
		goto L_8888;
		}

	/* EXECUTION PHASE: */

	/* - Perform the requested function on each pair of files in DFL. */

	jdfl = 1;
	jdfl_ = jdfl - 1;

	/* -- Get the second file if there is one */
        for(i=0;i<cmdfm.ndfl;i++) {
	  //	  fprintf(stderr,"reading file %d \n",i+1);
	  getfil( i+1, TRUE, &file_lengths[i], &file_pointers[i], &notused, nerr );
	  if( *nerr != 0 )
	    goto L_8888;
	  //	  fprintf(stderr,"reading file name %d \n",i+1);
	  lnumcl( kmdfm.kdfl,MAXCHARS, i+1, &file_name_pointers_a[i], &file_name_pointers_b[i] );
	  /* -- Check to make sure all files have the same number of points. */          
	  if( i > 0 )
	    {
	      if(nlen != file_lengths[i])
		{
		  *nerr = 2010;
		  setmsg( "ERROR", *nerr );
		  //		  fprintf(stderr, "ERROR file lengths not equal \n");
		  goto L_8888;
		}
	    } else {
	      nlen = file_lengths[0];
	      //	      	  fprintf(stderr,"file lengths OK: %d \n",nlen);
	      C_begin = *b;
	      C_delta = *delta;
	      C_dist = *dist;
	    }
	}

        namelength = strcspn(evid," ");
        evid[namelength]= '\0';
        p_evid = &evid[0];

	/* read input parameter file */
	//		  fprintf(stderr,"reading input file %s \n",paramfilename);
	*nerr = get_input(paramfilename, envelopes,&nbands,&global_params);
        if(*nerr != 0) {
	  fprintf(stderr, "Error reading input file! %s\n", paramfilename);
	  goto L_8888;
	}
	//  fprintf(stderr,"read input file OK %d \n",*nerr);

	/* read input pick file */
	if(strlen(pickfilename) > 0) {
	  //	  fprintf(stderr,"reading input file %s \n",pickfilename);

	  *nerr = get_picks(pickfilename, envelopes ,nbands);
	  if(*nerr != 0) {
	  fprintf(stderr, "Error reading input file! %s\n", pickfilename);
	  goto L_8888;
	  }
	  //  fprintf(stderr,"read input file OK %d \n",*nerr);
	}

        /* need to get begin and delta from somewhere
	 *b = begin, *delta, *dist are defined in hdr.h
	*/
 
        if(C_dist > 0.0) {
	  fprintf(stderr, "begin=%f delta=%f dist=%f ID=%s\n",C_begin,C_delta,C_dist,p_evid);
	} else {
	  fprintf(stderr, "dist not defined in header! f\n");
	  goto L_8888;          
	}
	calc_envelopes(cmdfm.ndfl,nlen,file_pointers,envelopes,&nbands,C_begin,C_delta,&npts,C_dist,evid);

	calc_coda_amplitudes(envelopes,nbands,C_dist,C_begin,evid,lcalibrate);

        if(linteractive) {
	  pickwindows(envelopes,nbands,C_begin,nerr);
	  calc_coda_amplitudes(envelopes,nbands,C_dist,C_begin,evid,lcalibrate);
	  pickwindows(envelopes,nbands,C_begin,nerr);
	}

	calc_moment_magnitude(envelopes,nbands,&global_params);
	calc_energy(envelopes,nbands,evid,lcalibrate,&global_params);
	send_output(envelopes,nbands,evid,lcalibrate,&global_params);
	plotspec(envelopes,nbands,C_begin,nerr);
	/* - Calculate and set new range of dependent variable. */

	setrng();

L_8888:
	return;

} /* end of function */

