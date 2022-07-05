#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
#include "../../inc/dload.h"
#include "../../inc/extfunc.h"
#include "../../inc/cpf.h"

void ext_init(void);

void updatedfl(sac_files call_data, int update, int* nerr);


void zexecute(index, nerr) int index, *nerr;
{
	int i, jdfl, nlen, ndx1, ndx2, ireturn;
        sac_files call_data;
        sac_header **call_headers, *this_header;

        char buf[1001];       
        char **ext_argv;
        int  ext_argc;         

        float **ydata, **xdata;
        float *Ptsacmem;

        int update = IGNORE;
        int nfile_out;
        int dummy_init = FALSE;

	/*=====================================================================
	 * PURPOSE: To execute a dynamically loaded external command.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    index:  Index (reference) used to access this command. [i]
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:   Error return flag. Set to 0 if no error occurred. [i]
	 *=====================================================================
	 * MODULE/LEVEL:  co/5
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *=====================================================================
	 * MACHINE DEPENDENCIES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    900804:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  900804
	 *===================================================================== */
	*nerr = 0;

	if( index <= cmextcom.nfiles ){

          if( dummy_init )ext_init(); /* this routine should never be called.    */
                                      /* this is just here to cause the external */
                                      /* function header access routines to be   */
                                      /* loaded.                                 */
          /* allocate space for the headers. */
          if((call_headers = malloc(cmdfm.ndfl*sizeof(sac_header *))) == NULL){
            *nerr = 301;
            return;
	  }

         
          /* allocate pointer arrays for data */
          if((ydata = malloc(cmdfm.ndfl*sizeof(float *))) == NULL){
            free(call_headers);
            *nerr = 301;
            return;
	  } 

          if((xdata = malloc(cmdfm.ndfl*sizeof(float *))) == NULL){
            *nerr = 301;
            free(call_headers);
            free(ydata);
            return;
	  } 

          for( i=0; i<cmdfm.ndfl; i++ ){
            call_headers[i] = NULL;
            ydata[i]        = NULL;
            xdata[i]        = NULL;
	  }

          
          for( jdfl = 1; jdfl <= cmdfm.ndfl; jdfl++ ){
            getfil( jdfl, TRUE, &nlen, &ndx1, &ndx2, nerr);
            if(*nerr != 0 ) goto L_8888;

            if((this_header = malloc(sizeof(sac_header))) == NULL){
              *nerr = 301;
              goto L_8888;
	    }

            call_headers[jdfl-1] = this_header;

            /* copy the header values into the call data struct. */
            memcpy(this_header->ext_fhdr,cmhdr.fhdr,MFHDR*sizeof(float));
            memcpy(this_header->ext_nhdr,cmhdr.nhdr,MNHDR*sizeof(int));
            memcpy(this_header->ext_ihdr,cmhdr.ihdr,MIHDR*sizeof(int));
            memcpy(this_header->ext_lhdr,cmhdr.lhdr,MLHDR*sizeof(int));
            memcpy(this_header->ext_khdr,kmhdr.khdr,MKHDR*9);

            /* allocate memory for the ydata. */
            if((ydata[jdfl-1] = malloc(nlen*sizeof(float))) == NULL){
              *nerr = 301;
              goto L_8888;
	    }

            /* copy ydata to call data yarray. */
            Ptsacmem = cmmem.sacmem[ndx1];
            memcpy(ydata[jdfl-1],Ptsacmem,(nlen*sizeof(float)));
          
            /* if there is another data component (xdata), allocate memory */
            /* for it and copy it in.                                      */
            if( ndx2 != 1 ){ /* There is xdata */
              if((xdata[jdfl-1] = malloc(nlen*sizeof(float))) == NULL){
                *nerr = 301;
                goto L_8888;
	      }
              Ptsacmem = cmmem.sacmem[ndx2];
              memcpy(xdata[jdfl-1],Ptsacmem,(nlen*sizeof(float)));
	    }else{
            /* if there is no other data component, set the pointer to NULL. */
              xdata[jdfl-1] = NULL;
	    }
            
	  }
          
          call_data.nfiles      = cmdfm.ndfl;
          call_data.ext_yvalues = ydata;
          call_data.ext_xvalues = xdata;
          call_data.ext_hdrs    = call_headers;

          strcpy(buf, kmcpf.kinputline);
          tokenize(&ext_argv, &ext_argc, buf, nerr);

          /* Call external routine. */
          if((ireturn = (*(cmextcom.extfuncs[index-1]))(ext_argc, ext_argv, &call_data, &update)) != 0){
            *nerr = ireturn;
            goto L_8888;
	  }

          /* Replace, append or ignore the return data, according to update flag. */
          if( update != IGNORE ){
            updatedfl(call_data, update, nerr);

	    /* If the user can write his/her own routine, we will trust him/her
	       to take care of evids. */
	    if( cmdfm.ltrust )
		cmdfm.nreadflag = HIGH ;
	    else
		cmdfm.nreadflag = LOW ;

	    cmdfm.nfilesFirst = 0 ;
	    cmdfm.lread = TRUE ;
	    sacToSeisMgr ( update == REPLACE , 0 , 1 , nerr ) ;
	    cmdfm.lread = FALSE ;
	  }

	}else{
                fprintf(stdout,"%s%2d\n", "ZEXECUTE: Error ",index );
	}

L_8888:

     /* free memory for call data. */
     call_headers = call_data.ext_hdrs;
     ydata        = call_data.ext_yvalues;
     xdata        = call_data.ext_xvalues;

     for( i=0; i<call_data.nfiles; i++){
       if( ydata[i]        != NULL )       free(ydata[i]);
       /* if not evenly spaced file release x storage. */
       if( !call_headers[i]->ext_lhdr[0] ) free(xdata[i]);
       if( call_headers[i] != NULL )       free(call_headers[i]);
     }

     free(call_headers);
     free(ydata);
     free(xdata);

	return;

} /* end of function */

