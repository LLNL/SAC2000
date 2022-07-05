#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/dfm.h"
#include "../../inc/hdr.h"
#include "../../inc/mem.h"
void /*FUNCTION*/ makeuniq(filelist,lenfilelist,nfiles,nerr)
char *filelist;
int lenfilelist;
int nfiles;
int *nerr;
{
	int ic1, ic2, jdfl, jdfl_, ndx1, ndx2, nlen;
	void *_p0;
        char *filename = NULL;
        char *buf1     = NULL;
        char *buf2     = NULL;
        char *target, *listptr;
        char char1, char2;
        int lunique = FALSE;
        int lenfilename, bufcount, numchars;
        int counter;
        char *field, *temp;
        int lenlist;
	/*=====================================================================
	 * PURPOSE:  To make sure the all the names in a filelist are unique.
	 *         
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    NERR:    Error return flag.  Set to zero if no error occurs.
	 *         
	 *         
	 *=====================================================================
	 * MODULE/LEVEL:  DFM/4
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    MACH:
	 *    DFM:     NDFL
	 *    HDR:     IFTYPE, ITIME, IXY, LEVEN
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  GTOUTM
	 *===================================================================== */
	/* PROCEDURE: */

      *nerr = 0;

      if((filename = (char *)malloc(lenfilelist+1)) == NULL){
        *nerr = 301;
        goto L_8888;
      }

      if((buf1 = (char *)malloc(lenfilelist+1)) == NULL){
        *nerr = 301;
        goto L_8888;
      }

      if((buf2 = (char *)malloc(lenfilelist+1)) == NULL){
        *nerr = 301;
        goto L_8888;
      }

   /* get the actual length of the list */
      lenlist = indexb(filelist,lenfilelist);
      if( lenlist == lenfilelist ){
   /* this is an error because we need room enough for one blank at the */
   /* end of the list for lnumcl to work properly. */
        *nerr = 901;
        goto L_8888;
      }

   /* increment the length so we copy one blank past the last entry */
      lenlist++;

      strncpy(buf1,filelist,lenlist);
      buf1[lenlist] = '\0';

      strcpy(buf2,buf1);

#ifdef DEBUG
   malloc_verify();
#endif
      counter = 0;

      while(!lunique){
        lunique = TRUE;
        counter++;

	/* - For each file in DFL: */

	for( jdfl = 1; jdfl <= nfiles; jdfl++ ){
          memset(filename,' ',lenfilelist);
          filename[lenfilelist] = '\0';

          lnumcl(buf1,lenfilelist, jdfl, &ic1, &ic2 );

#ifdef DEBUG
   malloc_verify();
#endif

          strncpy(filename,buf1+ic1-1,ic2-ic1+1);
          filename[ic2-ic1+1] = '\0';
          lenfilename = ic2-ic1+1;

#ifdef DEBUG
   malloc_verify();
#endif

          if((listptr = strstr(buf1,filename)) == NULL){
            *nerr = 901;
            goto L_8888;
	  }

          listptr += lenfilename;

          bufcount = listptr-buf1;
          strncpy(buf2,buf1,bufcount);

#ifdef DEBUG
   malloc_verify();
#endif

          char1 = '0';
          char2 = '1';

          /* look for identical filenames in the list */
          while((field = strstr(listptr,filename)) != NULL){
              numchars = field-listptr+lenfilename; 
              strncpy(buf2+bufcount,listptr,numchars);

#ifdef DEBUG
   malloc_verify();
#endif

              bufcount += numchars;
              listptr +=  numchars;
              if(field[lenfilename] == ' '){
                lunique = FALSE;
                buf2[bufcount] = char1;
                buf2[bufcount+1] = char2;
                bufcount += 2;
                if( char2 == '9' ) char2 = '0';
                else char2++;
                if( char2 == '0' ){
                  if( char1 == '9' ){
                    printf("too many output files-makeuniq\n");
                    *nerr = 901;
                    goto L_8888;
		  }else{
                    char1++;
	          } /* end if */
	        } /* end if */
	      }
	  } /* end while loop */
         
          strcpy(buf2+bufcount,listptr);

#ifdef DEBUG
   malloc_verify();
#endif

          temp = buf1;
          buf1 = buf2;
          buf2 = temp;
        } /* end for jdfl = .... */
      } /* end while !lunique */


L_8888:
        
        strcpy(filelist,buf1);

#ifdef DEBUG
   malloc_verify();
#endif

        if(filename != NULL) free(filename);
        if(buf1     != NULL) free(buf1);
        if(buf2     != NULL) free(buf2);
   
	return;

	/*=====================================================================
	 * MODIFICATION HISTORY:
	 *
	 *
	 *=====================================================================
	 * DOCUMENTED/REVIEWED: 
	 *===================================================================== */

} /* end of function */

