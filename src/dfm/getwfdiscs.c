#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/param.h>
#include "../../inc/complex.h"
#undef MIN  /* dont use the MIN defined in sys/param.h, use the one in proto.h */
#include "../../inc/proto.h"
#include <string.h>


void getwfdiscs(wflistout, nwfout, filesin, nfilesin, nerr)
char **wflistout;
int *nwfout;
char *filesin;
int nfilesin;
int *nerr;
{
    /* Find the wfdisc filenames in the input file list (filesin).       */
    /* Return them in wflistout aint with the number found (nwfout)     */
    /* Remove the wfdisc filenames from the input list.                  */


    char *tempsave[50];
    char *fname, *csave;
    char *filesin_save = NULL;
    int isave = 0;  
    int len = 0;
    int i, flength;
    char *finptr, *fptr;
    int lnoDot = FALSE ;

    *nwfout = 0;
    *nerr = 0;

    /* create working char array to hold files to be retained in the input */
    /* file list.  the wfdisc file names will be removed from the list.    */

    if((filesin_save = (char *)malloc(strlen(filesin)+1)) == NULL){
	*nerr = 0301;
	goto L_8888;
    }
    memset(filesin_save,' ',strlen(filesin));
    *(filesin_save+strlen(filesin)) = '\0';

    fname = strtok(filesin," \t\n");
    finptr = filesin_save+1;

    while ( fname != NULL ){
	csave = strstr(fname,".wfdisc");
	if( (csave != NULL ) && ((csave-fname+7) == strlen(fname))) {
	    /* temporarily save the wfdisc names found in tempsave */
	    strcpy((tempsave[isave++] = (char *)malloc(strlen(fname)+1)),fname);
	    *nwfout += 1;
	    len += strlen(fname)+1;
	}else{
	    /* if the user denoted the '.' in ".wfdisc" with a wildcard, */
	    /* it's a problem, alert the user.  maf 970425 */
	    csave = strstr ( fname, "wfdisc") ;
	    if ( ( csave != NULL ) && ( ( csave - fname + 6 ) == strlen ( fname ) ) &&
	       (*( csave - 1 ) == '*' || *(csave - 1 ) == '?' ) ) {
		setmsg ( "WARNING", 126 ) ;
		apcmsg ( "\n The token", 12 ) ;
		apcmsg ( fname , strlen ( fname ) + 1 ) ;
		apcmsg ( "is not treated as a .wfdisc file" , 33 ) ;
		outmsg () ;
		clrmsg () ;
		lnoDot = TRUE ;
	    }

            /* save the non wfdisc file names in a temporary area so they can */
            /* later be copied over the original input list.                  */
	    flength = strlen(fname);
	    strncpy(finptr,fname,flength);
	    finptr += flength + 1;
	}
	fname = strtok(NULL," \t\n");
    }

    if( *nwfout > 0 ){
	/* if any wfdisc filenames were found allocate the output list */
	/* and copy the names to it.                                   */
	if((*wflistout = (char *)malloc(len+2)) == NULL){
	    *nerr = 0301;
	    goto L_8888;    
    	    }else{
	    fptr = *wflistout;
	    memset(fptr,' ',len+2);
	    fptr++;
	    for(i=0; i<*nwfout; i++){
		flength = strlen(tempsave[i]);
		strncpy(fptr,tempsave[i],flength);
		fptr += flength + 1;
	    }
	    *(*wflistout+len+1) = '\0';
	}

    }else {
	/* if there were no wfdisc filenames found it is an error.      */
	if ( lnoDot )	/* if it's because wfdiscs had no dots, that's one problem. maf 970425 */
	    *nerr = 126 ;
	else {		/* else it's another problem */
	    *nerr = 120;
	    printf("The syntax of the readcss command has been changed.\n");
	    printf("The new syntax is:\n\n");
	    printf("readcss {MORE}{DIR name} wfdisclist filelist {css options}\n\n");
	    printf("The DIR option is no inter required, but the name of one \n");
	    printf("or more wfdisc files must be supplied on the input line.\n");
	    printf("If the DIR option is supplied, it will be used as a directory\n");
	    printf("to be searched for the file(s) in the wfdisclist.  Data files\n");
	    printf("will also be searched for first in the directory specified with\n");
	    printf("the DIR option, then in the path specified in the wfdisc.\n\n");
	}
    }
  
    strcpy(filesin,filesin_save);
  
L_8888:
    for (i=0; i<*nwfout; i++)free(tempsave[i]);
    if(filesin_save != NULL)free(filesin_save);

    return;

}
