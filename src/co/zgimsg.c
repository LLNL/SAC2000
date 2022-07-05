/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zgimsg_(argc,argv,mess,messlen)				D. Trimmer
 
 *** Purpose:	Gets run string text
 
 *** Inputs:	mess	a pointer to a character array to receive the run
			string text.
		messlen	The length of the mess array.  This parameter is
			generated automatically by F77 on UNIX VAX.
 
 *** Returns:	none
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
		This function expects the number of parameters and an array
		of pointers to the parameters(strings) to be stored in the
		external variables xargc and xargv, respectively.
 
 *** History:	08/01/84	Under development--D. Trimmer
		08/01/84	Tested--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/

#define DOINITS 
#include "stdu.h"
#undef DOINITS 

void zgimsg(argc,argv,mess,messlen)
int argc;
char **argv;
char mess[];
int messlen;
 
{


	int i;			/* index for counting parameters */
	int j;			/* index for parameter strings */
	int k;			/* character counter */
	char *pc;		/* character pointer */
 
 
	k=0;					/* length counter */
	pc = *(++argv);			        /* skip program name */


	for (i=1;i<argc;++i,pc= *(++argv))	/* for each input parameter */
		{
		for (j=0;*pc!='\0' && k<messlen;++j,++k)
			mess[k] = *(pc++);	/* copy parameters */
		if (k == messlen)
			{			/* run string too long */
			printf ("warning:  command string too long.\n");
			break;
			}
		mess[k++] = ' ';		/* parameter delimiter */
		}
/*	mess[k-1] = '\0';			 terminate string */
 
	return;
}
 
 
