/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	getline(pfd,pch,maxlen)				D. Trimmer
 
 *** Purpose:	gets a line from the specified file descripter, and appends
		a \0 (null) if necessary.
 
 *** Inputs:	pfd	a pointer to a file descripter
		pch	a pointer to a character array
		maxlen	a short integer containing the maximum string length
			including termination character (\0)
 
 *** Returns:	A short integer:
		>=0	number of characters read (not including string
			termination character)
		<0	number of characters read +1 (not including string
			termination character) and EOF encountered
 
 *** History:	07/24/84	Under development--D. Trimmer
		07/24/84	Tested--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 
#include <stdio.h>
 
int getlineSAC(FILE* pfd, char* pch, short maxlen)
{
	char *pchsave;	/* save string pointer */
	int   i;	/* index and number of characters read */
	short ichar;	/* used to read characters--is integer so EOF (usually
			   a -1 can be read) */
 
	pchsave = pch;
 
	for (i=0;i<(maxlen-1) && (ichar=getc(pfd))!='\0' && ichar!='\n' && ichar!=EOF;++i) {
		*(pchsave++) = (char) ichar;
	}	
 
	if (ichar==EOF) {
		i = ++i;
		i = -i;
	} 
	*(pchsave) = '\0';
 
	return(i);
}
 
