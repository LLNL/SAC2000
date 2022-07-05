/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	getfline(pfd,pch,maxlen)			D. Trimmer
 
 *** Purpose:	gets a line from the specified file descripter, and pads with
		blanks.  No EOS is added.
		
 *** Inputs:	pfd	a pointer to a file descripter
		pch	a pointer to a character array
		maxlen	a short integer containing the maximum string length
	
 *** Returns:	A short integer:
		>=0	number of characters read
		<0	number of characters read +1 and EOF encountered
 
 *** History:	08/17/84	Under development--D. Trimmer
		08/17/84	Tested--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 
#include <stdio.h>
 
int getfline(FILE* pfd, char* pch, short maxlen)
{
	char *pchsave;	/* save string pointer */
	int   i;	/* index and number of characters read */
	int   ret;	/* save return value */
	short ichar;	/* used to read characters--is integer so EOF (usually
			   a -1 can be read) */
 
	pchsave = pch;
 
	for (i=0;i<(maxlen) && (ichar=getc(pfd))!='\0' && ichar!='\n' && ichar!=EOF;++i) {
		*(pchsave++) = (char) ichar;	
	}
 
	ret = i;
	if (ichar==EOF) {
		ret = ++ret;
		ret = - (ret);
	}
 
	for (;i<(maxlen);++i) {
		*(pchsave++) = ' ';
	}

        pch[maxlen-1] = '\0';
 
	return(ret);
}
 
