/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zgetc_.c(array,str,pnumc)		D. Trimmer
 
 *** Purpose:	Gets a character string from an integer or real array.
 
 *** Inputs:	array	An real or integer array
		str	A character array
		pnumc	pointer to # of characters to copy to character array
		strlen  length of str--required by F77.
 
 *** Returns:	none
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
 *** History:	07/23/84   Under development--D. Trimmer
		07/23/84   Tested--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 

void zgetc(array,str,pnumc)
int array[];		/* array containing characters */
char str[];		/* sink character array */
int pnumc;		/* # characters to copy */
 
{
	char  *parray;		/* pointer into array */
	char  *pstr;		/* pointer into string */
	int  i;		/* index */
 
	parray = (char *) &array[0];	/* character pointer into array */
	pstr   = &str[0];		/* character pointer into str */
	for (i=0;i<pnumc;++i)
		*pstr++ = *parray++;	/* copy characters */
 
	return;
}
 
