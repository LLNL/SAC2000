/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:  izshft_(pint,pnumshft)                     D. Trimmer
 
 *** Purpose:	Shifts specified integer specified number of bits
		right with wrap.  pnumshft>0 implies left shift, pnumshft<0
		implies right shift.
 
 *** Inputs:	pint       a pointer to a short integer to be shifted
		pnumshft   a pointer to a short integer giving number of shifts

 *** Returns:  	the shifted integer as short integer
 
 *** Notes:     This function is to be called by a FORTRAN routine. The
		'_' is appended to the name for compatibility with FORTRAN.
 
 *** History:	07/20/84   Under development--D. Trimmer
		07/23/84   Tested and debugged
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 

short int izshft(pint,pnumshft)
short int *pint;	/*  pointer to integer to be shifted  */
short int *pnumshft;	/*  pointer to number of bits to shift  */
 
{
 
	unsigned   wrap;	/*  save shifted bits for wrapping  */
	short int nbits;	/*  length of shifted number in bits */
	short int shift;	/*  actual number of bits to be shifted */
 
	nbits = sizeof(*pint) * 8;
	shift = *pnumshft;
	while (shift < 0)	/* convert right shift into equiv. left shift*/
		shift += nbits;
	shift = shift % nbits;	/* # shifts must be less than integer size */
 
	wrap = ((~((short) 0)) << (nbits-shift))   &  *pint;	/* save wrap */
	wrap = (wrap>>(nbits-shift)) & ((1<<shift)-1);	/* move & mask wrap  */
	return ((short)(wrap |  (*pint << shift)));
}
 
