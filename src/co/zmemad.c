/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zmemad_(pvar,pvarloc)				     D. Trimmer
 
 *** Purpose:	This function returns a pointer to the address of the
		specified variable in 16 bit increments.
 
 *** Inputs:	pvar	a pointer to a short integer
		pvarloc a pointer to a location which will contain the address
			of pvar
 
 *** Returns:	see pvarloc
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
		The way the address is returned is for compatibility with FORTRAN
 
 *** History:	07/24/84	Under development--D. Trimmer
		07/24/84	Tested--D. Trimmer
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 
#include "stdu.h"
 

void zmemad(pvar,pvarloc)
short *pvar;
ADDRESS_TYPE *pvarloc;		/* ADDRESS_TYPE is defined in stdu.h as the
				   variable type of addresses */
{
	*pvarloc = (ADDRESS_TYPE) pvar;
	*pvarloc = *pvarloc/2;	/* return word address--not byte address */
	return;
}
 
