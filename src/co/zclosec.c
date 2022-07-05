/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 *** Function:	zclosec_(pfd)

 *** Purpose:	Close a "C" file descriptor (see zopenc_)

 *** Inputs:	pfd  A pointer to the negative of a "C" file descriptor.

 *** Notes:	SAC carries around FORTRAN logical unit numbers, but it
		doesn't currently have hooks for "C" file descriptors.
		Therefore, the "C" descriptors are carried in the same
		variable as the logical unit numbers.  The file descriptors
		are negated to distinguish them from logical unit numbers.

 *** History:	12/16/85  Tested--D. Trimmer
                01/15/88  Changed name from zclosc to zclosec--J. Tull
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#include <unistd.h>


void zclosec(pfd)
int *pfd;  	/* negative of a file descriptor */
{
  close(-(*pfd));
  return;
}

