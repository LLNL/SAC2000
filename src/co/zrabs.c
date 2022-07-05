/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 *** Function:	zrabs_(pfd,array,pnwords,pswords,pnerr)

 *** Purpose:	Read from an unformatted data file.

 *** Inputs:	pfd  	A pointer to the negative of a "C" file descriptor.
		array	Array to receive data.
		pnwords Pointer to number of 32bit words to read.
		pswords Pointer to first 32bit word to read(first is 0).
		pnerr	Address for returning error code.

 *** Returns:	array	See Inputs
		pnerr	See Inputs

 *** Notes:	SAC carries around FORTRAN logical unit numbers, but it
		doesn't currently have hooks for "C" file descriptors.
		Therefore, the "C" descriptors are carried in the same
		variable as the logical unit numbers.  The file descriptors
		are negated to distinguish them from logical unit numbers.

 *** History:	
		970123:   Replaced block-at-a-time reading with one big
			  read.  DEV_BSIZE & BSIZE are no longer used. maf
                03/18/92  Made default DEV_BSIZE 512 if not defined. wct
                03/13/87  Really Tested--initialized ret to 0--B. Hickman
                12/16/85  Sorta Tested--D. Trimmer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/* #include <sys/param.h> */
#include <stdio.h>
#include <unistd.h>
#include "mach.h"
#include "msg.h"
void setmsg(char* ktype, int number);

void zrabs(pfd,array,pnwords,pswords,pnerr)
int *pfd;       /* pointer to a file descriptor */
char array[];   /* array for data storage */
int pnwords;   /* # 32 bit words to be read */
int *pswords;   /* first 32 bit word to be read (starts at 0) */
int *pnerr;     /* pointer to error code */
{
  int ret;

  *pnerr=0;
  ret=0;

  lseek(-(*pfd),(int) (*pswords * 4),SEEK_SET);

  ret = read ( -(*pfd) , array , pnwords * 4 ) ;

  if ( ret != pnwords*4 )
  {
    *pnerr=114;
    setmsg ( "ERROR" , *pnerr ) ;
  }

  return;
}

