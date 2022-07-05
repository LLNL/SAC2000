/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 *** Function:	zwabs_(pfd,array,pnwords,pswords,pnerr)

 *** Purpose:	Write to an unformatted data file.

 *** Inputs:	pfd  	A pointer to the negative of a "C" file descriptor.
		array	Array containing data.
		pnwords Pointer to number of 32bit words to write.
		pswords Pointer to first 32bit word to write(first is 0).
		pnerr	Address for returning error code.

 *** Returns:	pnerr	See Inputs

 *** Notes:	SAC carries around FORTRAN logical unit numbers, but it
		doesn't currently have hooks for "C" file descriptors.
		Therefore, the "C" descriptors are carried in the same
		variable as the logical unit numbers.  The file descriptors
		are negated to distinguish them from logical unit numbers.

 *** History:	
                03/18/92  Added default DEV_BSIZE. wct
                03/16/87  Really Tested--initialized ret to 0--B.Hickman                       12/16/85  Sorta Tested--D. Trimmer
                

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#include <stdio.h>
#include <unistd.h>
#include <sys/param.h>
#include "mach.h"
#include "msg.h"

#ifndef DEV_BSIZE
#define DEV_BSIZE 512
#endif

#ifdef BSD4_2
#define BSIZE DEV_BSIZE
#endif

#ifdef STELLAR
#define BSIZE DEV_BSIZE
#endif

#ifdef IBM
#define BSIZE DEV_BSIZE
#endif


#define BSIZE DEV_BSIZE

void setmsg(char* ktype, int number);

void zwabs(pfd,array,pnwords,pswords,pnerr)
int *pfd;       /* pointer to a file descriptor */
char array[];   /* array for data data storage */
int pnwords;   /* # 32 bit words to be written */
int *pswords;   /* first 32 bit word to be written (starts at 0) */
int *pnerr;     /* pointer to error code */
{
  int count;
  int ret;
  int partial;
  int skip;

  *pnerr=0;
  count=0;
  ret=0;

  lseek(-(*pfd),(int) (*pswords * 4),0);

  if((skip=(*pswords*4)%BSIZE))  /* write partial block if necessary */
    {
    partial = (BSIZE-skip < pnwords*4) ? BSIZE-skip : pnwords*4;
    ret=write(-(*pfd),&array[count],partial);
    if(ret < 0)
	{
	*pnerr=115;
	setmsg ( "ERROR" , *pnerr ) ;
	return;
	}
      count += ret;
      }

					/* write full blocks if necessary */
  while((pnwords*4-count)>=BSIZE && (ret=write(-(*pfd),&array[count],BSIZE))>=
    BSIZE)
      count += ret;

  if(ret < 0)
    {
    *pnerr=115;
    setmsg ( "ERROR" , *pnerr ) ;
    return;
    }

  if(pnwords*4 > count)	/* write last partial block if necessary */
    {
    ret=write(-(*pfd),&array[count],pnwords*4 - count);
    count += ret;
    }

  if(ret <0  ||  pnwords*4 != count)
    {
    *pnerr=115;
    setmsg ( "ERROR" , *pnerr ) ;
    }

  return;
}

