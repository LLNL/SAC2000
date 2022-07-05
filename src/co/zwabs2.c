/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 *** Function:	zwabs2(pfd,array,pnwords,pswords,pnerr)

 *** Purpose:	Write to an unformatted data file from array of 2 byte words.

 *** Inputs:	pfd  	A pointer to the negative of a "C" file descriptor.
		array	Array containing data in 2 byte words.
		pnwords Pointer to number of 16 bit words to write.
		pswords Pointer to first 16 bit word to skip (first is 0).
		pnerr	Address for returning error code.

 *** Returns:	pnerr	See Inputs

 *** Notes:	SAC carries around FORTRAN logical unit numbers, but it
		doesn't currently have hooks for "C" file descriptors.
		Therefore, the "C" descriptors are carried in the same
		variable as the logical unit numbers.  The file descriptors
		are negated to distinguish them from logical unit numbers.

 *** History:	
                created on 10/7/1999 by Mike Firpo who plagerized zwabs.c

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

void zwabs2(pfd,array,pnwords,pswords,pnerr)
int *pfd;       /* pointer to a file descriptor */
char array[];   /* array for data data storage */
int pnwords;   /* # 16 bit words to be written */
int *pswords;   /* first 16 bit word to be skipped (starts at 0) */
int *pnerr;     /* pointer to error code */
{
  int count;
  int ret;
  int partial;
  int skip;

  *pnerr=0;
  count=0;
  ret=0;

  lseek(-(*pfd),(int) (*pswords * 2),0);

  if((skip=(*pswords*2)%BSIZE))  /* write partial block if necessary */
    {
    partial = (BSIZE-skip < pnwords*2) ? BSIZE-skip : pnwords*2;
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
  while((pnwords*2-count)>=BSIZE && (ret=write(-(*pfd),&array[count],BSIZE))>=
    BSIZE)
      count += ret;

  if(ret < 0)
    {
    *pnerr=115;
    setmsg ( "ERROR" , *pnerr ) ;
    return;
    }

  if(pnwords*2 > count)	/* write last partial block if necessary */
    {
    ret=write(-(*pfd),&array[count],pnwords*2 - count);
    count += ret;
    }

  if(ret <0  ||  pnwords*2 != count)
    {
    *pnerr=115;
    setmsg ( "ERROR" , *pnerr ) ;
    }

  return;
}

