
/*
**	MatSeis 1.6
**	Mark Harris, mharris@sandia.gov
**	Copyright (c) 1996-2001 Sandia National Laboratories. All rights reserved.
*/

void byteswap ( void *x, int n );


/*******************************************************************************
** IS_LITTLE_ENDIAN, IS_BIG_ENDIAN
**
** Test if this is a little endian (PC) or big endian (UNIX,MAC) machine.
**
*/
static const int endian = 0x12345678;
#define IS_LITTLE_ENDIAN (*(unsigned char *)&endian == 0x78)
#define IS_BIG_ENDIAN (*(unsigned char *)&endian == 0x12)


/*******************************************************************************
** BYTESWAP
**
** Swap all bytes of input.
**
*/
void byteswap ( void *x, int n )
{
  int i;
  unsigned char *px;
  unsigned char tmp;
  
  px = ( unsigned char * ) x;
  
  for ( i=0 ; i<n/2 ; ++i )
  {
    tmp = px[i];
    px[i] = px[n-1-i];
    px[n-1-i] = tmp;
  }
}

