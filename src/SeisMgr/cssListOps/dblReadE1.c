#include <stdlib.h>
#include <stdio.h>

#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "../dbselect/dbDefaults.h"
#include "../smMemory/smMemory.h"
#include "dblGetDataSubs.h"
#include "cssArchitecture.h"

/*******************************************************************************
** Y = READ_E1 ( DATAFILE, BYTEOFFSET, NUM );
**
**
*/

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


#define	MAX_REC	200000



/********************************
** Compression mapping defines **
********************************/

/*
** 7 9-bit samples
**
** Bit map format: 0AAAAAAA|AABBBBBB|BBBCCCCC|CCCCDDDD
**                 DDDDDEEE|EEEEEEFF|FFFFFFFG|GGGGGGGG
**
*/

#define MAP7_9(x)	(((x)&0x80000000)==0x00000000)
#define MAP7_9_A(x)	((((x)&0x7fc00000)<<1)>>23)
#define MAP7_9_B(x)	((((x)&0x003fe000)<<10)>>23)
#define MAP7_9_C(x)	((((x)&0x00001ff0)<<19)>>23)
#define MAP7_9_D(x,y)	((int)((((x)&0x0000000f)<<28)|(((int)((y)&0xf8000000)>>4)&0x0fffffff))>>23)
#define MAP7_9_E(y)	((((y)&0x07fc0000)<<5)>>23)
#define MAP7_9_F(y)	((((y)&0x0003fe00)<<14)>>23)
#define MAP7_9_G(y)	((((y)&0x000001ff)<<23)>>23)

/*
** 3 10-bit samples
**
** Bit map format: 10AAAAAA|AAAABBBB|BBBBBBCC|CCCCCCCC
**
*/

#define MAP3_10(x)	(((x)&0xc0000000)==0x80000000)
#define MAP3_10_A(x)	((((x)&0x3ff00000)<<2)>>22)
#define MAP3_10_B(x)	((((x)&0x000ffc00)<<12)>>22)
#define MAP3_10_C(x)	((((x)&0x000003ff)<<22)>>22)

/*
** 4 7-bit samples
**
** Bit map format: 1100AAAA|AAABBBBB|BBCCCCCC|CDDDDDDD
**
*/

#define MAP4_7(x)	(((x)&0xf0000000)==0xc0000000)
#define MAP4_7_A(x)	((((x)&0x0fe00000)<<4)>>25)
#define MAP4_7_B(x)	((((x)&0x001fc000)<<11)>>25)
#define MAP4_7_C(x)	((((x)&0x00003f80)<<18)>>25)
#define MAP4_7_D(x)	((((x)&0x0000007f)<<25)>>25)

/*
** 5 12-bit samples
**
** Bit map format: 1101AAAA|AAAAAAAA|BBBBBBBB|BBBBCCCC
**                 CCCCCCCC|DDDDDDDD|DDDDEEEE|EEEEEEEE
**
*/

#define MAP5_12(x)	(((x)&0xf0000000)==0xd0000000)
#define MAP5_12_A(x)	((((x)&0x0fff0000)<<4)>>20)
#define MAP5_12_B(x)	((((x)&0x0000fff0)<<16)>>20)
#define MAP5_12_C(x,y)	(((int)((((x)&0x0000000f)<<28)|(((int)((y)&0xff000000)>>4)&0x0fffffff)))>>20)
#define MAP5_12_D(y)	((((y)&0x00fff000)<<8)>>20)
#define MAP5_12_E(y)	((((y)&0x00000fff)<<20)>>20)

/*
** 4 15-bit samples
**
** Bit map format: 1110AAAA|AAAAAAAA|AAABBBBB|BBBBBBBB
**                 BBCCCCCC|CCCCCCCC|CDDDDDDD|DDDDDDDD
**
*/

#define MAP4_15(x)	(((x)&0xf0000000)==0xe0000000)
#define MAP4_15_A(x)	((((x)&0x0fffe000)<<4)>>17)
#define MAP4_15_B(x,y)	(((int)((((x)&0x00001fff)<<19)|(((int)((y)&0xc0000000)>>13)&0x0007ffff)))>>17)
#define MAP4_15_C(y)	((((y)&0x3fff8000)<<2)>>17)
#define MAP4_15_D(y)	((((y)&0x00007fff)<<17)>>17)

/*
** 1 28-bit sample
**
** Bit map format: 1111AAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA
**
*/

#define MAP1_28(x)	(((x)&0xf0000000)==0xf0000000)
#define MAP1_28_A(x)	((((x)&0x0fffffff)<<4)>>4)


int demap ( int word[2], int out[7], int *num_out )
{
  /*************************************************
  ** Check for 7 9-bit sample mapping.
  */
  if ( MAP7_9(word[0]) )
  {
    out[0] = MAP7_9_A(word[0]);
    out[1] = MAP7_9_B(word[0]);
    out[2] = MAP7_9_C(word[0]);
    out[3] = MAP7_9_D(word[0],word[1]);
    out[4] = MAP7_9_E(word[1]);
    out[5] = MAP7_9_F(word[1]);
    out[6] = MAP7_9_G(word[1]);
    *num_out = 7;
    return ( 2 );
  }

  /*************************************************
  ** Check for 3 10-bit sample mapping.
  */
  if ( MAP3_10(word[0]) )
  {
    out[0] = MAP3_10_A(word[0]);
    out[1] = MAP3_10_B(word[0]);
    out[2] = MAP3_10_C(word[0]);
    *num_out = 3;
    return ( 1 );
  }

  /*************************************************
  ** Check for 4 7-bit sample mapping.
  */
  if ( MAP4_7(word[0]) )
  {
    out[0] = MAP4_7_A(word[0]);
    out[1] = MAP4_7_B(word[0]);
    out[2] = MAP4_7_C(word[0]);
    out[3] = MAP4_7_D(word[0]);
    *num_out = 4;
    return ( 1 );
  }

  /*************************************************
  ** Check for 5 12-bit sample mapping.
  */
  if ( MAP5_12(word[0]) )
  {
    out[0] = MAP5_12_A(word[0]);
    out[1] = MAP5_12_B(word[0]);
    out[2] = MAP5_12_C(word[0],word[1]);
    out[3] = MAP5_12_D(word[1]);
    out[4] = MAP5_12_E(word[1]);
    *num_out = 5;
    return ( 2 );
  }

  /*************************************************
  ** Check for 4 15-bit sample mapping.
  */
  if ( MAP4_15(word[0]) )
  {
    out[0] = MAP4_15_A(word[0]);
    out[1] = MAP4_15_B(word[0],word[1]);
    out[2] = MAP4_15_C(word[1]);
    out[3] = MAP4_15_D(word[1]);
    *num_out = 4;
    return ( 2 );
  }

  /*************************************************
  ** Check for 1 28-bit sample mapping.
  */
  if ( MAP1_28(word[0]) )
  {
    out[0] = MAP1_28_A(word[0]);
    *num_out = 1;
    return ( 1 );
  }

  /*************************************************
  ** Unknown mapping.
  */
  *num_out = 0;
  return ( 1 );
}


int readE1 ( int num , FILE *fptr , struct wfdiscList * wfStruc )
{
    int *intPtr , nerr , jdx ;
    float    *fltPtr ;
  short comp_size, num_samp[MAX_REC];
  int num_read;
  char num_diff;
  int check=0;

  int rec_num, comp_length;
  int total_samp;
  int *comp_data, *uncomp_data[MAX_REC];
  int num_comp, num_uncomp;
  int i, j, k;

  int xint[2];
  unsigned short *xshort = ( unsigned short * ) xint;
  unsigned char *xchar = ( unsigned char * ) xint;

    intPtr = (int * ) smMalloc ( num * sizeof ( int ) ) ;
    fltPtr = (float * ) smMalloc ( num * sizeof ( float ) ) ;
    if ( !intPtr || !fltPtr ) {
	if ( intPtr ) smFree ( intPtr ) ;
	if ( fltPtr )  smFree ( fltPtr ) ;
	return 1 ;
    }
    
   /*************************************************
  ** Read all records from data file
  ** (or up to NUM samples).
  */
  for ( rec_num=0, total_samp=0 ; rec_num<MAX_REC ; ++rec_num )
  {
    /*************************************************
    ** Read header.
    */
    num_read = fread ( xint, sizeof(int), 2, fptr );
    if ( num_read != 2 )
      break;

    /*************************************************
    ** Swap bytes if needed.
    */
    if ( IS_LITTLE_ENDIAN )
      for ( i=0 ; i<2 ; ++i )
        byteswap ( &xshort[i], sizeof(short) );

    /*************************************************
    ** Get size of compressed data.
    */
    comp_size = ( int ) xshort[0];

    /*************************************************
    ** Get the length of the compressed data.
    */
    comp_length = comp_size/4 - 2;

    /*************************************************
    ** Get number of data samples.
    */
    num_samp[rec_num] = ( int ) xshort[1];

    /*************************************************
    ** Get number of differences.
    */
    num_diff = ( int ) xchar[4];

    /*************************************************
    ** Get check value.
    */
    check = ( int ) xchar[5] << 24 | 
            ( int ) xchar[6] << 16 | 
            ( int ) xchar[7] <<  8 ;
    check = check >> 8;

    /*************************************************
    ** Allocate memory for the compressed data.
    */
    comp_length = comp_size/4 - 2;
    comp_data = calloc ( comp_length, sizeof(int) );

    /*************************************************
    ** Read entire data record.
    */
    num_read = fread ( comp_data, sizeof(int), comp_length, fptr );
    if ( num_read != comp_length )
      break;
      
    /*************************************************
    ** Swap bytes if needed.
    */
    if ( IS_LITTLE_ENDIAN )
      for ( i=0 ; i<num_read ; ++i )
        byteswap ( &comp_data[i], sizeof(int) );

    /*************************************************
    ** Allocate memory for the uncompressed data.
    */
    uncomp_data[rec_num] = calloc ( num_samp[rec_num], sizeof(int) );

    /*************************************************
    ** Demap data.
    */
    for ( i=0, j=0 ;
          i<comp_length && j<num_samp[rec_num] ;
          i+=num_comp, j+=num_uncomp )
    {
      num_comp = demap ( comp_data+i,
                         uncomp_data[rec_num]+j, 
                         &num_uncomp );
    }

    /*************************************************
    ** Decompress data.
    */
    for ( i=0 ; i<num_diff ; ++i )
      for ( j=1 ; j<num_samp[rec_num] ; ++j )
        uncomp_data[rec_num][j] += uncomp_data[rec_num][j-1];

    /*************************************************
    ** Check decompression.
    */
    if ( check != uncomp_data[rec_num][num_samp[rec_num]-1] )
    {
      printf ( "Error decompressing \n" );
      printf ( "check value does not match last value.\n" );
      break;
    }

    /*************************************************
    ** Set number of samples to output.
    */
    total_samp += num_samp[rec_num];
    if ( num > 0 && num <= total_samp )
      break;
  }

  /*************************************************
  ** Set number of samples to output.
  */
  if ( num > 0 && num < total_samp )
    total_samp = num;
   
    
    for(i=0, k=0 ; i<=rec_num && k<total_samp ; ++i )
       for ( j=0 ; j<num_samp[i] && k<total_samp ; ++j, ++k )
          fltPtr[ k ] = (float)( uncomp_data[i][j] );   
  
    free(comp_data);
    for(i=0; i < rec_num; ++i )
    	free(uncomp_data[i]);
    
    wfStruc->element->nsamp = num ;
    wfStruc->seis->i = fltPtr;
    wfStruc->seis->Cmplx = 0;

    return 0 ;

} 
