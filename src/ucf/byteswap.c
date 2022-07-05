
/* this routine will reverse the byte order of 2, 4, or 8 byte numeric types.
   swapee is the data to be byteswapped.  Nbytes is the number of bytes to 
   swap.  byteswap returns 0 for success and -1 for failure. maf 2/5/03 */

int byteswap( void *swappee, int Nbytes )
{
    char temp, *ptr = swappee ;

    if( Nbytes == 2 ){
        temp   = ptr[0] ;
        ptr[0] = ptr[1] ;
        ptr[1] = temp ;
        return 0 ;
    }

    if( Nbytes == 4 ){
        temp   = ptr[0] ;
        ptr[0] = ptr[3] ;
        ptr[3] = temp ;

        temp   = ptr[1] ;
        ptr[1] = ptr[2] ;
        ptr[2] = temp ;
        return 0 ;
    }

     if( Nbytes == 8 ){
        temp   = ptr[0] ;
        ptr[0] = ptr[7] ;
        ptr[7] = temp ;

        temp   = ptr[1] ;
        ptr[1] = ptr[6] ;
        ptr[6] = temp ;

        temp   = ptr[2] ;
        ptr[2] = ptr[5] ;
        ptr[5] = temp ;

        temp   = ptr[3] ;
        ptr[3] = ptr[4] ;
        ptr[4] = temp ;
        return 0 ;
    }

    return -1 ;
}
