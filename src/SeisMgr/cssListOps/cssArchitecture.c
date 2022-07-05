#include <stdio.h>
#include <unistd.h>
#include <stdint.h>

/* prototype */
void archErr( char *message ) ; /* handles errors for SetIndianSize() */

const int endian = 0x12345678;

/* Accessor */
int  IsBigendinX()
{ return (*(unsigned char *)&endian == 0x12); }

int  IsBigendin()
{ 
    union {
        uint32_t i;
        char c[4];
    } e = { 0x01000000 };

    return e.c[0];	
 
}

void archErr( char *message )
{
   printf( "Error determining byte order\n%s\n", message ) ;
   printf("Will assume machine is bigendian when reading CSS waveforms\n" );
}
