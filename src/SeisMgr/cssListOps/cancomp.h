
#ifndef _cancomp_h_
#define _cancomp_h_

/***************************************
** Include files.
*/
#include <stdlib.h>

/***************************************
** Definitions.
*/
#define CANCOMP_ERR	-1	/* unrecoverable error (malloc fails) */
#define CANCOMP_SUCCESS	 0	/* success */
#define CANCOMP_NOT_20   1	/* number of samples not divisible by 20 */
#define CANCOMP_CORRUPT	 2	/* corrupted call */
#define CANCOMP_EXCEED	 3	/* number of bytes available in compressed
				   data exceeded during decompression */
/***************************************
** Function declarations.
*/
int cancomp( unsigned char *b, unsigned int *y, int *n, int m, unsigned int *v0 );
int uncancomp( unsigned char *b, unsigned int *y, int *n, int m, int *v0 );

#endif 
