#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
int /*FUNCTION*/ fndelcl( kcl , kcl_s , fileNumber )
char *kcl ;   int kcl_s ;
int fileNumber ;
{
	char *ptr1 , *ptr2 ;
	int ldelcl_v;
	int index1, index2, j1, idx = 1 ;

	/*====================================================================
	 * PURPOSE:  To delete an ENTRY in a character list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCL:     Character list. [cl]
	 *    fileNumber:  file to delete. [i]
	 *=====================================================================
	 * FUNCTION VALUE:  .TRUE. if fileNumber was found and deleted,
	 *                  .FALSE. if not found.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * EXAMPLES OF USE:
	 *=====================================================================
	 * SUBROUTINES CALLED:
	 *    SACLIB:  INDEXA, LNXTCL
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCL:     Number of characters in KCL. [i]
	 *    KDEL:    Delimiter between entries. [c1]
	 *    IDEL:    Used to search for delimiter in character list. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    980922:  Original version. plagerized from ldelcl.c.  maf
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860918
	 *===================================================================== */
	/* PROCEDURE: */
	/* - Assume the entry will not be found. */
	ldelcl_v = FALSE;

	/* - Initialize character pointer and determine length of entry. */

	index1 = 0;


	/* - Loop on each ENTRY in character list. */

	while ( lnxtcl( kcl,kcl_s, &index1, &index2 ) ){

		/* -- If match, delete characters in list, fill with delimiter, and set
		 *    return value to .TRUE. */

		if( idx == fileNumber ) {
			ptr1 = kcl + index1 - 2 ;
			ptr2 = kcl + index2 ;

			for ( j1 = index2 ; j1 < kcl_s - 1 ; j1++ , ptr1++ , ptr2++ )
			    *ptr1 = *ptr2 ;

			while ( ptr1 < ptr2 ) {
			    *ptr1 = ' ' ;
			    ptr1++ ;
			}

			ldelcl_v = TRUE;

			break ;
		}

		idx++ ;
	}

L_8888:
	return( ldelcl_v );

} /* end of function */

