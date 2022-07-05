#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"

void apcmsg2(char* kalpha, int kalpha_s);


void /*FUNCTION*/ putcl(kcl, kcl_s, kentry, kentry_s, nerr)
char *kcl;   int kcl_s;
char *kentry;   int kentry_s;
int *nerr;
{
	byte kdel;
	int ibeg, idel, iend_, ncl, nentry;
	void *_p0;
        char *strtemp;

	/*=====================================================================
	 * PURPOSE:  To put an entry into a character list.
	 *=====================================================================
	 * INPUT ARGUMENTS:
	 *    KCL:     Character list. [c]
	 *    KENTRY:  Character entry to put into list. [c]
	 *             Trailing blanks are trimmed before putting into list.
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    KCL:     Character list. [c]
	 *    NERR:    Error return flag. [i]
	 *             = 0 No error.
	 *             = 1 Delimiter character found in entry.
	 *             = 2 No room for entry in character list.
	 *=====================================================================
	 * MODULE/LEVEL:  SERVICE/4
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *    NCL:     Number of characters in KCL. [i]
	 *    KDEL:    Delimiter between entries. [c1]
	 *    NENTRY:  Length of entry without trailing blanks. [i]
	 *    IDEL:    Used to search for delimiter in new entry. [i]
	 *    IEND:    Ending location of last entry in list. [i]
	 *    IBEG:    Beginning location for new entry in list. [i]
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    870722:  Added descriptive error messages.
	 *    860128:  Original version.
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  860128
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

	/* - Determine length of character list and  delimiter. */

	ncl = (kcl_s - 1);
	kdel = kcl[0];

	/* - Determine length of entry without trailing blanks. */

	nentry = indexb( kentry,kentry_s );

        strtemp = malloc(nentry+1);
        strncpy(strtemp,kentry,nentry);
        strtemp[nentry] = '\0';

	/* - Make sure delimiter is not present in entry. */

	idel = indexa( strtemp, nentry + 1, kdel, TRUE, TRUE );

	if( idel > 0 ){
		*nerr = 920;
		setmsg( "ERROR", *nerr );
		apcmsg2(kentry,nentry);
		goto L_8888;
		}

	/* - Determine end of last entry in character list.
	 *   (This is the first nonoccurance of delimiter searching backwards.) */

	iend_ = indexa( kcl,kcl_s, kdel, FALSE, FALSE );

	/* - Make sure there is room in character list for entry and delimiter. */

	ibeg = iend_ + 2;
	if( (ibeg + nentry + 1) > ncl ){
		*nerr = 920;
		setmsg( "ERROR", *nerr );
                apcmsg2(kentry,nentry);
		goto L_8888;
		}

	/* - Copy entry to character list. */

	subscpy( kcl, ibeg - 1, ibeg + nentry - 2, kcl_s - 1, strtemp);

L_8888:
        free(strtemp);

	return;

} /* end of function */

