#include <stdio.h>

#include "dfm.h"
#include "ssi.h"


void sacToSeisMgr ( int lnew , int lupdate , int ldata , int *nerr );
void rollback (int whichHeaders , int * nerr);

void alignFiles ( int *nerr )
{
    /* Commit, recall, or rollback existing data as per user specs. */
    switch ( cmdfm.icomORroll ) {
	case COMMIT :
	    sacToSeisMgr ( 0 , 0 , 1 , nerr ) ;
	    break ;
	case RECALL :
	    sacToSeisMgr ( 0 , 1 , 0 , nerr ) ;
	    if ( *nerr )
		break ;
	    rollback ( wfHeader , nerr ) ;
	    break ;
	case ROLLBACK :
	    rollback ( allHeader , nerr ) ;
	    break ;
    } /* end switch */
}
