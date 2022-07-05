#include <stdio.h>

#include "dfm.h"
#include "ssi.h"
#include "cssStrucs.h"
#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "smDataIO.h"


void setmsg(char* ktype, int number);
void outmsg(void);
void clrmsg(void);
void dblClearErrorList(void);
void getfil(int idfl, int ldta, int* nlen, int* ndx1, int* ndx2, int* nerr);
void SacHeaderToDB ( struct SACheader *header , int whichHeaders , int idfl );
int deleteAllSacFiles ( int* nerr , int lname );
void CSStoSAC ( int idfl , struct SACheader *header , struct trace *seis , int lname , int lcutnow , int* nerr );


void rollback (int whichHeaders , int * nerr)
{

    DBlist tree ;
    struct wfdiscList *wfL = NULL ;
    int check ;
    int notused1 , notused2 , notused3 ;

    *nerr = 0 ;

    /* Initialize SeisMgr error handler */
    dblClearErrorList () ;

    /* get tree for default wordset */
    tree = smGetDefaultTree () ;

    if ( whichHeaders != allHeader ) {
        int jdfl ;

        for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl ++ ) {
	    getfil ( jdfl + 1 , FALSE , &notused1 , &notused2 , &notused3 , nerr ) ;
            SacHeaderToDB ( &( globalSacHeader[ jdfl ] ) , -whichHeaders , 0 ) ;
	}
    }

    check = deleteAllSacFiles ( nerr , FALSE ) ;
    if ( *nerr ) {
	if ( check < cmdfm.ndfl )
	    *nerr = 1401 ;
	else
	    *nerr = 1402 ;
	return ;
    }
    do {
        /* Get next waveform. */
        if ( ! ( wfL = dblNextTableInstance ( wfL , tree , dbl_LIST_WFDISC ) ) )
            break ;

        CSStoSAC ( ++cmdfm.ndfl , &( globalSacHeader[ cmdfm.ndfl -1 ] ) ,
                   wfL->seis , FALSE , FALSE , nerr ) ;
	if ( *nerr ) {
	    setmsg ( "ERROR" , *nerr ) ;
	    outmsg () ;
	    clrmsg () ;
	    *nerr = 1401 ;
	    break ;
	}
    } while ( wfL ) ;
} /* end rollback */
