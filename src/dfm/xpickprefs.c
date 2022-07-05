#include <stdio.h>
#include "../../inc/dfm.h"


void setmsg(char* ktype, int number);
void outmsg(void);
int lckeyExact(char* kkey,int kkey_s);
int lcmore(int* nerr);


void xpickprefs ( int* nerr )
{

    /*=====================================================================
     * PURPOSE:  By default, the pickPreferences file is not used in 
     *           reading picks from CSS data nor in passing data from
     *           SeisMgr to the SAC data buffers.
     *           This command controls the use of the preferences file.
     *=====================================================================
     * OUTPUT ARGUMENTS:
     *            NERR passes back an error number if an error occured.
     *=====================================================================
     * MODULE/LEVEL:  
     *=====================================================================
     * GLOBAL INPUT:
     *=====================================================================
     * GLOBAL OUTPUT:
     *    DFM:     cmdfm.lpref
     *=====================================================================
     * SUBROUTINES CALLED:
     *=====================================================================
     * MODIFICATION HISTORY:
     *    000606:  Original version.  maf
     *===================================================================== */
    /* PROCEDURE: */
    *nerr = 0;


    if( lcmore( nerr ) ) {
	if( lckeyExact( "ON#$", 5 ) ) {
	    cmdfm.lpref = TRUE ;
	}

	else if( lckeyExact( "OFF#$", 6 ) ) {
	    cmdfm.lpref = FALSE ;
	}

	else{
	    *nerr = 1394 ;
	    setmsg( "ERROR" , *nerr ) ;
	    outmsg() ;
	}
    }
    else
	cmdfm.lpref = !cmdfm.lpref ;
}
