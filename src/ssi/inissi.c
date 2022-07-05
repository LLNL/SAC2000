#include <stdio.h>

#include "../SeisMgr/dbselect/dbDefaults.h"
#include "ssi.h"
#include "dfm.h"
#include "../SeisMgr/cssListOps/cssStrucs.h"
#include "../SeisMgr/cssListOps/cssListStrucs.h"
#include "../SeisMgr/cssListOps/dblPublicDefs.h"
#include "../SeisMgr/cssListOps/cssListOps.h"
#include "../SeisMgr/smDataIO.h"

int linkedToOracleLibs ;  /* True if oracle libraries are linked to sac */
int triedAndFailed ;      /* True if sac tried to link to libs and failed */
void dblClearErrorList(void);
        /*=====================================================================
         * PURPOSE: Initialization of the data base module.
         *=====================================================================
         * PARAMETERS:
         *=====================================================================
         * VARIABLE DEFINITIONS:
         *=====================================================================
         * MODIFICATION HISTORY:
	 *      980127: added global variable: linkedToOracleLibs.  maf
	 *	971208:	Original version.
         *===================================================================== */

void inissi () 
{
    char * worksetName ;

    /* set flag */
    linkedToOracleLibs = FALSE ;
    triedAndFailed = FALSE ;

    /* Initialize SeisMgr error handler */
    dblClearErrorList () ;

    /* initialize query */
    dbSetQueryDefaults () ;

    /* delete files from SeisMgr (if this is a INICM command) */
    worksetName = smGetDefaultWorksetName () ;
    if ( worksetName )
	smDeleteWorksetByName ( worksetName ) ;
} /* end inissi */

