#include <stdio.h>
#include <string.h>
#include "../cssListOps/cssStrucs.h"
#include "../cssListOps/cssListStrucs.h"
#include "../cssListOps/dblPublicDefs.h"
#include "../cssListOps/cssListOps.h"
#include "gc.h"


/* Routines that clear out unused records from the css tables */


struct wftagList * gcCollectWftag ( struct wftagList *wtL , DBlist dblList )
{

	struct arrivalList     *arPntr;
	struct assocList       *asPntr;
	struct CSStree *tree;
	char * pString ;
	struct wftagList * returnValue = wtL->next ;

	tree = (struct CSStree *) dblList;
	if(!tree) 
	    return returnValue ;

	if ( !gcCheckWftag ( wtL->element , tree->wfHead ) ) {
	    /* remove associated arrival and assoc records */
	    pString = lower ( wtL->element->tagname ) ;
	    if ( !strcmp ( pString , "arid" ) ) {
		for ( arPntr = tree->arHead ; arPntr ; arPntr = arPntr->next ) {
		    if ( arPntr->element->arid == wtL->element->tagid )
			dblDeleteTableInstance ( dbl_LIST_ARRIVAL , dblList ,
						 arPntr ) ;
		} /* end for ( arPntr ... ) */

		for ( asPntr = tree->asHead ; asPntr ; asPntr = asPntr->next ) {
		    if ( asPntr->element->arid == wtL->element->tagid )
			dblDeleteTableInstance ( dbl_LIST_ASSOC , dblList , 
						 asPntr ) ;
		} /* end for ( asPntr ... ) */
	    } /* end if ( !strcmp ( pString , "arid" ) ) */

	    /* remove the wftag record */
	    dblDeleteTableInstance ( dbl_LIST_WFTAG , dblList , wtL ) ;
	} /* end !gcCheckWftag () */

	return returnValue ;
}


struct sensorList * gcCollectSensor ( struct sensorList *seL , DBlist dblList )
{
        struct CSStree *tree;
	struct sensorList * returnValue = seL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckSensor ( seL->element , tree->wfHead ) ) {
            /* remove the sensor record */
            dblDeleteTableInstance ( dbl_LIST_SENSOR , dblList , seL ) ;
        } /* end !gcCheckSensor () */

	return returnValue ;
}



struct sitechanList * gcCollectSitechan ( struct sitechanList *scL , DBlist dblList )
{
        struct CSStree *tree;
	struct sitechanList * returnValue = scL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckSitechan ( scL->element , tree->wfHead ) ) {
            /* remove the sitechan record */
            dblDeleteTableInstance ( dbl_LIST_SITECHAN , dblList , scL ) ;
        } /* end !gcCheckSitechan () */

	return returnValue ;
}




struct siteList * gcCollectSite ( struct siteList *siL , DBlist dblList )
{
        struct affiliationList     *afPntr;
        struct CSStree *tree;
	struct siteList * returnValue = siL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckSite ( siL->element , tree->wfHead ) ) {
            /* remove associated affiliation records */
	    for ( afPntr = tree->afHead ; afPntr ; afPntr = afPntr->next ) {
                if ( !strcmp ( afPntr->element->sta , siL->element->sta ) )
                    dblDeleteTableInstance ( dbl_LIST_AFFILIATION , dblList ,
                                             afPntr ) ;
	    } /* end for ( afPntr ... ) */

            /* remove the site record */
            dblDeleteTableInstance ( dbl_LIST_SITE , dblList , siL ) ;
        } /* end !gcCheckSite () */

	return returnValue ;
}



struct instrumentList * gcCollectInstrument ( struct instrumentList *inL , DBlist dblList )
{
        struct CSStree *tree;
	struct instrumentList * returnValue = inL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckInstrument ( inL->element , tree->seHead ) ) {
            /* remove the instrument record */
            dblDeleteTableInstance ( dbl_LIST_INSTRUMENT , dblList , inL ) ;
        } /* end !gcCheckInstrument () */

	return returnValue ;
}



struct originList * gcCollectOrigin ( struct originList *orL , DBlist dblList )
{
        struct assocList     *asPntr;
        struct CSStree *tree;
	struct originList * returnValue = orL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckOrigin ( orL->element , tree->evHead ) ) {
            /* remove associated assoc records */
            for ( asPntr = tree->asHead ; asPntr ; asPntr = asPntr->next ) {
                if ( asPntr->element->orid == orL->element->orid )
                    dblDeleteTableInstance ( dbl_LIST_ASSOC , dblList ,
                                             asPntr ) ;
            } /* end for ( asPntr ... ) */

            /* remove the origin record */
            dblDeleteTableInstance ( dbl_LIST_ORIGIN , dblList , orL ) ;
        } /* end if( !gcCheckOrigin () ) */

	return returnValue ;
}




struct eventList * gcCollectEvent ( struct eventList *evL , DBlist dblList )
{
        struct CSStree *tree;
	struct eventList * returnValue = evL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckEvent ( evL->element , tree->wtHead ) ) {
            /* remove the event record */
            dblDeleteTableInstance ( dbl_LIST_EVENT , dblList , evL ) ;
        } /* end !gcCheckEvent () */

	return returnValue ;
}




/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

 *              NOTE:  cssStrucs, and cssListStrucs do not             *

 *              contain the network table, so the following            *

 *              routine is neither possible, nor necessary.            *

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
struct networkList * gcCollectNetwork ( struct networkList *nwL , DBlist dblList )
{
        struct CSStree *tree;
	struct networkList * returnValue = nwL->next ;

        tree = (struct CSStree *) dblList;
        if(!tree) 
	    return returnValue ;

        if ( !gcCheckNetwork ( nwL->element , tree->afHead ) ) {
            * remove the event record *
            dblDeleteTableInstance ( dbl_LIST_NETWORK , dblList , nwL ) ;
        } * end !gcCheckNetwork () *

	return returnValue ;
}

*/





void gcCollect ( DBlist dblList )
{
	struct CSStree *tree = (struct CSStree *) dblList;

	if(!tree) {
		return;
	}

	struct wftagList *wtL = tree->wtHead ;
	struct sensorList *seL = tree->seHead ;
	struct sitechanList *scL = tree->scHead ;
	struct siteList *siL = tree->slHead ;
	struct instrumentList *inL = tree->inHead ;
	struct originList *orL = tree->orHead ;
	struct eventList *evL = tree->evHead ;


	while ( wtL )
	    wtL = gcCollectWftag ( wtL , dblList ) ;

	while ( seL )
	    seL = gcCollectSensor ( seL , dblList ) ;

	while ( scL )
	    scL = gcCollectSitechan ( scL , dblList ) ;

	while ( siL )
	    siL = gcCollectSite ( siL , dblList ) ;

	while ( inL )
	    inL = gcCollectInstrument ( inL , dblList ) ;

	while ( evL )
	    evL = gcCollectEvent ( evL , dblList ) ;

        while ( orL )
            orL = gcCollectOrigin ( orL , dblList ) ;

}
