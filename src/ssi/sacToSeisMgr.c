#include <stdio.h>
#include <math.h>

#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "hdr.h"
#include "dfm.h"
#include "mem.h"
#include "ssi.h"

#include "smDataIO.h"
#include "cssListStrucs.h"
#include "dblPublicDefs.h"
#include "cssListOps.h"
#include "dblErrors.h"

void gcCollect ( DBlist dblList );

int Unique(int *array, int size, int wfid) 
{
  /* Check for a unique wfid */
  int j;
  for(j=0;j<=size;++j)
    if(wfid==array[j]) return 0;
  return 1;
}

int next_wfid(int *array, int size)
{
  /* Set next wfid to max_wfid + 1 */
     int j;
     int tmp = 0; 
     for(j=0;j<=size;++j) {
       if(array[j]>=tmp) tmp=array[j]+1;
     }
     return tmp;
}

void sacToSeisMgr ( int lnew , int lupdate , int ldata , int *nerr )
{
    /* Declare Variables. */
    char * worksetName , defaultWorksetName[] = "workset01" ;
    int jdfl ;
    int takeEvid = FALSE ;
    int ndx1 , ndx2 , nunused , ndaerr ;
    float unused ;
    sacSACdata *data = NULL ;
    int * nwfid_array;

    *nerr = 0 ;

    /* Initialize SeisMgr error handler */
    dblClearErrorList () ;

    /* Get default workset name */
    worksetName = smGetDefaultWorksetName () ;

    if ( worksetName && lnew ) {
        smDeleteWorksetByName ( worksetName ) ;
    }
    else if ( !worksetName ) {
	lnew = TRUE ;
	worksetName = defaultWorksetName ;
    }

    /* Be self consistent */
    if ( lnew ) 
	lupdate = FALSE ;

    if ( lupdate )
	ldata = FALSE ;

    /* Allocate sacSACdata */
    if ( ldata ) {
	data = ( sacSACdata * ) malloc ( sizeof ( sacSACdata ) ) ;
	if ( !data ) {
	    *nerr = 301 ;
	    setmsg ( "ERROR" , *nerr ) ;
	    outmsg () ;
	    clrmsg () ;
	    return ;
	}
    }


    if ( lnew ) {
	if ( 0 != smCreateEmptyWorkset ( worksetName ) ) {
	    *nerr = 301 ;
	    setmsg ( "ERROR" , *nerr ) ;
	    outmsg () ;
	    clrmsg () ;
	    return ;
	}
    }


    /* Allocate memory for wfid array */
    nwfid_array = (int *) malloc ( cmdfm.ndfl * sizeof (int) );
    if ( !nwfid_array ) {
      *nerr = 301 ;
      setmsg ( "ERROR" , *nerr ) ;
      outmsg () ;
      clrmsg () ;
      return ;
    }


    /* Loop through sac data file list, writing data to the tree. */

    for ( jdfl = 0 ; jdfl < cmdfm.ndfl ; jdfl++ ) {
	int localLdata = Nlndta[ jdfl+1 ] <= 0 ? FALSE : ldata ;

	/* Get next waveform. */
	getfil ( jdfl+1 , localLdata , &nunused , &ndx1 , &ndx2 , nerr ) ;
	if ( *nerr ) {
	    setmsg ( "ERROR" , *nerr ) ;
	    outmsg () ;
	    clrmsg () ;
	    *nerr = 1401 ;
	    return ;
	}

	/* Check for nonunique wfid */
	nwfid_array[jdfl]=0;
	if( ! Unique(nwfid_array,jdfl,*nwfid) ) {
	  *nwfid = next_wfid(nwfid_array,jdfl);
	  nwfid_array[jdfl] = *nwfid;
	  /* Update the working memory SAC data */
	  putfil( jdfl+1, nerr);
	}
	else {
	  nwfid_array[jdfl] = *nwfid;
	}


  
	/* update pertinent information */
	if ( *leven )
	    *ennd = *begin + *delta*(float)( *npts - 1 );
	else
	    extrma( cmmem.sacmem[cmdfm.ndxdta[jdfl][1]], 1, *npts, begin,
		    ennd, &unused );

	if( (((*stla != cmhdr.fundef && *stlo != cmhdr.fundef) && *evla !=
	  cmhdr.fundef) && *evlo != cmhdr.fundef) && *lcalda ){
	    *az = 0.;
	    *baz = 0.;
	    *gcarc = 0.;
	    *dist = 0.;
	    if ((fabs(*stla - *evla) > RNDOFF ) || (fabs(*stlo - *evlo) > RNDOFF)) {
		distaz( *evla, *evlo, (float*)stla, (float*)stlo, 1, (float*)dist,
		  (float*)az, (float*)baz, (float*)gcarc, &ndaerr );
		if( ndaerr != 0 ){
		    *dist = cmhdr.fundef;
		    *az = cmhdr.fundef;
		    *baz = cmhdr.fundef;
		    *gcarc = cmhdr.fundef;
		}
	    }
	} /* end if( (((*stla != cmhdr.fundef ... ) */

	if ( localLdata ) {
	    extrma( cmmem.sacmem[ndx1], 1, *npts, depmin, depmax, depmen );
	} 

	/* Disallow undefined kstnm and kcmpnm */
	if ( uniqueStaAndChan () )
	    putfil ( jdfl + 1 , nerr ) ;

	/* Put the header into the sacHeader struct. */
	SacHeaderToDB ( &( globalSacHeader[ jdfl ] ) , lupdate ? eventHeader : allHeader , jdfl + 1 ) ;

	/* Put the data into the sacData struct. */
	if ( localLdata ) {
	    data->dataType = *iftype ;
	    data->yarray   = cmmem.sacmem[ndx1] ;
	    data->xarray   = ( ndx2 == 1 ) ? NULL : cmmem.sacmem[ndx2] ;
	}

	/* determine takeEvid to pass into sacLoadFromHeaderAndData */
	if( !cmdfm.lread || jdfl < cmdfm.nfilesFirst ) {
	    takeEvid = TRUE ;
	}
	else{
	    if( cmdfm.nreadflag == RDB )
	        takeEvid = TRUE ;

	    else if( cmdfm.nreadflag == HIGH) {
		if( cmdfm.ltrust )
		    takeEvid = TRUE ;
		else
		    takeEvid = FALSE ;
	    }

	    else{
		takeEvid = FALSE ;
	    }
	}

	/* Put the data into the tree */
	sacLoadFromHeaderAndData( &( globalSacHeader[ jdfl ] ) , data, 
				  worksetName, 0, lnew ? -1 : jdfl, localLdata,
				  takeEvid );

    } /* end for */

    free(nwfid_array);

    if ( ldata )
	free ( data ) ;

    gcCollect ( smGetDefaultTree() ) ;

} /* end sacToSeisMgr */
