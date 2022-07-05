#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include <string.h>
#include "../../inc/mach.h"
#include "../../inc/gem.h"
#include "../../inc/gam.h"
#include "../../inc/hdr.h"
#include "../../inc/dfm.h"
#include "../../inc/fks.h"
#define MAXRECORDLENGTH 128 /* max characters in a line of the event file, maf 960620 */
#define MAXSTRING 10000 /* Max characters in a string like psxy etc... */
#define MAXSTANAME 9 /* Max number of characters in a station name */

void /*FUNCTION*/ xgmtmap(nerr)
int *nerr;
{

        FILE *fp;

        float minlat, maxlat, minlon, maxlon; 
        float deltalat, deltalon;
	float deltalatbig = 100.0;
	float deltalonbig = 100.0; 
        float width;
        float lat_axis_len, lon_axis_len;
        float lon_annot_spac, lon_tick_spac, lat_annot_spac, lat_tick_spac;
        float lonspac, latspac;
        float latlon_undef = -1000.;    /* undefined value for lat and lon */
        float dlonsave, dlatsave;

        static float user_west  = -1000.;
        static float user_east  = -1000.;
        static float user_north = -1000.;
        static float user_south = -1000.;

        static int luser_west  = FALSE;
        static int luser_east  = FALSE;
        static int luser_north = FALSE;
        static int luser_south = FALSE;

	static float minscale = 0.1; /* minimum size of symbols */
	static float maxscale = 0.5; /* maximum size of symbols */ 
        int lsize = FALSE; /* default is fixed size event symbols */
        int lresid = FALSE; /* default is fixed size event symbols */
        int lrmean_resid = FALSE; /* default is don't subtract mean from resids */
        float size_undef = -1.0e38;    /* undefined value for size variable */
	float minsize, maxsize, midsize, deltasize; 
	float minneg, minpos, maxneg, maxpos; 
	float minsizeabs, maxsizeabs; 
	float midscale, deltascale;
	float slope, intercept; 

        float *buf = NULL;
	float *size, *pos, *neg;
        float *stalat, *stalon, *evlat, *evlon;
	/* char *staname[MDFL]; */
	/* char staname[MDFL][9]; */
	char **staname; 

        int nch, jdfl, jdfl_, nlen, ndxy, ndxx, i, idx, jdx;
        int lrefset = TRUE;
        int irefsta;
 	int nstationlocs = 0;
        int neventlocs = 0;
	int nsize = 0;
	float sumsize = 0.0;
	float meansize = 0.0;
        int npos = 0;
        int nneg = 0;

        static int lmercator = TRUE;  /* default projection */
        static int lequidistant = FALSE;
        static int lazim_equidist = FALSE;
        static int lrobinson = FALSE;
        static int llambert = FALSE;
        static int lutm = FALSE;
	static int lstanames = FALSE;
	static int ltopo = FALSE;
 
	char grdcoms[MAXSTRING];
        char pscoast[MAXSTRING];
        char psxy[MAXSTRING],pstext[MAXSTRING];
	char ctitle[MAXSTRING]; 
        char cwidth[10];
        char wrange[100], erange[100], srange[100], nrange[100];
        char iwrange[100], ierange[100], isrange[100], inrange[100];
        char carea[100];
        char clatscale[100], clonscale[100], cslatscale[100], clengthscale[100];
        char clon_annot_spac[100], clon_tick_spac[100], clat_annot_spac[100];
        char clat_tick_spac[100];
	char cbounds[100],cibounds[100];
 	char cprojection[100];
        char cmeridian[10],ccenterlat[10],ccenterlon[10],cparallel1[10],cparallel2[10];
	char c_utm_zone[10];
        char staloc[100], evloc[100];
        char bazim[10],mean_residual[10];
        char refsta[10];
        char *scriptname = "gmt.csh";
        char syscommand[100];
        char *viewer;
	char *name_undef = "-12345";  /* undefined value for names */

	/* The following variables are used for setting min and max sizes, maf 960702 */
	float	minSizeInput ,	/* defined by user with scale option */
		maxSizeInput ;	/* defined by user with scale option */
        int	lscale = FALSE; /* default is let the code determine min and max */

 	/* The following variables are used for handling an event file containing
 	   data on additional events,  maf 960620 */
 	FILE * eventFile;
	int leventfile = FALSE; /* default is no additional data */
 	char eventFileName[MCPFN + 1];	/* name of event file */
 	int eventFileNameLength;   /* length of the event file name. */
 	int eventFileLength = 0;	/* number of lines in the event file */
 	char eventBuffer[MAXRECORDLENGTH + 1];  /* stores a line of info from event file. */
 	char * pEventBuffer ;	/* point to elements in eventBuffer */
 	int number_event_station ;
 
	/*=====================================================================
	 * PURPOSE:  To generate a script to input to gmt for production of a
         *           map.
	 *
	 *=====================================================================
	 * OUTPUT ARGUMENTS:
	 *    nerr:    Error flag. Set to 0 if no error occurred.
	 *             Potential error numbers:
	 *=====================================================================
	 * MODULE/LEVEL:   fks/2
	 *=====================================================================
	 * GLOBAL INPUT:
	 *    dfm:     ndfl
	 *=====================================================================
	 * GLOBAL OUTPUT:  (none)
	 *=====================================================================
	 * SUBROUTINES CALLED:  (to be updated)
	 *=====================================================================
	 * LOCAL VARIABLES:
	 *=====================================================================
	 * MODIFICATION HISTORY:
	 *    960111:  Original version.
	 *    991217:  Added topography and station labeling options (peterg)
	 *=====================================================================
	 * DOCUMENTED/REVIEWED:  
	 *===================================================================== */
	/* PROCEDURE: */
	*nerr = 0;

/* PARSING PHASE: */
L_1000:
        if( lcmore ( nerr ) ){
            if( lkreal( "W#EST$", 7, &user_west )){
              luser_west = TRUE;
	      }
            else if( lkreal( "E#AST$", 7, &user_east )){
              luser_east = TRUE;
	      }
            else if( lkreal( "N#ORTH$", 8, &user_north )){
              luser_north = TRUE;
              }
            else if( lkreal( "S#OUTH$", 8, &user_south )){
              luser_south = TRUE;
              }  
/* -- Identify Projection: */
            else if( lckey( "MER#CATOR$", 11 )){
              lmercator    = TRUE;
              lazim_equidist = FALSE;
              lrobinson    = FALSE;
              lequidistant = FALSE;  
	      llambert     = FALSE;
	      lutm         = FALSE;
	      }
            else if( lckey( "AZ#IM_EQUIDIST$", 16 )){
              lmercator    = FALSE;
              lazim_equidist = TRUE;
              lrobinson    = FALSE;
              lequidistant = FALSE;  
	      llambert     = FALSE;
	      lutm         = FALSE;
	      }
            else if( lckey( "ROB#INSON$", 11 )){
              lmercator    = FALSE;
              lazim_equidist = FALSE;
              lrobinson    = TRUE;
              lequidistant = FALSE;  
	      llambert     = FALSE;
	      lutm         = FALSE;
	      }
            else if( lckey( "EQ#UIDISTANT$", 14 )){
              lmercator    = FALSE;
              lazim_equidist = FALSE;
              lrobinson    = FALSE;
              lequidistant = TRUE;  
	      llambert     = FALSE;
	      lutm         = FALSE;
	      } 
            else if( lckey( "LAM#BERT$", 10 )){
              lmercator    = FALSE;
              lazim_equidist = FALSE;
              lrobinson    = FALSE;
              lequidistant = FALSE;  
	      llambert     = TRUE;
	      lutm         = FALSE;
	      } 
            else if( lckey( "UTM$", 5 )){
              lmercator    = FALSE;
              lazim_equidist = FALSE;
              lrobinson    = FALSE;
              lequidistant = FALSE;  
	      llambert     = FALSE;
	      lutm         = TRUE;
	      } 



	    /* Check to see if topography was requested */
	    /* else if( lckey( "TOPO#GRAPHY$", 13 )){
	      ltopo = TRUE;
	      } */

	    else if( lklog( "TOPO#GRAPHY$", 13, &ltopo ) ) 
	      { /* Don't need to do anything here. */ }


	    /* Check to see if station labeling was requested */
	    /* else if( lckey( "STAN#AMES$", 11 )){
	      lstanames = TRUE;
	      } */ 
	    else if( lklog( "STAN#AMES$", 11, &lstanames ) ) 
	      { /* Don't need to do anything here. */ }


 	    /* eventfile reads named file for additional events; maf 960620 */
 	    else if( lkchar("EV#ENTFILE$", 12, MCPFN, eventFileName, MCPFN, &eventFileNameLength ) ){
 	      leventfile = TRUE;
	      eventFileName[eventFileNameLength] = '\0';
              }
	    
            /* -- SCALE min max:  define min and max sizes of marks in gmap, maf 960702 */
            else if( lkrrcp( "SC#ALED$",9, -100.0, 100.0, &minSizeInput, &maxSizeInput ) ){
	      lscale = TRUE ;
	      }

/* Initially just use keyword to decide if scaled symbols */ 
            else if( lckey( "SI#ZE$", 7 )){
              lsize = TRUE;
              } 
            else if( lckey( "MAG#NITUDE$", 12 )){
              lsize = TRUE;
              } 
            else if( lckey( "RE#SIDUAL$", 11 )){
              lresid = TRUE;
              } 
            else if( lckey( "RM#EAN_RESIDUAL$", 17 )){
              lresid = TRUE;
              lrmean_resid = TRUE;
              } 

/* -- "SCALE hdrvar":  the name of the header variable use for scaling symbols. */
	    /*	    else if( lklist( "SC#ALE$",8, (char*)kmlhf.kfhdr[40],9, 10, &cmsmm.irmspick ) ){ */

/* -- Bad syntax. */
	    else{
	      cfmt( "ILLEGAL OPTION:$",17 );
	      cresp();
	      goto L_9999;
	    } 

            goto L_1000;
	  }

	/* sizes and resid are mutually exclusive, but scale requires one of them. */
	/* if lsize and lresid are both true, send an error message. if lscale is */
	/* true and neither lsize nor lresid are true, set lsize true.  maf 960702 */
	if ( lsize && lresid ){
	    cfmt( "MUTUALLY EXCLUSIVE OPTIONS: SIZE and RESIDUALS:$", 49 ) ;
	    cresp();
	    goto L_9999;
	} /* end if ( lsize && lresid ) */

	if ( lscale && !lsize && !lresid ) {
	    lsize = TRUE ;
	}
	

/* CHECKING PHASE: */

/* - Check for null data file list. */

	vflist( nerr );
	if( *nerr != 0  && !leventfile )
	        goto L_9999; 

/* If an event file was specified, check that it exists 
   and count the number of lines, maf 960620 */
	if( leventfile ) {
	  eventFile = fopen(eventFileName, "r");
	  if(eventFile == NULL){
	    *nerr = 1000001;  /* event file not found */ /* @@@###$$$%%% deal with error handling */
	    goto L_9999;
	    }
	  else{
	    while( fgets(eventBuffer, MAXRECORDLENGTH, eventFile) )
	      eventFileLength++;
	    fclose(eventFile);
	    } /* end else */
	  }  

	number_event_station = cmdfm.ndfl + eventFileLength;
	
        if( (buf = (float *)malloc(7 * number_event_station * sizeof(float))) == NULL){
          *nerr = 1302;
          return;
	  }  

        stalat = buf;
        stalon = buf + number_event_station;
        evlat  = buf + 2*number_event_station;
        evlon  = buf + 3*number_event_station;
        size   = buf + 4*number_event_station;
        pos   = buf + 5*number_event_station;
        neg   = buf + 6*number_event_station;

        minlat =  1000.0;
        maxlat = -1000.0;
        minlon =  1000.0;
        maxlon = -1000.0;

	minsize = 1.0e38;
	maxsize = -1.0e38; 
	minneg = 1.0e38;
	maxneg = -1.0e38; 
	minpos = 1.0e38;
	maxpos = -1.0e38; 
	minsizeabs = 1.0e38;
	maxsizeabs = -1.0e38; 


/* Allocate memory for station names */
	/* eventually will want to change from number_event_station 
	   to number_stations after we add a station file option */
	staname = (char ** ) malloc (number_event_station * sizeof (char *) );
	if ( !staname ) {
	  *nerr = 1302;
	  return;
	}

	for (idx = 0; idx < number_event_station; idx++ ) {
	  staname[idx] = (char *) malloc (MAXSTANAME);
	  if ( !staname[idx] ) {
	    for (jdx = idx -1; jdx >= 0; jdx--) {
	      free (staname[jdx]);
	    }
	    free ( staname ) ;
	    *nerr = 1302;
	    return;
	  }
	}


/* DATA INPUT PHASE: */
 	nch = cmdfm.ndfl;	/* nch = number of entries from waveform file */

/* Load up station and event location arrays. */
	for( jdfl = 1; jdfl <= nch; jdfl++ ){
          jdfl_ = jdfl-1;
	  getfil( jdfl, FALSE, &nlen, &ndxy, &ndxx, nerr );
	  if( *nerr != 0 )goto L_9999;

/* Loop over files */
/* Check for a reference station specified in header. */
          if( jdfl == 1) {
            if(strncmp(kuser1, "-12345", 6) == 0){
              lrefset = FALSE;
	    }
            strcpy(refsta,kuser1);
	  }
          if(strcmp(kuser1,refsta) != 0)   lrefset = FALSE;

/* Store station location.  These must be set. */
          if( (*stla != cmhdr.fundef) &&  (*stlo != cmhdr.fundef)  ){
            stalat[jdfl_] = *stla;
            stalon[jdfl_] = *stlo;
	    /*	    staname[jdfl_] = kstnm; */
	    strcpy(staname[jdfl_], kstnm);
            maxlat        = fmax(maxlat,*stla);
            minlat        = fmin(minlat,*stla);
            maxlon        = fmax(maxlon,*stlo);
            minlon        = fmin(minlon,*stlo);
            nstationlocs  += 1;
          }
          else{
            *nerr = 5301;
            goto L_9999;
          }       

/* Check for and store event locations.  These are not required. */
          if( (*evla != cmhdr.fundef) && (*evlo != cmhdr.fundef) ){
            evlat[jdfl_]  = *evla;
            evlon[jdfl_]  = *evlo;
            maxlat        = fmax(maxlat,*evla);
            minlat        = fmin(minlat,*evla);
            maxlon        = fmax(maxlon,*evlo);
            minlon        = fmin(minlon,*evlo);
            neventlocs    += 1;
          }
          else{
            evlat[jdfl_]  = latlon_undef;
          }       

/* Check for and store size/magnitude/variable.  */
/* These are not required. */
	  if (*user0 != cmhdr.fundef) { 
		/* only use the size data if it is in range, maf 960702 */
	      if ( !lscale || ( *user0 > minSizeInput && *user0 < maxSizeInput ) ){
                  size[jdfl_]  = *user0; 
	          nsize += 1;
	          sumsize += size[jdfl_];
	      } /* end if ( lscale && ... */
	      else
		size[jdfl_] = size_undef ;
	  } /* end if ( user0 != cmhdr.fundef)  */
	  else{
            size[jdfl_]   = size_undef;
	  }


/* end of loop over files */
	}


/* nch is increased by the number of entries from the event file. maf 960620 */
        nch = cmdfm.ndfl + eventFileLength ;

/* If an event file was specified, read the data from it. maf 960620 */
	if ( leventfile ) {
	  eventFile = fopen ( eventFileName , "r" ) ; /* open event file */
	  if ( eventFile == NULL ){  /* if it won't open, error out */
	    *nerr = 1000001 ;
	    goto L_9999 ;
	    }

/* Begin loop between lines in the event file. */
	  for ( ; jdfl <= nch ; jdfl++ ){
	    jdfl_ = jdfl-1;
	    /* Get a line from the event file, and parse it */
	    fgets(eventBuffer, MAXRECORDLENGTH, eventFile);
	    pEventBuffer = eventBuffer ;	/* point pointer at beginning of array */
	    
	    /* Store event locations. */
	    evlat[jdfl_]  = atof ( pEventBuffer ) ;  /* @@@###$$$%%% add error checking */
	    while ( *pEventBuffer  == ' ' ) pEventBuffer++ ;  /* move pointer to next number */
	    while ( *pEventBuffer != ' ' ) pEventBuffer++ ;
	    evlon[jdfl_]  = atof ( pEventBuffer ) ;
	    stalat[jdfl_] = latlon_undef ;
	    stalon[jdfl_] = latlon_undef ;
	    maxlat        = fmax(maxlat, evlat[jdfl_]);
	    minlat        = fmin(minlat, evlat[jdfl_]);
	    maxlon        = fmax(maxlon, evlon[jdfl_]);
	    minlon        = fmin(minlon, evlon[jdfl_]);
	    neventlocs    += 1;

	    while ( *pEventBuffer  == ' ' ) pEventBuffer++ ;  /* move pointer to next number */
	    while ( *pEventBuffer != ' ' && *pEventBuffer != '\0') pEventBuffer++ ;
	    while ( *pEventBuffer  == ' ' ) pEventBuffer++ ;

	    /* Check for and store size variables.  These are not required. */
	    if( pEventBuffer != '\0' ){
	      size[jdfl_]  = atof ( pEventBuffer ) ;
	      /* if lscale, size[jdfl_] must be in range. maf 960702 */
	      if ( !lscale || ( size[jdfl_] > minSizeInput && size[jdfl_] < maxSizeInput ) ){
	          nsize += 1;
	          sumsize += size[jdfl_];
		  } /* end if ( !lscale || ... */
	      else {
		  size[jdfl_] = size_undef ;
		  } 
	      } /* end if( pEventBuffer != '\0' ) */
	    else{
	      size[jdfl_]   = size_undef;
	      }      

	    } /* end for ( ; jdfl <= nch ; jdfl++ ) */  
	  } /* end if ( leventfile ) */
 
/* Remove mean if requested */
	meansize = sumsize / ( (float) nsize ); 
	if (lrmean_resid) {
	  sprintf(mean_residual, "%1.2f",meansize);
	  setbbv("MEAN_RESIDUAL", mean_residual, nerr, 13, 9);
	  for( jdfl = 1; jdfl <= nch; jdfl++ ){
	    jdfl_ = jdfl-1;
	    if ( size[jdfl_] != size_undef )  /* if it's not defined, */
	    	size[jdfl_] -= meansize; 	/* don't mess with it. maf 960702 */
	  }
	}

/* Get max and min size/magnitude/resid variable. Overhauled 960702 maf */
	if ( lsize || lresid ) {
	    for( jdfl = 1; jdfl <= nch; jdfl++ ){
		jdfl_ = jdfl-1;
	    	if ( lsize ) {
	            maxsize = lscale ? maxSizeInput : fmax(maxsize,size[jdfl_]);
	            minsize = lscale ? minSizeInput : fmin(minsize,size[jdfl_]);
	    	} /* end if ( lsize ) */
	    	else if ( lresid ) {
	  	    maxsize = lscale ? fmax(minSizeInput,maxSizeInput) : fmax(maxsize,fabs(size[jdfl_]) );
	  	    minsize = fmin(minsize,fabs(size[jdfl_]) );
	  	    if (size[jdfl_] < 0) {
			nneg += 1;
			minneg = fmin(minneg,fabs(size[jdfl_]) );
			maxneg = lscale ? fabs(minSizeInput) : fmax(maxneg,fabs(size[jdfl_]) );
	  	    }
		    else{
			npos += 1;
			minpos = fmin(minpos,size[jdfl_] );
			maxpos = lscale ? fabs(maxSizeInput) : fmax(maxpos,size[jdfl_] );
	  	    }
	    	}
	    }
	}

/* Reset min and max lat and lon if necessary */
	deltalon = fabs(maxlon-minlon);
        deltalat = fabs(maxlat-minlat);

        if( luser_west ){
          minlon = user_west;
	}else{
          minlon = minlon - .1 * deltalon;
	}

        if( luser_east ){
          maxlon = user_east;
	}else{
          maxlon = maxlon + .1 * deltalon;
	}

        if( luser_south){
          minlat = user_south;
	}else{
          minlat = minlat - .1 * deltalat;
	}

        if( luser_north){
          maxlat = user_north;
	}else{
          maxlat = maxlat + .1 * deltalat;
	}

        minlon = fmax(minlon, -180.0);
        maxlon = fmin(maxlon,  180.0);
        minlat = fmax(minlat,  -89.0);
        maxlat = fmin(maxlat,   89.0);

/* Recompute deltas if mins and maxs were changed. */
        deltalat = fabs(maxlat-minlat);
        deltalon = fabs(maxlon-minlon);
        
/* Force aspect ratio to be .75 */
        if( !luser_west && !luser_east && !luser_north && !luser_south){
          if( deltalat <  .75*deltalon ){
            dlatsave = deltalat;
            deltalat = .75 * deltalon;
            minlat = minlat - (deltalat - dlatsave ) / 2.0;
            maxlat = maxlat + (deltalat - dlatsave ) / 2.0;
            minlat = fmax(minlat,-89.0);
            maxlat = fmin(maxlat, 89.0);
            deltalat = maxlat - minlat;
	  }else if(deltalat > .75*deltalon ){
            dlonsave = deltalon;
            deltalon = 1.3333*deltalat;
            minlon = minlon - ( deltalon - dlonsave )/2.0;
            maxlon = maxlon + ( deltalon - dlonsave )/2.0;
            minlon = fmax(minlon, -180.0);
            maxlon = fmin(maxlon,  180.0);
            deltalon = maxlon - minlon;
	  }
        }

/* Set up spacing for intitudinal labeling. */
        if     ( deltalon > 300. ) lon_annot_spac = 100. ;
        else if( deltalon > 150. ) lon_annot_spac =  50. ;
        else if( deltalon > 90.  ) lon_annot_spac =  30. ;
        else if( deltalon > 60.  ) lon_annot_spac =  20. ;
        else if( deltalon > 30.  ) lon_annot_spac =  10. ;
        else if( deltalon > 15.  ) lon_annot_spac =   5. ;
        else if( deltalon > 9.   ) lon_annot_spac =   3. ;
        else if( deltalon > 6.   ) lon_annot_spac =   2. ;
        else if( deltalon > 3.   ) lon_annot_spac =   1. ;
        else if( deltalon > 1.5  ) lon_annot_spac =    .5;
        else if( deltalon > .9   ) lon_annot_spac =    .3;
        else if( deltalon > .6   ) lon_annot_spac =    .2;
        else if( deltalon > .3   ) lon_annot_spac =    .1;
        else if( deltalon > .15  ) lon_annot_spac =   .05;
        else              lon_annot_spac = deltalon / 3.0;

        lon_tick_spac = lon_annot_spac; 


/* Set up spacing for latitudinal labeling. */
        if     ( deltalat > 300. ) lat_annot_spac = 100. ;
        else if( deltalat > 150. ) lat_annot_spac =  50. ;
        else if( deltalat > 90.  ) lat_annot_spac =  30. ;
        else if( deltalat > 60.  ) lat_annot_spac =  20. ;
        else if( deltalat > 30.  ) lat_annot_spac =  10. ;
        else if( deltalat > 15.  ) lat_annot_spac =   5. ;
        else if( deltalat > 9.   ) lat_annot_spac =   3. ;
        else if( deltalat > 6.   ) lat_annot_spac =   2. ;
        else if( deltalat > 3.   ) lat_annot_spac =   1. ;
        else if( deltalat > 1.5  ) lat_annot_spac =    .5;
        else if( deltalat > .9   ) lat_annot_spac =    .3;
        else if( deltalat > .6   ) lat_annot_spac =    .2;
        else if( deltalat > .3   ) lat_annot_spac =    .1;
        else if( deltalat > .15  ) lat_annot_spac =   .05;
        else              lat_annot_spac = deltalat / 3.0;

        lat_tick_spac = lat_annot_spac; 

        sprintf(clon_annot_spac,"%1.3f",lon_annot_spac);
        sprintf(clon_tick_spac, "%1.3f",lon_tick_spac);
        sprintf(clat_annot_spac,"%1.3f",lat_annot_spac);
        sprintf(clat_tick_spac, "%1.3f",lat_tick_spac);

        width = (deltalat < (1.33*deltalon)) ? (fabs(7.5*deltalat/deltalon)) : 5.5; 
        sprintf(cwidth, "%1.1f",width);

/* write lat and lon variables to strings */
        sprintf(wrange,"%1.3f",(minlon));
        sprintf(erange,"%1.3f",(maxlon));
        sprintf(srange,"%1.3f",(minlat));
        sprintf(nrange,"%1.3f",(maxlat));

/* create integer lat and lon range strings for grdraster */
	if ( (float) ((int) minlon) == minlon ) {
	  sprintf(iwrange,"%d",(int)minlon);
	}else{
	  if ( minlon < 0 ) {
	    sprintf(iwrange,"%d",(int)(minlon-1));
	  }else{
	    sprintf(iwrange,"%d",(int)(minlon));
	  }
	}
	if ( (float) ((int) minlat) == minlat ) {
	  sprintf(isrange,"%d",(int)minlat);
	}else{
	  if ( minlat < 0 ) {
	    sprintf(isrange,"%d",(int)(minlat-1));
	  }else{
	    sprintf(isrange,"%d",(int)(minlat));
	  }
	}
	if ( (float) ((int) maxlon) == maxlon ) {
	  sprintf(ierange,"%d",(int)maxlon);
	}else{
	  if ( maxlon < 0 ) {
	    sprintf(ierange,"%d",(int)(maxlon));
	  }else{
	    sprintf(ierange,"%d",(int)(maxlon+1));
	  }
	}
	if ( (float) ((int) maxlat) == maxlat ) {
	  sprintf(inrange,"%d",(int)maxlat);
	}else{
	  if ( maxlat < 0 ) {
	    sprintf(inrange,"%d",(int)(maxlat));
	  }else{
	    sprintf(inrange,"%d",(int)(maxlat+1));
	  }
	}


/* compute area for min resolution in pscoast */
	if( lmercator || lequidistant || llambert || lutm) {
	  sprintf(carea,"%1d",(int)((deltalon*deltalat)+1));
	}else if( lrobinson || lazim_equidist ){ 
	  sprintf(carea,"%1d",(int) 10000 );
	}

        sprintf(clonscale,"%1.3f",(minlon+deltalon*.33));
        sprintf(clatscale,"%1.3f",(minlat+deltalat*.16));
        sprintf(cslatscale,"%1.3f",(minlat+deltalat/2.0));
	/*        sprintf(clengthscale,"%1.3f",(float)((int)(deltalon*30.)));*/
        if((int)(lat_annot_spac*100)<=1) {
	  sprintf(clengthscale,"%1.3f",(float)((lat_annot_spac*100.0)));
	}else{
	  sprintf(clengthscale,"%1.3f",(float)((int)(lat_annot_spac*100.0)));
	}

/* Adjust symbol size range if mapping a large region */ 
	if ( lazim_equidist || ( deltalat > deltalatbig && deltalon > deltalonbig ) ) {
	  minscale = .1;
	  maxscale = .3;
	}


/* Convert size/magnitude/residual vars to linear scale ranging */
/* from minscale to maxscale inches. scale = slope * size + intercept */ 
	if( lsize || lresid ) { 
	  midscale = (minscale + maxscale) / 2.0; 
	  deltascale = maxscale - minscale;
	  
	  midsize = (minsize + maxsize ) / 2.0;
	  deltasize = maxsize - minsize; 
	  
	  slope = deltascale / deltasize; 
	  intercept = midscale - slope * midsize;
	  for( jdfl = 1; jdfl <= nch; jdfl++ ){
	    jdfl_ = jdfl-1;
	    if(size[jdfl_] != size_undef ){
	      if (lresid && size[jdfl_] < 0.0 ){
		size[jdfl_] = slope * size[jdfl_] - intercept;
	      }else{
		size[jdfl_] = slope * size[jdfl_] + intercept;
	      }
	    }
	  }  
	}


/* Buildup pscoast command */
        memset(pscoast,' ',MAXSTRING);
	if ( neventlocs > 0 || nstationlocs > 0 ) {
	  if ( ltopo ) {
	    strcpy(pscoast,"pscoast -K -O ");
	  }else{
	    strcpy(pscoast,"pscoast -K -X0.75 -Y1.0 -P ");
	  }
	}else{
	  if ( ltopo ) {
	    strcpy(pscoast,"pscoast -O ");
	  }else{
	    strcpy(pscoast,"pscoast -X0.75 -Y1.0 -P ");
	  }
	}

/* defined projection character string */
	strcpy(cprojection,"-J");

/* Mercator projection */
        if( lmercator ){        
          strcat(cprojection,"M");
	  strcat(cprojection,cwidth);
	}
/* Equidistant cylindrical projection */
        else if( lequidistant  ){
	  /* compute the central meridian */
          if( minlon != maxlon ){
	    sprintf(cmeridian,"%1.3f",(minlon + maxlon)/2.0);
	  }else{
	    sprintf(cmeridian,"%1.3f",minlon);
	  }
          strcat(cprojection,"Q");
          strcat(cprojection,cmeridian);
          strcat(cprojection,"/");
	  strcat(cprojection,cwidth);
	}
/* Robinson cylindrical projection */
        else if( lrobinson ){
	  /* compute the central meridian */
          if( minlon != maxlon ){
	    sprintf(cmeridian,"%1.3f",(minlon + maxlon)/2.0);
	  }else{
	    sprintf(cmeridian,"%1.3f",minlon);
	  }
          strcat(cprojection,"N");
          strcat(cprojection,cmeridian);
          strcat(cprojection,"/");
	  strcat(cprojection,cwidth);
	}
/* Azimuthal Equidistant Projection */
        else if( lazim_equidist ){
/* Choose the projection center as the first events station */
	  sprintf(ccenterlat,"%1.3f",stalat[0]);
	  sprintf(ccenterlon,"%1.3f",stalon[0]);
          strcat(cprojection,"E");
          strcat(cprojection,ccenterlon);
          strcat(cprojection,"/");
          strcat(cprojection,ccenterlat);
          strcat(cprojection,"/");
	  strcat(cprojection,cwidth);
	}
/* Lambert Projection */
        else if( llambert ){
/* Choose the projection center as the midpoint in lat and lon */
	  sprintf(ccenterlon,"%1.3f",(minlon + maxlon)/2.0);
	  sprintf(ccenterlat,"%1.3f",(minlat + maxlat)/2.0);
	  sprintf(cparallel1,"%1.3f",(minlat + maxlat)/4.0);
	  sprintf(cparallel2,"%1.3f",3.0*(minlat + maxlat)/4.0);
          strcat(cprojection,"L");
          strcat(cprojection,ccenterlon);
          strcat(cprojection,"/");
          strcat(cprojection,ccenterlat);
          strcat(cprojection,"/");
          strcat(cprojection,cparallel1);
          strcat(cprojection,"/");
          strcat(cprojection,cparallel2);
          strcat(cprojection,"/");
	  strcat(cprojection,cwidth);
	}
/* Azimuthal Equidistant Projection */
        else if( lazim_equidist ){
/* Choose the projection center as the first events station */
	  sprintf(ccenterlat,"%1.3f",stalat[0]);
	  sprintf(ccenterlon,"%1.3f",stalon[0]);
          strcat(cprojection,"E");
          strcat(cprojection,ccenterlon);
          strcat(cprojection,"/");
          strcat(cprojection,ccenterlat);
          strcat(cprojection,"/");
	  strcat(cprojection,cwidth);
	}
/* UTM Projection */
        else if( lutm ){
/* Determine the utm zone as (int) centerlat/6.0 */
	  sprintf(c_utm_zone,"%d",(int)( (minlat + maxlat)/12.0));
          strcat(cprojection,"U");
          strcat(cprojection,c_utm_zone);
          strcat(cprojection,"/");
	  strcat(cprojection,cwidth);
	}

/* buildup floating point bounds string */
        strcpy(cbounds," -R");
        strcat(cbounds,wrange);
        strcat(cbounds,"/");
        strcat(cbounds,erange);
        strcat(cbounds,"/");
        strcat(cbounds,srange);
        strcat(cbounds,"/");
        strcat(cbounds,nrange);

/* buildup integer bounds string */
        strcpy(cibounds," -R");
        strcat(cibounds,iwrange);
        strcat(cibounds,"/");
        strcat(cibounds,ierange);
        strcat(cibounds,"/");
        strcat(cibounds,isrange);
        strcat(cibounds,"/");
        strcat(cibounds,inrange);

/* cat the projection,bounds and addtional parameters to pscoast command */
	strcat(pscoast,cprojection);
	strcat(pscoast,cbounds);
        strcat(pscoast," -Ba");
        strcat(pscoast,clon_annot_spac);
        strcat(pscoast,"f");
        strcat(pscoast,clon_tick_spac);
        strcat(pscoast,"/a");
        strcat(pscoast,clat_annot_spac);
        strcat(pscoast,"f");
        strcat(pscoast,clat_tick_spac);
	if( lmercator || lequidistant || lrobinson || llambert || lutm) {
	  if ( deltalat > deltalatbig && deltalon > deltalonbig ) {
	    strcat(pscoast," -N1 -W -Dc -A");
	  }else{ 
	    strcat(pscoast," -N1 -N2 -W -Dl -A");
	  }
	  strcat(pscoast,carea);
	  /* strcat(pscoast," -G250/250/200 -S220/240/250 -Lf");*/
	  /* strcat(pscoast," -G250/250/200 -Lf");*/
	  if ( ltopo ) {
	    strcat(pscoast," -Lf");
	  }else{
	    strcat(pscoast," -G250/250/200 -Lf");
	  }

	  strcat(pscoast,clonscale);
	  strcat(pscoast,"/"); 
	  strcat(pscoast,clatscale);
	  strcat(pscoast,"/");
	  strcat(pscoast,cslatscale);
	  strcat(pscoast,"/");
	  strcat(pscoast,clengthscale);
	}else if( lazim_equidist ){ 
	  strcat(pscoast," -N1 -W -Dc -A");
	  strcat(pscoast,carea);
	  strcat(pscoast," -G250/250/200 -S220/240/250");
	}
	if ( ltopo ) {
        strcat(pscoast," >> gmt.ps \n");   
	}else{
        strcat(pscoast," > gmt.ps \n");   
	}

/* Buildup the optional grd commands for topograph */
	if ( ltopo ) {
	  memset(grdcoms,' ',MAXSTRING);
	  strcpy(grdcoms, "grdraster 1 -Ggmttopo.grd -I5m");
	  strcat(grdcoms, cibounds);
	  strcat(grdcoms, "\n");

	  strcat(grdcoms, "grdfilter gmttopo.grd -D1 -Fb35 -N -V ");
	  strcat(grdcoms, "-Ggmttopofilt.grd \n");

	  strcat(grdcoms, "grdgradient gmttopofilt.grd -A315 ");
	  strcat(grdcoms, "-Ggmttopograd.grd -Nt -V \n");

	  strcat(grdcoms, "grdimage -K -X0.75 -Y1.0 -P ");
	  strcat(grdcoms, cprojection);
	  strcat(grdcoms, cbounds);
	  strcat(grdcoms, " gmttopofilt.grd -C${SACAUX}/ctables/gmt.cpt ");
	  strcat(grdcoms, " -Igmttopograd.grd -V > gmt.ps \n");
	}

/* Open the output file */
        if(( fp = fopen(scriptname,"w")) == NULL){
          *nerr = 102;
          goto L_9999;
	}
        fputs("#!/bin/csh -f\n#\n#\n",fp);
	if ( ltopo ) {
	  fputs(grdcoms, fp);
	  fputs(pscoast, fp);
	}else{
	  fputs(pscoast, fp);
	}

/* Add optional station names if requested */
	if (lstanames) {
	  memset(pstext, ' ', MAXSTRING);
	  strcpy(pstext, "pstext -K -O -D.15 ");
	  strcat(pstext,cprojection);
	  if ( lazim_equidist || ( deltalat > deltalatbig && deltalon > deltalonbig ) ) {
	    strcat(pstext," -R -St.1 -G0/0/0 <<eof >> gmt.ps \n");
	  }else{ 
	    strcat(pstext," -R -St.25 -G0/0/0 <<eof >> gmt.ps \n");
	  }	  
	  for( i=0; i<nch; i++ ){
	    if ( stalon[i] != latlon_undef && stalat[i] != latlon_undef ) {
	      if ( i == 0 || stalon[i] != stalon[i-1] || stalat[i] != stalat[i-1] ) {
		if (strncmp(staname[i], "-12345", 6)) { 
		  sprintf(staloc,"%1.3f %1.3f  10 0 0 BL  %s \n",stalon[i],stalat[i],staname[i]);
		  strcat(pstext,staloc);
		} 
	      }
	    }
	  }        
	  strcat(pstext,"eof\n");
	  fputs("#Add station location labels.  Input are intitude, ",fp);
	  fputs("latitude, and name.\n#\n",fp);
	  fputs(pstext, fp);
	}

/* Add station locations to map. */
        memset(psxy, ' ', MAXSTRING);
        if( neventlocs > 0 ){
	  strcpy(psxy, "psxy -K -O ");
	}else{
	  strcpy(psxy, "psxy -O ");
	}
	strcat(psxy,cprojection);
	if ( lazim_equidist || ( deltalat > deltalatbig && deltalon > deltalonbig ) ) {
	  strcat(psxy," -R -St.1 -G0/0/0 <<eof >> gmt.ps \n");
	}else{ 
	  strcat(psxy," -R -St.25 -G0/0/0 <<eof >> gmt.ps \n");
	}	  

        for( i=0; i<nch; i++ ){
	  if ( stalon[i] != latlon_undef && stalat[i] != latlon_undef ) {
	    if ( i == 0 || stalon[i] != stalon[i-1] || stalat[i] != stalat[i-1] ) {
	      sprintf(staloc,"%1.3f %1.3f \n",stalon[i],stalat[i]);
	      strcat(psxy,staloc);
	    }
	  }
        }        

        strcat(psxy,"eof\n");
        fputs("#Add station locations.  Input are intitude latitude.\n#\n",fp);
        fputs(psxy, fp);

/* Draw a great circle path aint the back azimuth if the */
/* information is available.                              */

        getbbv("BBFK_BAZIM", bazim, nerr, 10, 9);
        if( *nerr == 0 ){
          if( !lrefset ){
/* Use first station in memory as the reference station. */
            getfil(1, FALSE, &nlen, &ndxy, &ndxx, nerr );
            irefsta = 1;
	  }else{
/* Get station location of the specified reference station. */
            for( irefsta = 1; irefsta <= nch; irefsta++ ){
              getfil( irefsta, FALSE, &nlen, &ndxy, &ndxx, nerr );
              if( !strcmp(kuser1,kstnm) )break;
	    }
	  }
	  memset(psxy, ' ', MAXSTRING);
	  strcpy(psxy, "psxy -K -O ");
	  strcat(psxy,cprojection);
	  strcat(psxy, " -R -SV0.025/.2/.2 -G125 <<eof >> gmt.ps \n");
          sprintf(staloc,"%1.3f %1.3f ",stalon[irefsta-1],stalat[irefsta-1]);
          strcat(psxy,staloc);
          strcat(psxy,bazim);
          strcat(psxy," 1 \n");
          strcat(psxy,"eof\n");
          fputs("#Draw a great circle path aint the back azimuth.\n",fp);
          fputs("#Input are intitude latitude azimuth size in inches.\n#\n",fp);
          fputs(psxy, fp);
/* Draw a line from reference station to event. */
          memset(psxy, ' ', MAXSTRING);
	  if( neventlocs > 0 ){
	    strcpy(psxy, "psxy -K -O ");
	  }else{
	    strcpy(psxy, "psxy -O ");
	  }
	  strcat(psxy,cprojection);
	  strcat(psxy," -R <<eof >> gmt.ps \n");
          strcat(psxy,staloc);
          strcat(psxy,"\n");
          sprintf(evloc,"%1.3f %1.3f \n",evlon[irefsta-1],evlat[irefsta-1]);
          strcat(psxy,evloc);
          strcat(psxy,"eof\n");
          fputs("#Draw a line from reference station to event.\n",fp);
          fputs(psxy,fp);
	}else{
          *nerr = 0;
	}

/* Put a legend on the map if doing size/mag/resid */
	    if ( cmgem.ltitl ) {
	      for (i=0; i<cmgem.ntitl; i++)
		ctitle[i]=kmgem.ktitl[i];
	      ctitle[cmgem.ntitl] = '\0';
	      memset(psxy, ' ', MAXSTRING);
	      strcpy(psxy, "pstext -K -O ");
	      strcat(psxy,cprojection);
	      strcat(psxy, " -R -N <<eof >> gmt.ps \n");
/*	      sprintf(evloc,"%1.3f %1.3f 18 0 5 5 %s \n",minlon,1.08*maxlat,kmgem.ktitl);*/
/*	      sprintf(evloc,"%1.3f %1.3f 18 0 5 2 %s \n",(minlon+maxlon)/2.0,1.07*maxlat,ctitle); */
	      /* fixed y position of title. maf 970306 */
	      sprintf ( evloc , "%1.3f %1.3f 18 0 5 2 %s \n" , ( minlon + maxlon ) / 2.0 , 
		maxlat + ( maxlat - minlat ) * 0.09 , ctitle ) ;
	      strcat(psxy,evloc);   
	      strcat(psxy,"eof\n");
	      fputs("#Add Title.\n#\n",fp); 
	      fputs(psxy, fp);
	    }

/* Add event locations to map if any exist. */
        if( neventlocs > 0 ){

/* Put a legend on the map if doing size/mag/resid */
	    if (lsize) {
	      memset(psxy, ' ', MAXSTRING);
/* legend symbols */
	      strcpy(psxy, "psxy -K -O ");
	      strcat(psxy,cprojection);
	      strcat(psxy, " -R -Sc -N -W10/0/0/0 <<eof >> gmt.ps \n");
	      sprintf(evloc,"%1.3f %1.3f %1.3f \n",1.17*maxlon,0.90*maxlat,minscale);
	      strcat(psxy,evloc);   
	      sprintf(evloc,"%1.3f %1.3f %1.3f \n",1.17*maxlon,1.00*maxlat,maxscale);
	      strcat(psxy,evloc);   
	      strcat(psxy,"eof\n");
/* legend values */
	      strcat(psxy, "pstext -K -O ");
	      strcat(psxy,cprojection);
	      strcat(psxy, " -R -N <<eof >> gmt.ps \n");
	      sprintf(evloc,"%1.3f %1.3f 14 0 5 5 %1.3f \n",1.24*maxlon,0.90*maxlat,minsize);
	      strcat(psxy,evloc);   
	      sprintf(evloc,"%1.3f %1.3f 14 0 5 5 %1.3f \n",1.24*maxlon,1.00*maxlat,maxsize);
	      strcat(psxy,evloc);   
	      strcat(psxy,"eof\n");
	      fputs("#Add Legend for size, magnitude, residuals.\n#\n",fp); 
	      fputs(psxy, fp);
	    }else if (lresid){
/* legend symbols */
/* neg resid legend */
	      if (nneg > 0) {
	      memset(psxy, ' ', MAXSTRING);
	      strcpy(psxy, "psxy -K -O ");
	      strcat(psxy,cprojection);
     /* Draw minus signs for negative resids (obs-predicted)*/
	      /* strcat(psxy, " -R -Sv0/0/0 -N -W10/0/0/0 <<eof >> gmt.ps \n"); */
	      /* sprintf(evloc,"%1.3f %1.3f 0.0 %1.3f \n",1.15*maxlon,0.80*maxlat,slope*minneg+intercept);*/
	      /* strcat(psxy,evloc);   */
	      /* sprintf(evloc,"%1.3f %1.3f 0.0 %1.3f \n",1.135*maxlon,0.70*maxlat,slope*maxneg+intercept);*/
	      /* strcat(psxy,evloc);   */
     /* Draw circles signs for negative resids (obs-predicted)*/
	      strcat(psxy, " -R -Sc -N -W10/0/0/0 <<eof >> gmt.ps \n");
	      sprintf(evloc,"%1.3f %1.3f %1.3f \n",1.17*maxlon,0.80*maxlat,slope*minneg+intercept);
	      strcat(psxy,evloc);   
	      sprintf(evloc,"%1.3f %1.3f %1.3f \n",1.17*maxlon,0.70*maxlat,slope*maxneg+intercept);
	      strcat(psxy,evloc);   
	      strcat(psxy,"eof\n");
	      fputs("#Add Legend for size, magnitude, residuals.\n#\n",fp); 
	      fputs(psxy, fp);
	      }
/* positive resid legend */ 
	      if (npos > 0) {
	      memset(psxy, ' ', MAXSTRING);
	      strcpy(psxy, "psxy -K -O ");
	      strcat(psxy,cprojection);
	      strcat(psxy, " -R -Sx -N -W10/255/0/0 <<eof >> gmt.ps \n");
	      sprintf(evloc,"%1.3f %1.3f %1.3f \n",1.17*maxlon,0.90*maxlat,slope*minpos+intercept);
	      strcat(psxy,evloc);   
	      sprintf(evloc,"%1.3f %1.3f %1.3f \n",1.17*maxlon,1.00*maxlat,slope*maxpos+intercept);
	      strcat(psxy,evloc);   
	      strcat(psxy,"eof\n");
	      fputs("#Add Legend for size, magnitude, residuals.\n#\n",fp); 
	      fputs(psxy, fp);
	      }
/* legend values */
	      memset(psxy, ' ', MAXSTRING);
	      strcpy(psxy, "pstext -K -O ");
	      strcat(psxy,cprojection);
	      strcat(psxy, " -R -N <<eof >> gmt.ps \n");
	      /* minimum and maximum for legend */
	      if (nneg > 0) {
	      sprintf(evloc,"%1.3f %1.3f 14 0 5 5 %1.3f \n",1.24*maxlon,0.70*maxlat,-maxneg);
	      strcat(psxy,evloc);   
	      sprintf(evloc,"%1.3f %1.3f 14 0 5 5 %1.3f \n",1.24*maxlon,0.80*maxlat,-minneg);
	      strcat(psxy,evloc);   
	      }
	      if (npos > 0) {
	      sprintf(evloc,"%1.3f %1.3f 14 0 5 5 %1.3f \n",1.24*maxlon,0.90*maxlat,minpos);
	      strcat(psxy,evloc);   
	      sprintf(evloc,"%1.3f %1.3f 14 0 5 5 %1.3f \n",1.24*maxlon,1.00*maxlat,maxpos);
	      strcat(psxy,evloc);   
	      }
	      /* Put the mean value out if removing the mean */ 
	      if ( lrmean_resid ) {
		sprintf(evloc,"%1.3f %1.3f 14 0 5 5 Mean = %1.3f \n",1.11*maxlon,0.60*maxlat,meansize);
		strcat(psxy,evloc);   
	      }
	      strcat(psxy,"eof\n");
	      fputs("#Add Legend for size, magnitude, residuals.\n#\n",fp); 
	      fputs(psxy, fp);
	    }
	  

/* do event symbols/size/magnitude/positive residuals first */
          memset(psxy, ' ', MAXSTRING);
	  /* put the initial command line out */
	  if( lsize ){
	    strcpy(psxy, "psxy -O ");
	    strcat(psxy,cprojection);
	    strcat(psxy, " -R -Sc -W10/0/0/0 <<eof >> gmt.ps \n");
	  }else if( lresid ){
	    strcpy(psxy, "psxy -K -O ");
	    strcat(psxy,cprojection);
	    strcat(psxy, " -R -Sx -W10/255/0/0 <<eof >> gmt.ps \n");
	  }else{
	    strcpy(psxy, "psxy -O ");
	    strcat(psxy,cprojection);
	    if ( lazim_equidist || ( deltalat > deltalatbig && deltalon > deltalonbig ) ) {
	      strcat(psxy," -R -Sc.1 -W10/0/0/0 <<eof >> gmt.ps \n");
	    }else{ 
	      strcat(psxy," -R -Sc.25 -W10/0/0/0 <<eof >> gmt.ps \n");
	    }	  
	  }

	  /* put the xy locations and sizes out */
          for( i=0; i<nch; i++) {
	    if ( i == 0 || evlon[i] != evlon[i-1] || evlat[i] != evlat[i-1] ) {
	      if( evlon[i] != latlon_undef ){
		if( lsize ){
		  sprintf(evloc,"%1.3f %1.3f %1.3f \n",evlon[i],evlat[i],size[i]);
		  strcat(psxy,evloc);   
		}else if( lresid ){
		  if (size[i] != size_undef && size[i] >= 0.0 ) {
		    sprintf(evloc,"%1.3f %1.3f %1.3f \n",evlon[i],evlat[i],fabs(size[i]) ); 
		    strcat(psxy,evloc);   
		  }
		}else{
		  sprintf(evloc,"%1.3f %1.3f \n",evlon[i],evlat[i]);
		  strcat(psxy,evloc);   
		}
	      }
	    }
	  }

          strcat(psxy,"eof\n");
	  if (lresid) {
	    fputs("#Add event locations with positive residuals.\n#Input are intitude latitude.\n#\n",fp); 
	  }else{
	    fputs("#Add event locations.\n#Input are intitude latitude.\n#\n",fp); 
	  }
          fputs(psxy, fp);

/* do negative residuals if any exist */
	  if( lresid ){
	    memset(psxy, ' ', MAXSTRING);
	    strcpy(psxy, "psxy -O ");
	    strcat(psxy,cprojection);

	  /* put the initial command line out */
	    /* strcat(psxy, " -R -Sv0/0/0 -W10/0/0/0 <<eof >> gmt.ps \n");*/
	    strcat(psxy, " -R -Sc -W10/0/0/0 <<eof >> gmt.ps \n");

	  /* put the xy locations and sizes out */
	    for( i=0; i<nch; i++) {
	      if ( i == 0 || evlon[i] != evlon[i-1] || evlat[i] != evlat[i-1] ) {
		if( evlon[i] != latlon_undef && size[i] != size_undef && size[i] < 0.0 ) {
		  /* for minus signs */ 
		  /* sprintf(evloc,"%1.3f %1.3f 0.0 %1.3f \n",evlon[i],evlat[i],fabs(size[i]) );*/
		  sprintf(evloc,"%1.3f %1.3f %1.3f \n",evlon[i],evlat[i],fabs(size[i]) );
		  strcat(psxy,evloc);   
		}
	      }
	    }

	    strcat(psxy,"eof\n");
	    fputs("#Add event locations with negative residuals.\n#Input are intitude latitude.\n#\n",fp); 
	    fputs(psxy, fp);
	  }
        }

        fclose(fp);

        strcpy(syscommand, "chmod +x ");
        strcat(syscommand, scriptname);
        system(syscommand);
        system(scriptname);

/* Show the map. */
/* Get the user's postscript viewer preference. */
        if((viewer = getenv("SACPSVIEWER")) != NULL){
          strcpy(syscommand,viewer);
	}else{
/* Use the default. */
	  /*          strcpy(syscommand,"pageview");*/
	  strcpy(syscommand,"ghostview -magstep -2");
	}

        strcat(syscommand," ");
        strcat(syscommand,"gmt.ps &");
        system(syscommand);

L_9999:
/* Clean up memory before finishing */
	for (idx = 0; idx < number_event_station; idx++ ) {
	  free (staname[idx]);
	}
	free (staname);

        if( buf != NULL) free(buf);
	return;

} /* end of function */

