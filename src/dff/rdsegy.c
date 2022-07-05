#include <stdio.h>
#include <math.h> 
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "hdr.h"
#include "dfm.h"
#include "mem.h"
#include "segy.h"
#include "segy2sac.h"

int    isleap( int year);

int DoTime( short *year, short *day, short *hour, short *min, short *sec,
             short *ms, short *ay,   short *ad,  short *ah,   short *am,
             short *as,  short *ams, float *reference, float *alternate,
             char* kfile ) ;




/*********************************************************************/
/* convlat(&xlat,&xlon)                                 */
/*********************************************************************/
/*
 * Subroutine to convert x-y coordinates to lat int assume a spherical earth
 * simple model xlat is y coordinate on input returned as latitude xlon is x
 * coordinate on input returned as intitude
 *
 * lat positive to north int positive to east
 */
void convlat(xlat, xlon)
        float          *xlat, *xlon;
{
        double          d, D, L, mpd, tpi;
        int             iflg;

        iflg = 1;
        if (*xlon < 0.)
                iflg = -1;

        tpi = 3.1415927;
        mpd = (6080.2 * 60 * .9144) / (3);      /* meters per degree lat */
        d = (double) ((*xlat * *xlat) + (*xlon * *xlon));
        d = sqrt(d);
        D = d / mpd;            /* total distance */

        *xlat = *xlat / mpd;    /* get latitude */

        d = cos(D * tpi / 180.);
        L = cos(*xlat * tpi / 180.);

        D = acos(d / L);        /* get intitude */
        D = (double) iflg *D;   /* set direction */
        *xlon = (float) (D * 180. / tpi);
        return;
}




void rdsegy ( int idfl , char *kfile , int *nlen ,
	      int *ndx1 , int *ndx2 , int *nerr )
{
    /* declare variables. */
    unsigned char *cptr ;
    short short_hold ;
    int num_sam , sam_rate , int_hold , idx ;
    int llon, llat , IBflag = TRUE , check ;
    float xs, xlat, xlon, elat, elon, useScale ;
    float *ar ;

    FILE *fpin = NULL ;
    struct SegyHead trace ;

    /* Read seqy file */
    /* open the input file */
    if ( !( fpin = fopen ( kfile, "rb" ) ) ) {
	/* error handling */
        *nerr = 101 ;
        setmsg( "ERROR" , *nerr ) ;
        apcmsg( kfile , strlen( kfile ) ) ;
        outmsg() ;
        return ;
    }

    /* start reading the data file   */
    /* first read the SEGY trace header    */

    if ( fread ( (void *) ( &trace.lineSeq ) , 240 , 1 , fpin ) != 1 ) {
	/* error handling */
	fclose(fpin);
        *nerr = 114 ;
        setmsg( "ERROR" , *nerr ) ;
        apcmsg( kfile , strlen( kfile ) ) ;
        outmsg() ;
        return ;
    }
    if ( trace.sampleLength == 32767 || trace.sampleLength == 0 )
	num_sam = trace.num_samps == 0 ? -12345 : trace.num_samps ;
    else
	num_sam = trace.sampleLength;


    /* Check number of samples */
    if (num_sam < 0) {
	/* error handling */
	fclose(fpin);
        *nerr = 1340 ;
        setmsg( "ERROR" , *nerr ) ;
        apcmsg( kfile , strlen( kfile ) ) ;
        outmsg() ;
        return ;
    }

    putcl ( kmdfm.kdfl , MAXCHARS , kfile , strlen( kfile ) , nerr ) ;
    if ( *nerr ) {
	/* error handling */
        fclose(fpin);
        *nerr = 1387 ;
        setmsg( "ERROR" , *nerr ) ;
        outmsg() ;
        return ;
    }

    allamb ( &cmmem , MHDR , &Ndxhdr[ idfl ] , nerr ) ;
    if ( *nerr ) {
	/* error handling */
        fclose( fpin ) ;
        return ;
    }

    Nlndta[ idfl ] = num_sam ;

    /* Null the header */
    for( idx = 0 ; idx < MFHDR ; idx++ )
        cmhdr.fhdr[ idx ] = cmhdr.fundef ;
    for( idx = 0 ; idx < MNHDR ; idx++ )
        cmhdr.nhdr[ idx ] = cmhdr.nundef ;
    for( idx = 0 ; idx < MIHDR ; idx++ )
        cmhdr.ihdr[ idx ] = cmhdr.iundef ;
    for( idx = 0 ; idx < MKHDR ; idx ++ )
	strcpy( kmhdr.khdr[ idx ] , kmhdr.kundef ) ;
    strcpy( kevnm , "-12345           " ) ;

    *iftype = ITIME ;
    *nvhdr  = 6 ;
    *npts   = num_sam ;
    

    allamb ( &cmmem , num_sam , &cmdfm.ndxdta[ idfl - 1 ][ 0 ] , nerr ) ;
    if ( *nerr ) {
	/* error handling */
        fclose( fpin ) ;
        return ;
    }
    Ncomp[ idfl ] = 1 ;
    ar = cmmem.sacmem[ cmdfm.ndxdta[ idfl - 1 ][ 0 ] ] ;

    *dist = trace.sourceToRecDist == 0 ? -12345. : trace.sourceToRecDist ;

    if ( trace.deltaSample == 1 || trace.deltaSample == 0 ) {
	sam_rate = trace.samp_rate;
    } else {
	sam_rate = trace.deltaSample;
    }
    *delta = (float) sam_rate / 1000000.0 ;

    /* get scale factor for data  */
    if ( trace.scale_fac == 0 ) {
	*scale = 1.0 ;
    }
    else 
	*scale = trace.scale_fac /
		(trace.gainConst == 0 ? 1.0 : trace.gainConst ) ;

    if( cmdfm.lscale ) {
	useScale = *scale ;
	*scale = 1.0 ;
    }
    else {
	useScale = 1.0 ;
    }

    *depmax = trace.max == 0 ? -12345. : trace.max * useScale ;
    *depmin = trace.min == 0 ? -12345. : trace.min * useScale ;


    /* now go get the trace data */
    if (trace.data_form == 1) {     /* 32 bit integer data */
	cptr = (unsigned char *) &int_hold;
	for (idx = 0; idx < num_sam; idx++) {
	    fread(cptr, 4, 1, fpin);
	    *(ar + idx) = ((float) int_hold) * useScale;
	}
    } else {        /* 16 bit integer data */
	cptr = (unsigned char *) &short_hold;
	for (idx = 0; idx < num_sam; idx++) {
	    fread(cptr, 2, 1, fpin);
	    *(ar + idx) = ((float) short_hold) * useScale;
	}
    }

    fclose(fpin);

    /* Fill SAC Header fields */

    /* time */
    if( cmdfm.iztype == IO ) {
        check = DoTime( &trace.trigyear,   &trace.trigday,    &trace.trighour,
                        &trace.trigminute, &trace.trigsecond, &trace.trigmills,
                        &trace.year,       &trace.day,        &trace.hour,
                        &trace.minute,     &trace.second,     &trace.m_secs,
                        origin,            begin,             kfile ) ;
        if( !check ) {
            check = DoTime( &trace.year,   &trace.day,        &trace.hour,
                        &trace.minute,     &trace.second,     &trace.m_secs,
                        &trace.trigyear,   &trace.trigday,    &trace.trighour,
                        &trace.trigminute, &trace.trigsecond, &trace.trigmills,
                        begin,             origin,            kfile ) ;
        }
    }
    else {
       check = DoTime(&trace.year,         &trace.day,        &trace.hour,
                      &trace.minute,       &trace.second,     &trace.m_secs,
                      &trace.trigyear,     &trace.trigday,    &trace.trighour,
                      &trace.trigminute,   &trace.trigsecond, &trace.trigmills,
                      begin,               origin,            kfile ) ;
       if( !check ) {
          check = DoTime( &trace.trigyear, &trace.trigday,    &trace.trighour,
                      &trace.trigminute,   &trace.trigsecond, &trace.trigmills,
                      &trace.year,         &trace.day,        &trace.hour,
                      &trace.minute,       &trace.second,     &trace.m_secs,
                      origin,              begin,             kfile ) ;

       }
    }

    if( !check ) {
        *nerr = 907 ;
        setmsg( "ERROR" , *nerr ) ;
        apcmsg( " Cannot determine BEGIN or ORIGIN time for " , 44 ) ;
        apcmsg( kfile , strlen( kfile ) ) ;
        apcmsg( "\nSkipping file." , 16) ;
        outmsg() ;
        clrmsg() ;

        relamb( Ndxhdr[ idfl ] ) ;
        relamb( cmdfm.ndxdta[ idfl - 1 ][ 0 ] ) ;

        return ;
    }

    *ennd = ( ( num_sam -1 ) * *delta ) + *begin ;

    sprintf(kcmpnm, "      %d", trace.channel_number);

    /* strings */
    strncpy( kstnm , trace.station_name, 6 );
    if( trace.channel_name[ 0 ] != '\0' &&
       strncmp( trace.channel_name , "    ", 4 ) )
        sprintf( kcmpnm , "%-4.4s    " , trace.channel_name ) ;
    if( trace.event_number )
        sprintf( kevnm , "%d" , trace.event_number ) ;

    /* now some coordinate information   */
    xs = (float) trace.coordScale;  /* scale factor  */
    if (xs < 0)
	xs = -1 / xs;
    llon = trace.recLongOrX;
    xlon = xs * (float) llon;
    llat = trace.recLatOrY;
    xlat = xs * (float) llat;
    elon = xs * (float) trace.sourceLongOrX ;
    elat = xs * (float) trace.sourceLatOrY ;
    if (trace.coordUnits == 1) {
	convlat(&xlat, &xlon);
        convlat(&elat, &elon);
    }
    else {
	xlat = xlat / 3600;
	xlon = xlon / 3600;
        elat = elat / 3600;
        elon = elon / 3600;
    }
    *stla = (float) xlat;
    *stlo = (float) xlon;
    *stel = (trace.recElevation == 0 ? -12345. : trace.recElevation ) ;
    *evla = elat ;
    *evlo = elon ;
    *evel = trace.sourceSurfaceElevation == 0 ? -12345. :
            trace.sourceSurfaceElevation ;
    *evdp = trace.sourceDepth == 0 ? -12345. : trace.sourceDepth ;

    *leven  = TRUE ;
    *lpspol = TRUE ;
    *lovrok = TRUE ;
    *lcalda = ( *evla == -12345. || *evlo == -12345. ||
                *stla == -12345. || *stlo == -12345. ) ? FALSE : TRUE ;

    /* now load them into SAC 
    putcl ( kmdfm.kdfl , MAXCHARS , kfile , strlen( kfile ) , nerr ) ;
    if ( *nerr ) {
        fclose(fpin);
        *nerr = 1387 ;
        setmsg( "ERROR" , *nerr ) ;
        outmsg() ;
        return ;
    } */

    putfil ( idfl , nerr ) ;

} /* end rdsegy */



int DoTime( short *year, short *day, short *hour, short * min, short *sec,
             short *ms, short *ay,   short *ad,  short *ah,   short * am,
             short *as,  short *ams, float *reference, float *alternate,
             char* kfile )
{
   short dumbY, dumbD, dumbH, dumbM, dumbS, dumbMS ;

   if( year == 0 || day == 0 )
      return FALSE ;

   *iztype = cmdfm.iztype ;
   *reference = 0.0;

   dumbY  = *year; /* the year */
   dumbD  = *day;  /* the julian day */
   dumbH  = *hour; /* the hour */
   dumbM  = *min;  /* the minute */
   dumbS  = *sec;  /* the second */
   dumbMS = *ms;   /* the milliseconds */

   timecheck( &dumbY, &dumbD, &dumbH, &dumbM,
              &dumbS,  &dumbMS ) ;

   *nzyear = dumbY;  /* the year */
   *nzjday = dumbD;  /* the julian day */
   *nzhour = dumbH;  /* the hour */
   *nzmin  = dumbM;  /* the minute */
   *nzsec  = dumbS;  /* the second */
   *nzmsec = dumbMS; /* the milliseconds */


   if( ay != 0 && ad != 0 ) {
      *alternate = ( (float)( *ams - *nzmsec ) ) / 1000.0 +
                   ( (float)( *as  - *nzsec  ) ) +
                   ( (float)( *am  - *nzmin  ) ) *   60.0 +
                   ( (float)( *ah  - *nzhour ) ) * 3600.0 +
                   ( (float)( *ad  - *nzjday ) ) * 3600.0 * 24.0 +
                   ( (float)( *ay  - *nzyear ) ) * 3600.0 * 24.0 *
                   (  isleap( *as  < *nzyear ? *ay : *nzyear ) ? 366. : 365. ) ;

/*      if( *iztype == IO )
         *alternate = -(*alternate) ; */
   }
   else {
      setmsg( "WARNING" , 907 ) ;

      if( *iztype == IB ) {
         apcmsg( " Cannot determine origin time for " , 35 ) ;
         apcmsg( kfile , strlen( kfile ) ) ;
         apcmsg( "\nSetting ORIGIN to BEGIN." , 26) ;
      }
      else {
         apcmsg( " Cannot determine begin time for " , 34 ) ;
         apcmsg( kfile , strlen( kfile ) ) ;
         apcmsg( "\nSetting BEGIN to ORIGIN." , 26) ;
      }
      outmsg() ;
      clrmsg() ;

      *alternate = 0.0 ;
   }

   return TRUE ;
}
