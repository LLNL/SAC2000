#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <unistd.h>
#include "complex.h"
#include "proto.h"
#include "dfm.h"
#include "mem.h"
#include "hdr.h"
/* #include "sac2segy.h" */
#include "segy.h"

void loadbuffer( char *buffer , SEGYHEAD *outHdr ) ;

void wrsegy( int idfl , char *filename, int *nerr )
{
   double data_roof , data_max ;

   float scale , value ;

   char buffer[ 245 ] ;

   int *idata , idx , bytesOdata , segyFile = -1 , check , nlcmem ;

   /* initialize segy header to zeros */
   SEGYHEAD outHdr = { 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
                       0, "\0\0\0\0\0\0", "\0\0\0\0\0\0\0\0", "\0\0\0\0",
                       0,   0,   0,   0,   0,   0,  
                       0,   0,   0,   0, 0.0,   0,   0,   0,   0,   0 } ;


   /* Check that the data is TIME SERIES data and is evenly spaced. */
   if( *iftype != ITIME || *leven != TRUE ) {
      setmsg( "WARNING" , 1306 ) ;
      apcmsg( "or spectral file.  File: " , 26 ) ;
      apcmsg( filename , strlen( filename ) ) ;
      apcmsg( " filenumber: " , 14 ) ;
      apimsg( idfl ) ;
      outmsg() ;
      clrmsg() ;
   }

   /* fill segy header with actual values. */

   /* do kevnm only if it completely numeric, and has less than 10 digits. */
   if( isdigit( kevnm[ 0 ] ) ||
              ( kevnm[ 0 ] == '-' && isdigit( kevnm[ 1 ] ) ) ) {
      char temp[ 18 ] ;
      strcpy( temp , kevnm ) ;
      for( idx = strlen( temp ) - 1 ; idx >= 0 ; idx-- ) {
         if( !isspace( temp[ idx ] ) ) {
            temp[ idx + 1 ] = '\0' ;
            break ;
         }
      }
      if( strlen( temp ) <= 9 ) {
         int lint = TRUE ;

         for( idx = 1 ; idx < 9 ; idx++ ) {
            if( !isdigit( temp[ idx ] ) ) {
               lint = FALSE ;
               break ;
            }
         }

         if( lint )
            outHdr.event_number = atoi( kevnm ) ;
      }
   }

   outHdr.traceID = 1 ;  /* there is only one trace per file */
   outHdr.sourceToRecDist = *dist == -12345. ? 0 : *dist + 0.5 ;
   outHdr.recElevation = *stel == -12345. ? 0 : *stel + (*stel < 0 ? -.5 : .5);
   outHdr.sourceSurfaceElevation = *evel == -12345. ? 0 :
                                   *evel + (*evel < 0 ? -.5 : .5 ) ;
   outHdr.sourceDepth = *evdp == -12345. ? 0 : *evdp + 0.5 ;
   outHdr.elevationScale = 1 ;
   outHdr.coordScale = 1 ;
   outHdr.sourceLongOrX = *evlo == -12345. ? 0 : ( *evlo * 3600 ) +
                                                 ( *evlo < 0 ? -.5 : .5);
   outHdr.sourceLatOrY = *evla == -12345. ? 0 :  ( *evla * 3600 ) +
                                                 ( *evla < 0 ? -.5 : .5);
   outHdr.recLongOrX = *stlo == -12345. ? 0 : ( *stlo * 3600.0 ) +
                                              ( *stlo < 0 ? -.5 : .5) ;
   outHdr.recLatOrY = *stla == -12345. ? 0 :  ( *stla * 3600.0 ) +
                                              ( *stla < 0 ? -.5 : .5) ;
   outHdr.coordUnits = 2 ;
   outHdr.sampleLength = *npts == -12345. ? 0 : ( *npts >= 32767 ? 32767 : *npts ) ;
   outHdr.num_samps = *npts == -12345. ? 0 : *npts ;
   outHdr.deltaSample = *delta == -12345. ? 0 : 
             ( *delta * 1000000 >= 32767 ? 1 : ( *delta * 1000000 ) + 0.5 ) ;
   outHdr.samp_rate = *delta == -12345. ? 0 : ( *delta * 1000000 ) + 0.5 ;

   outHdr.gainType = 1 ;
   outHdr.gainConst = 1 ;

   /* Set begin time of the trace */
   outHdr.year = *nzyear == -12345. ? 0 : *nzyear ;
   outHdr.day  = *nzjday == -12345. ? 0 : *nzjday ;
   outHdr.hour = *nzhour == -12345. ? 0 : *nzhour ;
   outHdr.minute = *nzmin == -12345. ? 0 : *nzmin ;
   if( *b != -12345. ) {
      outHdr.second = (int) (*b) ;
      outHdr.m_secs = ( ( *b - (float) outHdr.second ) * 1000. ) +
                        ( *b > 0 ? 0.5 : -0.5 ) ;
      outHdr.second += *nzsec == -12345. ? 0 : *nzsec ;
      outHdr.m_secs += *nzmsec == -12345. ? 0 : *nzmsec ;  

      timecheck( &outHdr.year,   &outHdr.day,    &outHdr.hour,
		 &outHdr.minute, &outHdr.second, &outHdr.m_secs ) ;
   } /* end if( *b != -12345. ) */
   else {
      outHdr.second = *nzsec == -12345. ? 0 : *nzsec ;
      outHdr.m_secs = *nzmsec == -12345. ? 0 : *nzmsec ;
   }

   /* Set origin time of the trace */
   outHdr.trigyear = *nzyear == -12345. ? 0 : *nzyear ;
   outHdr.trigday  = *nzjday == -12345. ? 0 : *nzjday ;
   outHdr.trighour = *nzhour == -12345. ? 0 : *nzhour ;
   outHdr.trigminute = *nzmin == -12345. ? 0 : *nzmin ;
   if( *o != -12345. ) {
      outHdr.trigsecond = (int) (*o) ;
      outHdr.trigmills = ( ( *o - (float) outHdr.trigsecond ) * 1000. ) +
                           ( *o > 0 ? 0.5 : -0.5 ) ;
      outHdr.trigsecond += *nzsec == -12345. ? 0 : *nzsec ;
      outHdr.trigmills += *nzmsec == -12345. ? 0 : *nzmsec ;

      timecheck( &outHdr.trigyear,   &outHdr.trigday,    &outHdr.trighour,
		 &outHdr.trigminute, &outHdr.trigsecond, &outHdr.trigmills ) ;

   } /* end if( *o != -12345. ) */
   else {
      outHdr.trigsecond = *nzsec == -12345. ? 0 : *nzsec ;
      outHdr.trigmills = *nzmsec == -12345. ? 0 : *nzmsec ;
   }
      


   if( strncmp( kstnm, "-12345", 6 ) ) strncpy( outHdr.station_name, kstnm, 6 );
   if( strncmp( kcmpnm, "-12345", 6 ) ) strncpy(outHdr.channel_name, kcmpnm, 4);
   outHdr.data_form = 1 ;
   outHdr.max = *depmax == -12345 ? 0 : *depmax + ( *depmax < 0. ? -.5 : .5 ) ;
   outHdr.min = *depmin == -12345 ? 0 : *depmin + ( *depmin < 0. ? -.5 : .5 ) ;

   /* Zero out the filename */
   for( idx = 0 ; idx < MCPFN ; idx++ )
      if( isspace( filename[ idx ] ) || filename[ idx ] == '\0' )
         break ;

   filename[ idx ] = '\0' ;

   /* Establish the data buffers */
   bytesOdata = *npts * sizeof( int ) ;
   idata = (int *) malloc( bytesOdata ) ;
   nlcmem = cmdfm.ndxdta[ idfl - 1 ][ 0 ] ;

   /* Figure out the scaling */
   data_roof = 2147483600.0 ; 
   data_max = 0.0 ;
   for( idx = 0 ; idx < *npts ; idx++ )
      if( fabs( cmmem.sacmem[ nlcmem ][ idx ] ) > data_max )
         data_max = fabs( cmmem.sacmem[ nlcmem ][ idx ] ) ;

   if( data_max != 0.0 )
      scale = data_roof / data_max ;
   else
      scale = 1.0 ;

   outHdr.scale_fac = ( 1.0 / scale ) ;

   outHdr.max *= scale ;
   outHdr.min *= scale ;

   /* Scale the data */
   for( idx = 0 ; idx < *npts ; idx++ ) {
      value = ( cmmem.sacmem[ nlcmem ][ idx ] * scale ) ;
      value += ( value < 0 ? -0.5 : 0.5 ) ;
      idata[ idx ] = value ;
   }


   /* Open a new file for segy formated header and trace. */
   segyFile = creat( filename , 0666 ) ;
   if( segyFile == -1 ) {
      /* error handling */
      *nerr = 102 ;
      setmsg( "ERROR" , *nerr ) ;
      apcmsg( filename , strlen( filename ) ) ;
      outmsg() ;
      free( idata ) ;
      return ;
   }


   /* Write the segy header to the file. */
   check = write( segyFile , (char *) ( &outHdr.lineSeq ) , 240 ) ;
   if( check != 240 ) {
      /* error handling */
      *nerr = 115 ;
      setmsg( "ERROR" , *nerr ) ;
      apcmsg( filename , strlen( filename ) ) ;
      apcmsg( ": Trouble writing SEGY header" , 30 ) ;
      outmsg() ;
      free( idata ) ;
      close( segyFile ) ;
      return ;
   }
   
   /* Write the segy data to the file. */
   check = write( segyFile , (char *)idata, bytesOdata ) ;
   if( check != bytesOdata ) {
      /* error handling */
      *nerr = 115 ;
      setmsg( "ERROR" , *nerr ) ;
      apcmsg( filename , strlen( filename ) ) ;
      apcmsg( ": Trouble writing SEGY data" , 28 ) ;
      outmsg() ;
   }

   /* Adios */
   free( idata ) ;
   close( segyFile ) ;
} /* end wrsegy() */

