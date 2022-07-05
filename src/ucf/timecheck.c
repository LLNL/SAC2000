#include <stdio.h>
int isleap( int yr);

void timecheck( short *year, short *day, short *hour, short *min,
                short *sec, short *ms )
{
      while( *ms > 999 ) {
         *ms -= 1000 ;
         (*sec)++ ;
      }
      while( *ms < 0 ) {
         *ms += 1000 ;
         (*sec)-- ;
      }

      while( *sec > 59 ) {
         *sec -= 60 ;
         (*min)++ ;
      }
      while( *sec < 0 ) {
         *sec += 60 ;
         (*min)-- ;
      }

      while( *min > 59 ) {
         *min -= 60 ;
         (*hour)++ ;
      }
      while( *min < 0 ) {
         *min += 60 ;
         (*hour)-- ;
      }

      while( *hour > 23 ) {
         *hour -= 24 ;
         (*day)++ ;
      }
      while( *hour < 0 ) {
         *hour += 24 ;
         (*day)-- ;
      }

      while( *day > ( isleap( *year ) ? 366 : 365 ) ) {
         (*year)++ ;
         *day -= isleap( *year ) ? 366 : 365 ;
      }
      while( *day < 1 ) {
         (*year)-- ;
         *day += isleap( *year ) ? 366 : 365 ;
      }
} /* end timecheck() */
