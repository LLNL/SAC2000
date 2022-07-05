#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "complexNDC.h"
#include "css_general.h"
int unscaled_response( char *dir, char *dfile, char *type, int units,
                       int log_flag, double start_fr, double end_fr,
                       int nfr, DCOMPLEX *response );


int ndcTransfer( char *dir, char *dfile, double dt, int nfr,
                 double *xre, double *xim, int *nerr )
{
   char *padding, cdfile[ 130 ] ;
   int i, result ;
   DCOMPLEX *response;
   double start_fr = 0.0 ;
   double end_fr = dt * (double)( nfr - 1 ) ;

   response = (DCOMPLEX *) malloc( nfr * sizeof( DCOMPLEX ) ) ;
   if( !response ){
      *nerr = 301 ;
      free(response);
      return 0;
   }

   strcpy( cdfile, dfile ) ;
   padding = strchr( cdfile, ' ' ) ;
   if( padding )
      *padding = '\0' ;
   /* "measured" or "theoretical" */
   result = unscaled_response ( dir, cdfile, "both", 'd', 0, 
                                start_fr, end_fr, nfr, response);
				    
   if( result != OK ){
      if( result == ERR ) *nerr = 2121 ;	/* system error */
      else if( result == ERR2 ) *nerr = 2122 ;	/* application error */
      else if( result == ERR3 ) *nerr = 2123 ;	/* SQL error */
      else *nerr = 2124 ;			/* unknown error */

      return 0;
   }
				    
   for( i = 0; i < nfr; ++i ){
      xre[ i ] = response[i].r;
      xim[ i ] = response[i].i;
   }
   free(response);
   return OK;
}
/* --------------------------------------------------- */
