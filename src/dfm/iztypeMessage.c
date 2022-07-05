#include <stdio.h>
#include <string.h>
#include <strings.h>
#include "complex.h"
#include "proto.h"
#include "mach.h"
#include "msg.h"
#include "extfunc.h"


void iztypeMessage ( const int item , int iztype )
{
	/* if CHANGEHDR is changing the header field which
	   happens to be the reference time (eg. origin when
	   iztype == IO) then warn the user. */

	char field[ 3 ] ;

	field[ 0 ] = '\0' ;

	switch ( item ) {
	    case 6:	/* B */
		if ( iztype == IB )
		    strcpy ( field , "B" ) ;
		break ;
	    case 8:	/* O */
		if ( iztype == IO )
		    strcpy ( field , "O" ) ;
		break ;
	    case 9:	/* A */
		if ( iztype == IA )
		    strcpy ( field , "A" ) ;
		break ;
	    case 11:	/* T0 */
		if ( iztype == IT0 )
		    strcpy ( field , "T0" ) ;
		break ;
	    case 12:	/* T1*/
		if ( iztype == IT1 )
		    strcpy ( field , "T1" ) ;
		break ;
	    case13:	/* T2 */
		if ( iztype == IT2 )
		    strcpy ( field , "T2" ) ;
		break ;
	    case14:	/* T3 */
		if ( iztype == IT3 )
		    strcpy ( field , "T3" ) ;
		break ;
	    case15:	/* T4 */
		if ( iztype == IT4 )
		    strcpy ( field , "T4" ) ;
		break ;
	    case16:	/* T5 */
		if ( iztype == IT5 )
		    strcpy ( field , "T5" ) ;
		break ;
	    case17:	/* T6 */
		if ( iztype == IT6 )
		    strcpy ( field , "T6" ) ;
		break ;
	    case18:	/* T7 */
		if ( iztype == IT7 )
		    strcpy ( field , "T7" ) ;
		break ;
	    case19:	/* T8 */
		if ( iztype == IT8 )
		    strcpy ( field , "T8" ) ;
		break ;
	    case20:	/* T9 */
		if ( iztype == IT9 )
		    strcpy ( field , "T9" ) ;
		break ;
	} /* end switch */


	if ( field[ 0 ] ) {
	    setmsg ( "WARNING" , 1388 ) ;
	    apcmsg ( field , strlen ( field ) + 1 ) ;
	    outmsg () ;
	    clrmsg () ;
	}
}
