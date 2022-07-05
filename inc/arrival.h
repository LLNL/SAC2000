#ifndef __ARRIVAL_H_DEFINED
#define __ARRIVAL_H_DEFINED

struct arrecord
{
	char	 station[ 7 ] ;
	double	 epochtime ;
	int arid ;
	char	 channel[ 9 ] ,
		 iphase[ 9 ] ,
		 qual ,
		 auth[ 16 ] ;
} ;

struct arrivalInfo
{
	char station[7] ,
	     channel[9] ;

	int  orid ,
	     nArids ,	/* initialized to zero */
	    *arids ,	/* initialized to NULL */
	     nRecs ;	/* initialized to zero */

	float originCorrection ;
	double epochCorrection ;

	struct arrecord *recs ; /* initialized to NULL */
	
} ; /* end struct arrival */

#endif
