/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zinfo_(atime,adate,compid,atlen,adlen,colen)	     D. Trimmer
 
 *** Purpose:	Returns local time and date and computer id.
 
 *** Inputs:	atime	a character array to receive the local time as
			hh:mm:ss
		adate	a character array to receive the local date as
			mm/dd/yy
		compid	a character array to receive the computer id
			(hard coded as 'UNIX').
		atlen	length of atime (required for F 77 compatibility).
		adlen	length of adate (required for F 77 compatibility).
		colen	length of compid (required for F 77 compatibility).
 
 *** Returns:	see atime,adate, and pcolen
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
 *** History:	07/24/84	Under development--D. Trimmer
		07/24/84	Tested--D. Trimmer
		12/19/84	Modified for ROS 3.2/RIDGE 32--D. Trimmer 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 
#include <time.h>
#include <string.h>
#include <stdio.h>
 

void zinfo(atime,atlen,adate,adlen,compid,colen)
char atime[];		/* character array to receive local time */
int atlen;		/* for F77 compatibility */
char adate[];		/* character array to receive local date */
int adlen;		/* for F77 compatibliity */
char compid[];		/* character array to receive computer id */
int colen;		/* for F77 compatibility */
 
{
	time_t ztime;
	struct tm *p;
 
	time (&ztime);
	p=localtime(&ztime);
 
	sprintf(atime,"%2.2d:%2.2d:%2.2d",p->tm_hour,p->tm_min,p->tm_sec);
	sprintf(adate,"%2.2d/%2.2d/%2.2d",p->tm_mon,p->tm_mday,p->tm_year);
	sprintf(compid,"%s","UNIX");
	
	return;
}
 
