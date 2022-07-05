/*
CDOC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
 *** Function:	zsleep_(pmsec)					   D. Trimmer
 
 *** Purpose:	Sleep specified number of milliseconds
 
 *** Inputs:	pmsec	pointer to int containing # milliseconds to
 
 *** Returns:	none
 
 *** Notes:	This function is to be called by a FORTRAN routine.  The
		'_' is appended to the name for compatibility with FORTRAN.
 
		BSD 4.2 UNIX offers a sleep function in increments of one
		second, and is only accurate to +/- 1 second.  Therefore,
		this software sleep was written to provide smaller sleeps.
 
                It is used by the graphics library for delays when
                performing certain graphics operations to the terminal.

 *** Bugs:	If the system is busy, the actual sleep may be much longer
		than that specified, since it will take longer to complete
		the loop.  The loop needs to be tuned for machines with
		different speeds.
 
 *** History:	07/26/84	Under development--D. Trimmer
		07/26/84	Tested--D. Trimmer
		12/19/84        Tuned for ROS 3.2/RIDGE 32--D. Trimmer
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CEND
*/
 

void zsleep(pmsec)
int *pmsec;
 
{
	int i;		/* index */
	int j;		/* index */
 
	for (i=0;i<*pmsec;++i)
		for (j=0;j<364;++j);		/* 1 millisecond loop */
	return;
}
 
