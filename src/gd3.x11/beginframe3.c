/*******************************************************************************
** PURPOSE:
*    To begin a frame to the XWindow device.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** SUBROUTINES CALLED:
*    erase3_
*******************************************************************************/
void erase3(void);
void dispatchevent3(int* nerr);

void beginframe3(int* nerr)
{
  *nerr = 0;
  
/* Erase the window */

  erase3();

/* Check for asynchronous events, in case of resize, etc. */

  dispatchevent3(nerr);

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    920320:  Portability to IBM.
*    890607:  Modified to run under X11 rather than X10.  (kjm)
*    870227:  Original Version
*******************************************************************************/
