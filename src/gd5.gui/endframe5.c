/*******************************************************************************
** PURPOSE:
*    To end plotting to a frame.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** SUBROUTINES CALLED:
*    flushbuffer5
*******************************************************************************/
void flushbuffer5(int* nerr);

void endframe5(int* nerr)
{
  int tmp = 0;
  flushbuffer5(&tmp);
  *nerr = tmp;
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    102695:  Original Version
*******************************************************************************/
