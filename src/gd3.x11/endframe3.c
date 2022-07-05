/*******************************************************************************
** PURPOSE:
*    To end plotting to a frame.
*
** OUTPUT ARGUMENTS:
*    nerr:  Error flag.  Set to 0 if no error occurs. (Pointer)
*
** SUBROUTINES CALLED:
*    flushbuffer3
*******************************************************************************/
void flushbuffer3(int* nerr);

void endframe3(int* nerr)
{
  int tmp = 0;
  flushbuffer3(&tmp);
  *nerr = tmp;
}

/*******************************************************************************
** MODIFICATION HISTORY:
*    890608:  Used as-is from X10 version.  (kjm)
*    870223:  Original Version
*******************************************************************************/
