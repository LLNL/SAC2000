/*******************************************************************************
** PURPOSE:
*    To set the color table.
*
** INPUT ARGUMENTS:
*    win_num:      Window number to set table for. (Pointer)
*    nentry:       Number of entries in color table. (Pointer)
*    red:          Array containing full of reds.  Range:  [0.0,1.0]
*    green:        Array containing full of greens.  Range: [0.0,1.0]
*    blue:         Array containing full of blues.  Range: [0.0,1.0]
*
** OUTPUT ARGUMENTS:
*    nentry:       Actual number of entries in color table. (Pointer)
*                  On output, nentry must be a multiple of two.
** GLOBAL INPUT:
*    gd4com:  win_attr4.pw
*
** SUBROUTINES CALLED:
*    pw_setcmsname, pw_putcolormap
*
** LOCAL VARIABLES:
*    name:      Name of color table. (Pointer)
*    uc_red:    Scaled red array.
*    uc_green:  Scaled green array.
*    uc_blue:   Scaled blue array.
*    full:      'Fullest color' -- Colors are in range [0, full].
*    i:         Loop counter.
*
** LIMITATIONS:
*    - Maximum of 256 colors.
******************************************************************************/

void setctable4(int* win_num, int nentry, float* red, float* green, float* blue)
{ 

}

/*******************************************************************************
** MODIFICATION HISTORY:
*    871006:  Added check that color table length is multiple of 2.
*             Changed color table name to depend on the window number and
*             color table length.
*    870422:  Saved number of colors.
*    870316:  Changed calling sequence.
*    870120:  Allowed for multiple windows.
*    861205:  Original Version
*******************************************************************************/
