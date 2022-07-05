/*******************************************************************************
** PURPOSE:
*    To change the auxiliary colors in the color table.
*
** INPUT ARGUMENTS:
*    icolortable: Requested auxiliary color table.  
** GLOBAL OUTPUT:
*
*
** SUBROUTINES CALLED:
*    XGetWindowAttributes,XAllocColorCells,XStoreColors
*
** LOCAL VARIABLES:
*    Status:  Status of color cells.
*    i:       Loop counter.
*    planes:  Plane mask.
*    pixels:  Pixel values.
*    full:    'Fullest color' -- Colors are in range [0, full].
*
** LIMITATIONS:
*    - Maximum of 256 colors.
*******************************************************************************
** MODIFICATION HISTORY:
*    940921:  Original Version
*******************************************************************************/

#define MGREY 1
#define MCOLOR 2
#include "../../inc/complex.h"
#include "../../inc/proto.h"
#include "../../inc/mach.h"
#include "../../inc/gd2.h"
#include "../../inc/sgfcolor.h"
#include "../../inc/gdm.h"

void changectable2(int nentry,int icolortable)
{ 

    int i;

    for (i = 0; i < cmgdm.npscimage; i++){
      if(icolortable == MCOLOR){
      sred[i] = sgfred[i];
      sgreen[i] = sgfgreen[i];
      sblue[i] = sgfblue[i];
      } else {
      sred[i] = 255-i;
      sgreen[i] = 255-i;
      sblue[i] = 255-i;
      }
    }
}

