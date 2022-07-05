#define DELETEBIT       1LU<<31
#define READONLYBIT     1<<30
#define INDIRECTBIT     1<<29
#define SHAREDBIT       1<<28
#define RESERVEDBIT     1<<27
#define VALUETYPEBITS    037
#define VALUETYPESHIFT   22
#define APPLICATIONBIT1 1<<21
#define APPLICATIONBIT2 1<<20
#define NAMELENGTHBITS  07
#define NAMELENGTHSHIFT	17
#define LONGVALUEBIT    1<<16
#define VALUELENGTHBITS	0177777


void decodevdesc( descriptor, deleteflag, readonlyflag, indirectflag, 
		   sharedflag, reservedflag, applflag1, applflag2,
                   valuetype, desclength, namelength, valuelength )
int *descriptor, *deleteflag, *readonlyflag, *indirectflag, 
     *sharedflag, *reservedflag, *applflag1, *applflag2, 
     *valuetype, *namelength, *valuelength, *desclength;
/*
*=====================================================================
* PURPOSE: To decode the vars descriptor into individual fields.
*=====================================================================
* INPUT ARGUMENTS:
*   *descriptor:    pointer to 32 vars descriptor.
*=====================================================================
* OUTPUT ARGUMENTS:
*   *deleteflag:    logical (int) TRUE if delete bit is set.
*   *readonlyflag:  logical (int) TRUE if readonly bit is set.
*   *indirectflag:  logical (int) TRUE if indirect bit is set.
*   *reservedflag:  logical (int) TRUE if reserved bit is set.
*   *sharedflag:    logical (int) TRUE if shared bit is set.
*   *applflag1:     logical (int) TRUE if first application bit is set.
*   *applflag2:     logical (int) TRUE if second application bit is set.
*   *valuetype:     integer contains valuetype field.
*   *desclength:    integer returns length of descriptor.
*   *namelength:    integer contains namelength field.
*   *valuelength:   integer contains valuelength field.
*=====================================================================
* MODULE/LEVEL: vars/4
*=====================================================================
* MODIFICATION HISTORY:
*    920320:  Portability to IBM RISC 6000 via precompiler flag.
*    881104:  Modifications due to further changes in descriptor format.
*    870208:  Modifications due to change in vars descriptor format.
*    861218:  Original version.
*=====================================================================
* DOCUMENTED/REVIEWED:  881104
*=====================================================================
*/
{
register int  descrip;

   descrip = *descriptor;
   *deleteflag = ( descrip & DELETEBIT ) != 0 ;
   *readonlyflag = ( descrip & READONLYBIT ) != 0 ;
   *indirectflag = ( descrip & INDIRECTBIT ) != 0 ;
   *sharedflag = ( descrip & SHAREDBIT ) != 0 ;
   *reservedflag = ( descrip & RESERVEDBIT ) != 0 ;
   *valuetype = ( descrip >> VALUETYPESHIFT ) & VALUETYPEBITS ;
   *applflag1 = ( descrip & APPLICATIONBIT1 ) != 0 ;
   *applflag2 = ( descrip & APPLICATIONBIT2 ) != 0 ;
   *namelength = ( ( descrip >> NAMELENGTHSHIFT ) & NAMELENGTHBITS ) + 1L ;
   if ( descrip & LONGVALUEBIT ) {
      *valuelength =  *++descriptor + 1L  ;
      *desclength = 2L ;
      }
    else {
      *valuelength = ( descrip & VALUELENGTHBITS ) + 1L  ;
      *desclength = 1L ;
      }
}

