/*******************************************************************************
 ** PURPOSE:
 *    To set the color table.
 *
 ** INPUT ARGUMENTS:
 *    win_num:      Window number to set table for. (Pointer)
 *    ncolors:      Number of entries in color table. (Pointer)
 *    red:          Array containing intensities of reds.  Range:  [0.0,1.0]
 *    green:        Array containing intensities of greens.  Range: [0.0,1.0]
 *    blue:         Array containing intensities of blues.  Range: [0.0,1.0]
 *
 ** GLOBAL OUTPUT:
 *    gd3.x11.h:  pixdef->(pixel, red, green, blue)
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
 *    030227:  Fix for colors on 24 bit displays.  Intelligence provided by
 *             Bob Herrmann.  Implemented by Mike Firpo (maf).
 *    910318:  Added check for black and white monitor (ammon)
 *    890608:  Modified to run under X11 rather than X10.  (kjm)
 *    870316:  Changed call sequence and arrangement of color table.
 *    870310:  Original Version
 *******************************************************************************/

#include <X11/Xlib.h>
#include <math.h>
#include "../../inc/gd3.x11.h"


static   XColor colors[244];

static double minDataValue = 0;
static double maxDataValue = 1;


int getColorCellCount(void)
{
    return 244;
}

void setDataRange( double minValue, double maxValue )
{
    minDataValue = minValue;
    maxDataValue = maxValue;
}


XColor getColorValue( double value )
{
   if( value < minDataValue ) return colors[0];
   if( value > maxDataValue ) return colors[63];
   double range = maxDataValue - minDataValue;
   if( range == 0 ) return colors[0];
   double rindex = round((value - minDataValue) / range * 243);
   return colors[(int)rindex]; 
}
XColor getColorByIndex( int idx )
{
    return colors[idx];
}


void setctable3(int* win_num, unsigned int nentry, float* red, float* green, float* blue) {
    Status status;
    int i, nent;
    int planes;
    int default_depth;
    int full = 65535.0;
    Colormap colormap;
    
    XColor exact_def ;        /* 030227, maf */
    
    /*  check for depth (black and white = 1; color != 1)  */
    default_depth = DefaultDepth(display3, DefaultScreen(display3));
    if(default_depth != 1) {
        
        /* Scale red, green, blue to be in range [0, full]. */
        /* Colors are passed in with the first color representing the background */
        /* color and the last color representing the foreground color.  These    */
        /* need to be switched around to conform to X standards -- pixel value   */
        /* 0 representing foreground (black) and pixel value of 1 representing   */
        /* background (white).                                                   */
        
        colormap = DefaultColormap(display3, DefaultScreen(display3));
        pixdef3[nentry - 1].pixel = BlackPixel(display3, DefaultScreen(display3));
        pixdef3[nentry - 1].red = full * red[nentry - 1];
        pixdef3[nentry - 1].green = full * green[nentry - 1];
        pixdef3[nentry - 1].blue = full * blue[nentry - 1];
        pixdef3[nentry - 1].flags = DoRed | DoGreen | DoBlue;
        
        pixdef3[0].pixel = WhitePixel(display3, DefaultScreen(display3));
        pixdef3[0].red = full * red[0];
        pixdef3[0].green = full * green[0];
        pixdef3[0].blue = full * blue[0];
        pixdef3[0].flags = DoRed | DoGreen | DoBlue;
        
        for (i = 1; i< nentry-1; i++) {
            pixdef3[i].pixel = BlackPixel(display3, DefaultScreen(display3));
            pixdef3[i].red   = full * red[i];
            pixdef3[i].green = full * green[i];
            pixdef3[i].blue  = full * blue[i];
            pixdef3[i].flags = DoRed | DoGreen | DoBlue;
            exact_def.red   = pixdef3[ i ].red   ;
            exact_def.green = pixdef3[ i ].green ;
            exact_def.blue  = pixdef3[ i ].blue  ;
            XAllocColor( display3, colormap, &exact_def ) ;
            pixdef3[ i ].pixel = exact_def.pixel ;
            pixdef3[ i ].red   = exact_def.red   ;
            pixdef3[ i ].green = exact_def.green ;
            pixdef3[ i ].blue  = exact_def.blue  ;
        }
        
        XFlush(display3);
    }
    else {
        /*  black and white */
        pixdef3[0].pixel = WhitePixel(display3, DefaultScreen(display3));
        nent = nentry - 1;
        for (i = 1; i< nent; i++)
            pixdef3[i].pixel = BlackPixel(display3, DefaultScreen(display3));
    }
}



void initializeColorMap(void) {
    Colormap default_cmap;
    static int isInitialized = 0;
    if( !isInitialized ){
        default_cmap = DefaultColormap(display3, DefaultScreen(display3));
	colors[0].red =     0; colors[0].green = 47031; colors[0].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[0]) );
	colors[1].red =     0; colors[1].green = 45232; colors[1].blue = 59367;
	XAllocColor(display3, default_cmap, &(colors[1]) );
	colors[2].red =     0; colors[2].green = 43433; colors[2].blue = 53456;
	XAllocColor(display3, default_cmap, &(colors[2]) );
	colors[3].red =     0; colors[3].green = 41891; colors[3].blue = 47545;
	XAllocColor(display3, default_cmap, &(colors[3]) );
	colors[4].red =     0; colors[4].green = 40092; colors[4].blue = 41634;
	XAllocColor(display3, default_cmap, &(colors[4]) );
	colors[5].red =     0; colors[5].green = 38550; colors[5].blue = 35723;
	XAllocColor(display3, default_cmap, &(colors[5]) );
	colors[6].red =     0; colors[6].green = 36751; colors[6].blue = 29555;
	XAllocColor(display3, default_cmap, &(colors[6]) );
	colors[7].red =     0; colors[7].green = 34952; colors[7].blue = 23644;
	XAllocColor(display3, default_cmap, &(colors[7]) );
	colors[8].red =     0; colors[8].green = 33410; colors[8].blue = 17733;
	XAllocColor(display3, default_cmap, &(colors[8]) );
	colors[9].red =     0; colors[9].green = 31611; colors[9].blue = 11822;
	XAllocColor(display3, default_cmap, &(colors[9]) );
	colors[10].red =     0; colors[10].green = 30069; colors[10].blue =  5911;
	XAllocColor(display3, default_cmap, &(colors[10]) );
	colors[11].red =     0; colors[11].green = 26728; colors[11].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[11]) );
	colors[12].red =     0; colors[12].green = 27756; colors[12].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[12]) );
	colors[13].red =     0; colors[13].green = 30069; colors[13].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[13]) );
	colors[14].red =     0; colors[14].green = 32382; colors[14].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[14]) );
	colors[15].red =     0; colors[15].green = 34695; colors[15].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[15]) );
	colors[16].red =     0; colors[16].green = 37008; colors[16].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[16]) );
	colors[17].red =     0; colors[17].green = 39321; colors[17].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[17]) );
	colors[18].red =     0; colors[18].green = 41634; colors[18].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[18]) );
	colors[19].red =     0; colors[19].green = 43947; colors[19].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[19]) );
	colors[20].red =     0; colors[20].green = 46260; colors[20].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[20]) );
	colors[21].red =     0; colors[21].green = 50886; colors[21].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[21]) );
	colors[22].red =     0; colors[22].green = 53199; colors[22].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[22]) );
	colors[23].red =     0; colors[23].green = 55512; colors[23].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[23]) );
	colors[24].red =     0; colors[24].green = 57825; colors[24].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[24]) );
	colors[25].red =     0; colors[25].green = 60138; colors[25].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[25]) );
	colors[26].red =     0; colors[26].green = 62451; colors[26].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[26]) );
	colors[27].red =     0; colors[27].green = 64764; colors[27].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[27]) );
	colors[28].red =  1542; colors[28].green = 63993; colors[28].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[28]) );
	colors[29].red =  3084; colors[29].green = 63222; colors[29].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[29]) );
	colors[30].red =  4626; colors[30].green = 62451; colors[30].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[30]) );
	colors[31].red =  6168; colors[31].green = 61680; colors[31].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[31]) );
	colors[32].red =  7710; colors[32].green = 60138; colors[32].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[32]) );
	colors[33].red =  9252; colors[33].green = 58596; colors[33].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[33]) );
	colors[34].red = 10794; colors[34].green = 57054; colors[34].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[34]) );
	colors[35].red = 12336; colors[35].green = 55512; colors[35].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[35]) );
	colors[36].red = 13878; colors[36].green = 53970; colors[36].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[36]) );
	colors[37].red = 15420; colors[37].green = 52428; colors[37].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[37]) );
	colors[38].red = 16962; colors[38].green = 50886; colors[38].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[38]) );
	colors[39].red = 18504; colors[39].green = 49344; colors[39].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[39]) );
	colors[40].red = 20046; colors[40].green = 47802; colors[40].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[40]) );
	colors[41].red = 23130; colors[41].green = 44718; colors[41].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[41]) );
	colors[42].red = 24672; colors[42].green = 43176; colors[42].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[42]) );
	colors[43].red = 26214; colors[43].green = 41634; colors[43].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[43]) );
	colors[44].red = 27756; colors[44].green = 40092; colors[44].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[44]) );
	colors[45].red = 29298; colors[45].green = 38550; colors[45].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[45]) );
	colors[46].red = 30840; colors[46].green = 37008; colors[46].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[46]) );
	colors[47].red = 32382; colors[47].green = 35466; colors[47].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[47]) );
	colors[48].red = 33924; colors[48].green = 33924; colors[48].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[48]) );
	colors[49].red = 35466; colors[49].green = 32382; colors[49].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[49]) );
	colors[50].red = 37008; colors[50].green = 30840; colors[50].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[50]) );
	colors[51].red = 38550; colors[51].green = 29298; colors[51].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[51]) );
	colors[52].red = 40092; colors[52].green = 27756; colors[52].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[52]) );
	colors[53].red = 41634; colors[53].green = 26214; colors[53].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[53]) );
	colors[54].red = 43176; colors[54].green = 24672; colors[54].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[54]) );
	colors[55].red = 44718; colors[55].green = 23130; colors[55].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[55]) );
	colors[56].red = 46260; colors[56].green = 21588; colors[56].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[56]) );
	colors[57].red = 47802; colors[57].green = 20046; colors[57].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[57]) );
	colors[58].red = 49344; colors[58].green = 18504; colors[58].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[58]) );
	colors[59].red = 50886; colors[59].green = 16962; colors[59].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[59]) );
	colors[60].red = 52428; colors[60].green = 15420; colors[60].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[60]) );
	colors[61].red = 55512; colors[61].green = 12336; colors[61].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[61]) );
	colors[62].red = 57054; colors[62].green = 10794; colors[62].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[62]) );
	colors[63].red = 58596; colors[63].green =  9252; colors[63].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[63]) );
	colors[64].red = 60138; colors[64].green =  7710; colors[64].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[64]) );
	colors[65].red = 61680; colors[65].green =  6168; colors[65].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[65]) );
	colors[66].red = 62451; colors[66].green =  4626; colors[66].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[66]) );
	colors[67].red = 63222; colors[67].green =  3084; colors[67].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[67]) );
	colors[68].red = 63993; colors[68].green =  1542; colors[68].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[68]) );
	colors[69].red = 64764; colors[69].green =     0; colors[69].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[69]) );
	colors[70].red = 64764; colors[70].green =     0; colors[70].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[70]) );
	colors[71].red = 64764; colors[71].green =     0; colors[71].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[71]) );
	colors[72].red = 64764; colors[72].green =     0; colors[72].blue =     0;
	XAllocColor(display3, default_cmap, &(colors[72]) );
	colors[73].red = 64764; colors[73].green =     0; colors[73].blue =   257;
	XAllocColor(display3, default_cmap, &(colors[73]) );
	colors[74].red = 64507; colors[74].green =     0; colors[74].blue =   771;
	XAllocColor(display3, default_cmap, &(colors[74]) );
	colors[75].red = 64250; colors[75].green =     0; colors[75].blue =  1285;
	XAllocColor(display3, default_cmap, &(colors[75]) );
	colors[76].red = 63993; colors[76].green =     0; colors[76].blue =  1799;
	XAllocColor(display3, default_cmap, &(colors[76]) );
	colors[77].red = 63736; colors[77].green =     0; colors[77].blue =  2313;
	XAllocColor(display3, default_cmap, &(colors[77]) );
	colors[78].red = 63736; colors[78].green =     0; colors[78].blue =  2570;
	XAllocColor(display3, default_cmap, &(colors[78]) );
	colors[79].red = 63736; colors[79].green =     0; colors[79].blue =  3084;
	XAllocColor(display3, default_cmap, &(colors[79]) );
	colors[80].red = 63736; colors[80].green =     0; colors[80].blue =  3598;
	XAllocColor(display3, default_cmap, &(colors[80]) );
	colors[81].red = 63736; colors[81].green =     0; colors[81].blue =  4112;
	XAllocColor(display3, default_cmap, &(colors[81]) );
	colors[82].red = 63222; colors[82].green =     0; colors[82].blue =  5140;
	XAllocColor(display3, default_cmap, &(colors[82]) );
	colors[83].red = 62965; colors[83].green =     0; colors[83].blue =  5654;
	XAllocColor(display3, default_cmap, &(colors[83]) );
	colors[84].red = 62708; colors[84].green =     0; colors[84].blue =  6425;
	XAllocColor(display3, default_cmap, &(colors[84]) );
	colors[85].red = 62451; colors[85].green =     0; colors[85].blue =  6939;
	XAllocColor(display3, default_cmap, &(colors[85]) );
	colors[86].red = 62194; colors[86].green =     0; colors[86].blue =  7453;
	XAllocColor(display3, default_cmap, &(colors[86]) );
	colors[87].red = 61937; colors[87].green =     0; colors[87].blue =  7967;
	XAllocColor(display3, default_cmap, &(colors[87]) );
	colors[88].red = 61680; colors[88].green =     0; colors[88].blue =  8738;
	XAllocColor(display3, default_cmap, &(colors[88]) );
	colors[89].red = 61680; colors[89].green =     0; colors[89].blue =  8995;
	XAllocColor(display3, default_cmap, &(colors[89]) );
	colors[90].red = 61423; colors[90].green =     0; colors[90].blue =  9509;
	XAllocColor(display3, default_cmap, &(colors[90]) );
	colors[91].red = 61166; colors[91].green =     0; colors[91].blue = 10023;
	XAllocColor(display3, default_cmap, &(colors[91]) );
	colors[92].red = 60909; colors[92].green =     0; colors[92].blue = 10537;
	XAllocColor(display3, default_cmap, &(colors[92]) );
	colors[93].red = 60652; colors[93].green =     0; colors[93].blue = 11051;
	XAllocColor(display3, default_cmap, &(colors[93]) );
	colors[94].red = 60652; colors[94].green =     0; colors[94].blue = 11565;
	XAllocColor(display3, default_cmap, &(colors[94]) );
	colors[95].red = 60652; colors[95].green =     0; colors[95].blue = 12079;
	XAllocColor(display3, default_cmap, &(colors[95]) );
	colors[96].red = 60652; colors[96].green =     0; colors[96].blue = 12850;
	XAllocColor(display3, default_cmap, &(colors[96]) );
	colors[97].red = 60395; colors[97].green =     0; colors[97].blue = 13364;
	XAllocColor(display3, default_cmap, &(colors[97]) );
	colors[98].red = 60138; colors[98].green =     0; colors[98].blue = 13878;
	XAllocColor(display3, default_cmap, &(colors[98]) );
	colors[99].red = 59881; colors[99].green =     0; colors[99].blue = 14392;
	XAllocColor(display3, default_cmap, &(colors[99]) );
	colors[100].red = 59624; colors[100].green =     0; colors[100].blue = 15163;
	XAllocColor(display3, default_cmap, &(colors[100]) );
	colors[101].red = 59367; colors[101].green =     0; colors[101].blue = 15677;
	XAllocColor(display3, default_cmap, &(colors[101]) );
	colors[102].red = 58853; colors[102].green =     0; colors[102].blue = 16705;
	XAllocColor(display3, default_cmap, &(colors[102]) );
	colors[103].red = 58596; colors[103].green =     0; colors[103].blue = 17219;
	XAllocColor(display3, default_cmap, &(colors[103]) );
	colors[104].red = 58596; colors[104].green =     0; colors[104].blue = 17476;
	XAllocColor(display3, default_cmap, &(colors[104]) );
	colors[105].red = 58596; colors[105].green =     0; colors[105].blue = 17990;
	XAllocColor(display3, default_cmap, &(colors[105]) );
	colors[106].red = 58596; colors[106].green =     0; colors[106].blue = 18504;
	XAllocColor(display3, default_cmap, &(colors[106]) );
	colors[107].red = 58596; colors[107].green =     0; colors[107].blue = 19018;
	XAllocColor(display3, default_cmap, &(colors[107]) );
	colors[108].red = 58339; colors[108].green =     0; colors[108].blue = 19532;
	XAllocColor(display3, default_cmap, &(colors[108]) );
	colors[109].red = 58082; colors[109].green =     0; colors[109].blue = 20046;
	XAllocColor(display3, default_cmap, &(colors[109]) );
	colors[110].red = 57825; colors[110].green =     0; colors[110].blue = 20560;
	XAllocColor(display3, default_cmap, &(colors[110]) );
	colors[111].red = 57568; colors[111].green =     0; colors[111].blue = 21331;
	XAllocColor(display3, default_cmap, &(colors[111]) );
	colors[112].red = 57311; colors[112].green =     0; colors[112].blue = 21845;
	XAllocColor(display3, default_cmap, &(colors[112]) );
	colors[113].red = 57054; colors[113].green =     0; colors[113].blue = 22359;
	XAllocColor(display3, default_cmap, &(colors[113]) );
	colors[114].red = 56797; colors[114].green =     0; colors[114].blue = 22873;
	XAllocColor(display3, default_cmap, &(colors[114]) );
	colors[115].red = 56540; colors[115].green =   257; colors[115].blue = 23644;
	XAllocColor(display3, default_cmap, &(colors[115]) );
	colors[116].red = 56283; colors[116].green =   257; colors[116].blue = 24158;
	XAllocColor(display3, default_cmap, &(colors[116]) );
	colors[117].red = 56026; colors[117].green =   257; colors[117].blue = 24672;
	XAllocColor(display3, default_cmap, &(colors[117]) );
	colors[118].red = 55769; colors[118].green =   257; colors[118].blue = 25186;
	XAllocColor(display3, default_cmap, &(colors[118]) );
	colors[119].red = 55512; colors[119].green =     0; colors[119].blue = 25957;
	XAllocColor(display3, default_cmap, &(colors[119]) );
	colors[120].red = 55512; colors[120].green =     0; colors[120].blue = 26214;
	XAllocColor(display3, default_cmap, &(colors[120]) );
	colors[121].red = 55512; colors[121].green =     0; colors[121].blue = 26728;
	XAllocColor(display3, default_cmap, &(colors[121]) );
	colors[122].red = 55512; colors[122].green =     0; colors[122].blue = 27756;
	XAllocColor(display3, default_cmap, &(colors[122]) );
	colors[123].red = 55255; colors[123].green =     0; colors[123].blue = 28270;
	XAllocColor(display3, default_cmap, &(colors[123]) );
	colors[124].red = 54998; colors[124].green =     0; colors[124].blue = 28784;
	XAllocColor(display3, default_cmap, &(colors[124]) );
	colors[125].red = 54741; colors[125].green =     0; colors[125].blue = 29298;
	XAllocColor(display3, default_cmap, &(colors[125]) );
	colors[126].red = 54484; colors[126].green =     0; colors[126].blue = 29812;
	XAllocColor(display3, default_cmap, &(colors[126]) );
	colors[127].red = 54227; colors[127].green =     0; colors[127].blue = 30326;
	XAllocColor(display3, default_cmap, &(colors[127]) );
	colors[128].red = 53970; colors[128].green =     0; colors[128].blue = 30840;
	XAllocColor(display3, default_cmap, &(colors[128]) );
	colors[129].red = 53713; colors[129].green =     0; colors[129].blue = 31354;
	XAllocColor(display3, default_cmap, &(colors[129]) );
	colors[130].red = 53456; colors[130].green =     0; colors[130].blue = 32125;
	XAllocColor(display3, default_cmap, &(colors[130]) );
	colors[131].red = 53199; colors[131].green =     0; colors[131].blue = 32639;
	XAllocColor(display3, default_cmap, &(colors[131]) );
	colors[132].red = 52942; colors[132].green =     0; colors[132].blue = 33153;
	XAllocColor(display3, default_cmap, &(colors[132]) );
	colors[133].red = 52685; colors[133].green =     0; colors[133].blue = 33667;
	XAllocColor(display3, default_cmap, &(colors[133]) );
	colors[134].red = 52428; colors[134].green =     0; colors[134].blue = 34438;
	XAllocColor(display3, default_cmap, &(colors[134]) );
	colors[135].red = 52428; colors[135].green =     0; colors[135].blue = 34695;
	XAllocColor(display3, default_cmap, &(colors[135]) );
	colors[136].red = 52428; colors[136].green =     0; colors[136].blue = 35209;
	XAllocColor(display3, default_cmap, &(colors[136]) );
	colors[137].red = 52428; colors[137].green =     0; colors[137].blue = 35723;
	XAllocColor(display3, default_cmap, &(colors[137]) );
	colors[138].red = 52428; colors[138].green =     0; colors[138].blue = 36237;
	XAllocColor(display3, default_cmap, &(colors[138]) );
	colors[139].red = 52171; colors[139].green =     0; colors[139].blue = 36751;
	XAllocColor(display3, default_cmap, &(colors[139]) );
	colors[140].red = 51914; colors[140].green =     0; colors[140].blue = 37265;
	XAllocColor(display3, default_cmap, &(colors[140]) );
	colors[141].red = 51657; colors[141].green =     0; colors[141].blue = 37779;
	XAllocColor(display3, default_cmap, &(colors[141]) );
	colors[142].red = 51400; colors[142].green =     0; colors[142].blue = 38550;
	XAllocColor(display3, default_cmap, &(colors[142]) );
	colors[143].red = 50886; colors[143].green =     0; colors[143].blue = 39578;
	XAllocColor(display3, default_cmap, &(colors[143]) );
	colors[144].red = 50629; colors[144].green =     0; colors[144].blue = 40092;
	XAllocColor(display3, default_cmap, &(colors[144]) );
	colors[145].red = 50372; colors[145].green =     0; colors[145].blue = 40863;
	XAllocColor(display3, default_cmap, &(colors[145]) );
	colors[146].red = 50372; colors[146].green =     0; colors[146].blue = 41120;
	XAllocColor(display3, default_cmap, &(colors[146]) );
	colors[147].red = 50372; colors[147].green =     0; colors[147].blue = 41634;
	XAllocColor(display3, default_cmap, &(colors[147]) );
	colors[148].red = 50372; colors[148].green =     0; colors[148].blue = 41891;
	XAllocColor(display3, default_cmap, &(colors[148]) );
	colors[149].red = 50372; colors[149].green =     0; colors[149].blue = 42405;
	XAllocColor(display3, default_cmap, &(colors[149]) );
	colors[150].red = 50115; colors[150].green =     0; colors[150].blue = 42919;
	XAllocColor(display3, default_cmap, &(colors[150]) );
	colors[151].red = 49858; colors[151].green =     0; colors[151].blue = 43433;
	XAllocColor(display3, default_cmap, &(colors[151]) );
	colors[152].red = 49601; colors[152].green =     0; colors[152].blue = 43947;
	XAllocColor(display3, default_cmap, &(colors[152]) );
	colors[153].red = 49344; colors[153].green =     0; colors[153].blue = 44718;
	XAllocColor(display3, default_cmap, &(colors[153]) );
	colors[154].red = 49087; colors[154].green =     0; colors[154].blue = 45232;
	XAllocColor(display3, default_cmap, &(colors[154]) );
	colors[155].red = 48830; colors[155].green =     0; colors[155].blue = 45746;
	XAllocColor(display3, default_cmap, &(colors[155]) );
	colors[156].red = 48573; colors[156].green =     0; colors[156].blue = 46260;
	XAllocColor(display3, default_cmap, &(colors[156]) );
	colors[157].red = 48316; colors[157].green =     0; colors[157].blue = 47031;
	XAllocColor(display3, default_cmap, &(colors[157]) );
	colors[158].red = 48059; colors[158].green =     0; colors[158].blue = 47545;
	XAllocColor(display3, default_cmap, &(colors[158]) );
	colors[159].red = 47802; colors[159].green =     0; colors[159].blue = 48059;
	XAllocColor(display3, default_cmap, &(colors[159]) );
	colors[160].red = 47545; colors[160].green =     0; colors[160].blue = 48573;
	XAllocColor(display3, default_cmap, &(colors[160]) );
	colors[161].red = 47288; colors[161].green =     0; colors[161].blue = 49344;
	XAllocColor(display3, default_cmap, &(colors[161]) );
	colors[162].red = 47288; colors[162].green =     0; colors[162].blue = 49601;
	XAllocColor(display3, default_cmap, &(colors[162]) );
	colors[163].red = 47288; colors[163].green =     0; colors[163].blue = 50629;
	XAllocColor(display3, default_cmap, &(colors[163]) );
	colors[164].red = 47288; colors[164].green =     0; colors[164].blue = 51143;
	XAllocColor(display3, default_cmap, &(colors[164]) );
	colors[165].red = 47031; colors[165].green =     0; colors[165].blue = 51657;
	XAllocColor(display3, default_cmap, &(colors[165]) );
	colors[166].red = 46774; colors[166].green =     0; colors[166].blue = 52171;
	XAllocColor(display3, default_cmap, &(colors[166]) );
	colors[167].red = 46517; colors[167].green =     0; colors[167].blue = 52685;
	XAllocColor(display3, default_cmap, &(colors[167]) );
	colors[168].red = 46260; colors[168].green =     0; colors[168].blue = 53456;
	XAllocColor(display3, default_cmap, &(colors[168]) );
	colors[169].red = 46003; colors[169].green =     0; colors[169].blue = 53970;
	XAllocColor(display3, default_cmap, &(colors[169]) );
	colors[170].red = 45746; colors[170].green =     0; colors[170].blue = 54484;
	XAllocColor(display3, default_cmap, &(colors[170]) );
	colors[171].red = 45489; colors[171].green =     0; colors[171].blue = 54998;
	XAllocColor(display3, default_cmap, &(colors[171]) );
	colors[172].red = 45232; colors[172].green =     0; colors[172].blue = 55512;
	XAllocColor(display3, default_cmap, &(colors[172]) );
	colors[173].red = 45232; colors[173].green =     0; colors[173].blue = 55769;
	XAllocColor(display3, default_cmap, &(colors[173]) );
	colors[174].red = 45232; colors[174].green =     0; colors[174].blue = 56283;
	XAllocColor(display3, default_cmap, &(colors[174]) );
	colors[175].red = 45232; colors[175].green =     0; colors[175].blue = 56797;
	XAllocColor(display3, default_cmap, &(colors[175]) );
	colors[176].red = 45232; colors[176].green =     0; colors[176].blue = 57311;
	XAllocColor(display3, default_cmap, &(colors[176]) );
	colors[177].red = 44975; colors[177].green =     0; colors[177].blue = 57825;
	XAllocColor(display3, default_cmap, &(colors[177]) );
	colors[178].red = 44718; colors[178].green =     0; colors[178].blue = 58339;
	XAllocColor(display3, default_cmap, &(colors[178]) );
	colors[179].red = 44461; colors[179].green =     0; colors[179].blue = 58853;
	XAllocColor(display3, default_cmap, &(colors[179]) );
	colors[180].red = 44204; colors[180].green =     0; colors[180].blue = 59624;
	XAllocColor(display3, default_cmap, &(colors[180]) );
	colors[181].red = 43947; colors[181].green =     0; colors[181].blue = 60138;
	XAllocColor(display3, default_cmap, &(colors[181]) );
	colors[182].red = 43690; colors[182].green =     0; colors[182].blue = 60652;
	XAllocColor(display3, default_cmap, &(colors[182]) );
	colors[183].red = 43176; colors[183].green =     0; colors[183].blue = 61937;
	XAllocColor(display3, default_cmap, &(colors[183]) );
	colors[184].red = 42919; colors[184].green =     0; colors[184].blue = 62451;
	XAllocColor(display3, default_cmap, &(colors[184]) );
	colors[185].red = 42662; colors[185].green =     0; colors[185].blue = 62965;
	XAllocColor(display3, default_cmap, &(colors[185]) );
	colors[186].red = 42405; colors[186].green =     0; colors[186].blue = 63479;
	XAllocColor(display3, default_cmap, &(colors[186]) );
	colors[187].red = 42148; colors[187].green =     0; colors[187].blue = 64250;
	XAllocColor(display3, default_cmap, &(colors[187]) );
	colors[188].red = 42148; colors[188].green =     0; colors[188].blue = 64507;
	XAllocColor(display3, default_cmap, &(colors[188]) );
	colors[189].red = 42148; colors[189].green =     0; colors[189].blue = 64764;
	XAllocColor(display3, default_cmap, &(colors[189]) );
	colors[190].red = 42148; colors[190].green =     0; colors[190].blue = 65021;
	XAllocColor(display3, default_cmap, &(colors[190]) );
	colors[191].red = 42148; colors[191].green =     0; colors[191].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[191]) );
	colors[192].red = 41891; colors[192].green =     0; colors[192].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[192]) );
	colors[193].red = 41634; colors[193].green =     0; colors[193].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[193]) );
	colors[194].red = 41377; colors[194].green =     0; colors[194].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[194]) );
	colors[195].red = 41120; colors[195].green =     0; colors[195].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[195]) );
	colors[196].red = 40863; colors[196].green =     0; colors[196].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[196]) );
	colors[197].red = 40606; colors[197].green =     0; colors[197].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[197]) );
	colors[198].red = 40349; colors[198].green =     0; colors[198].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[198]) );
	colors[199].red = 40092; colors[199].green =     0; colors[199].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[199]) );
	colors[200].red = 39835; colors[200].green =     0; colors[200].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[200]) );
	colors[201].red = 39578; colors[201].green =     0; colors[201].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[201]) );
	colors[202].red = 39321; colors[202].green =     0; colors[202].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[202]) );
	colors[203].red = 39064; colors[203].green =     0; colors[203].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[203]) );
	colors[204].red = 39064; colors[204].green =     0; colors[204].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[204]) );
	colors[205].red = 39064; colors[205].green =     0; colors[205].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[205]) );
	colors[206].red = 39064; colors[206].green =     0; colors[206].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[206]) );
	colors[207].red = 38807; colors[207].green =     0; colors[207].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[207]) );
	colors[208].red = 38550; colors[208].green =     0; colors[208].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[208]) );
	colors[209].red = 38293; colors[209].green =     0; colors[209].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[209]) );
	colors[210].red = 38036; colors[210].green =     0; colors[210].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[210]) );
	colors[211].red = 38807; colors[211].green =  2056; colors[211].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[211]) );
	colors[212].red = 39578; colors[212].green =  4112; colors[212].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[212]) );
	colors[213].red = 40349; colors[213].green =  6168; colors[213].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[213]) );
	colors[214].red = 41120; colors[214].green =  8224; colors[214].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[214]) );
	colors[215].red = 41891; colors[215].green = 10280; colors[215].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[215]) );
	colors[216].red = 42662; colors[216].green = 12336; colors[216].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[216]) );
	colors[217].red = 43433; colors[217].green = 14392; colors[217].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[217]) );
	colors[218].red = 44204; colors[218].green = 16448; colors[218].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[218]) );
	colors[219].red = 45232; colors[219].green = 18504; colors[219].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[219]) );
	colors[220].red = 46260; colors[220].green = 20560; colors[220].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[220]) );
	colors[221].red = 47288; colors[221].green = 22616; colors[221].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[221]) );
	colors[222].red = 48316; colors[222].green = 24672; colors[222].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[222]) );
	colors[223].red = 49087; colors[223].green = 26728; colors[223].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[223]) );
	colors[224].red = 50629; colors[224].green = 30840; colors[224].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[224]) );
	colors[225].red = 51400; colors[225].green = 32896; colors[225].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[225]) );
	colors[226].red = 52171; colors[226].green = 34695; colors[226].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[226]) );
	colors[227].red = 52942; colors[227].green = 36494; colors[227].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[227]) );
	colors[228].red = 53713; colors[228].green = 38293; colors[228].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[228]) );
	colors[229].red = 54484; colors[229].green = 40092; colors[229].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[229]) );
	colors[230].red = 55255; colors[230].green = 42148; colors[230].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[230]) );
	colors[231].red = 56026; colors[231].green = 44204; colors[231].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[231]) );
	colors[232].red = 56797; colors[232].green = 46260; colors[232].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[232]) );
	colors[233].red = 57568; colors[233].green = 48316; colors[233].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[233]) );
	colors[234].red = 58596; colors[234].green = 50372; colors[234].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[234]) );
	colors[235].red = 59624; colors[235].green = 52428; colors[235].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[235]) );
	colors[236].red = 60652; colors[236].green = 54484; colors[236].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[236]) );
	colors[237].red = 61680; colors[237].green = 56540; colors[237].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[237]) );
	colors[238].red = 62451; colors[238].green = 58596; colors[238].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[238]) );
	colors[239].red = 63222; colors[239].green = 60652; colors[239].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[239]) );
	colors[240].red = 63993; colors[240].green = 62708; colors[240].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[240]) );
	colors[241].red = 64764; colors[241].green = 64764; colors[241].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[241]) );
	colors[242].red = 65021; colors[242].green = 65021; colors[242].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[242]) );
	colors[243].red = 65278; colors[243].green = 65278; colors[243].blue = 65535;
	XAllocColor(display3, default_cmap, &(colors[243]) );
        isInitialized = 1;
    }
    
}

