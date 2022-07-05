/***************************************
** Functions.
**
** int cancomp( unsigned char *b, unsigned int *y, int *n, int m, unsigned int *v0 );
** int uncancomp( unsigned char *b, unsigned int *y, int *n, int m, int *v0 );
*/

/***************************************
** Include files.
*/
#include "cancomp.h"
static const int endian = 0x12345678;
#define IS_LITTLE_ENDIAN (*(unsigned char *)&endian == 0x78)
#define IS_BIG_ENDIAN (*(unsigned char *)&endian == 0x12)

#ifdef IS_LITTLE_ENDIAN
#define CCSWAP(x) (3-x)
#else
#define CCSWAP(x) (x)
#endif

/***************************************
** Function declarations.
*/
static void pack(unsigned int m, unsigned int *y, unsigned char *b, int *j);
static void unpack(unsigned int m, unsigned int *y, unsigned char *b, int *j);

static int corrupt = 0;

int cancomp( unsigned char *b, unsigned int *y, int *n, int m, unsigned int *v0 )
{
	/*
	 * Compresses time series data according to the Canadian
	 * algorithm.
	 * b is the array of compressed data bytes
	 * y is the array of 4-byte uncompressed integer samples
	 * n is the number of bytes in b
	 * m is the number of samples (must be divisible by 20)
	 * v0 is used as the last value, it is not changed
	 *
	 * note that there are m samples, but m+1 elements in array y.
	 * the last element is needed for the differences.
	 *
	 * sets *n and elements of b
	 */

	int i, j, k;
	unsigned int im;
	unsigned int m0, m1, m2, m3, m4;
	unsigned int y0, y1, y2, y3, y4;
	unsigned int *py, *pm;
	unsigned int first;
	static unsigned int *max4 = 0;
	static int max4sz = 0;

	if (m % 20) return CANCOMP_NOT_20;
	if (!max4) {
		max4 = (unsigned int *)malloc((m/4) * sizeof(unsigned int));
		if (!max4) { 
		    free(max4);
		    return CANCOMP_ERR;
		}
		max4sz = m;
	}
	if (m > max4sz) {
		max4 = (unsigned int *)realloc((void *)max4, (m/4) *
			sizeof(unsigned int));
		if (!max4) { 
		    free(max4);
		    return CANCOMP_ERR;
		}
		max4sz = m;
	}
	corrupt = 0;

	/*
	 * first difference
	 * to simplify compression in place, make sure all first differences
	 * fit in the y array.  Save the first value which is treated
	 * differently anyway.
	 */
	first = *y;
	for (k = 0; k < m - 1; k++) y[k] = y[k + 1] - y[k];
	y[m - 1] = *v0 - y[m - 1];

	/*
	 * second difference
	 */
	for (k = m - 1; k > 0; k--) y[k] -= y[k - 1];

	/*
	 * number of bits for each block of 4 differences
	 */
	py = y;
	for (pm = max4, k = 0; k < m; k += 4, pm++) {
		/*
		 * compute 1's complement absolute value
		 * note -8 to +7 can be coded in 4 bits
		 * RJS:  I'm not sure what they meant, but they
		 *       coded y0 = 1 - y0 for signed longs.
		 *       (in the CNSN protocol manual).
		 *       I think they wanted y0 = -(1 + y0)
		 *       this would be y0 = 0xffffffff ^ y0
		 */
		/*
		y0 = *py++; if (y0 & 0x80000000) y0 = (0xffffffff ^ y0) + 2;
		y1 = *py++; if (y1 & 0x80000000) y1 = (0xffffffff ^ y1) + 2;
		y2 = *py++; if (y2 & 0x80000000) y2 = (0xffffffff ^ y2) + 2;
		y3 = *py++; if (y3 & 0x80000000) y3 = (0xffffffff ^ y3) + 2;
		*/
		y0 = *py++; if (y0 & 0x80000000) y0 = (0xffffffff ^ y0);
		y1 = *py++; if (y1 & 0x80000000) y1 = (0xffffffff ^ y1);
		y2 = *py++; if (y2 & 0x80000000) y2 = (0xffffffff ^ y2);
		y3 = *py++; if (y3 & 0x80000000) y3 = (0xffffffff ^ y3);
		y4 = y0 | y1 | y2 | y3;

			if (y4 & 0x78000000)	*pm = 32;
		else	if (y4 & 0x07800000)	*pm = 28;
		else	if (y4 & 0x00780000)	*pm = 24;
		else	if (y4 & 0x00060000)	*pm = 20;
		else	if (y4 & 0x00018000)	*pm = 18;
		else	if (y4 & 0x00006000)	*pm = 16;
		else	if (y4 & 0x00001800)	*pm = 14;
		else	if (y4 & 0x00000600)	*pm = 12;
		else	if (y4 & 0x00000180)	*pm = 10;
		else	if (y4 & 0x00000060)	*pm =  8;
		else	if (y4 & 0x00000018)	*pm =  6;
		else				*pm =  4;
	}

	/*
	 * pack data - blocks of 20 samples
	 */
	
	/*
	 * first sample immediately after index - write as 32 bit absolute value
	 */
	j = m / 10;
	b[j++] = (unsigned char)(first & 0xff000000) >> 24;
	b[j++] = (unsigned char)(first & 0x00ff0000) >> 16;
	b[j++] = (unsigned char)(first & 0x0000ff00) >>  8;
	b[j++] = (unsigned char)(first & 0x000000ff)      ;

	for (i = 0, py = y, pm = max4; i < m / 10; i += 2, py += 20) {
		m0 = *pm++;
		m1 = *pm++;
		m2 = *pm++;
		m3 = *pm++;
		m4 = *pm++;

		if ((m0 > 18) || (m1 > 18) || (m2 > 18) ||
		    (m3 > 18) || (m4 > 18)) {
			/*
			 * valid bits/sample: 4,8,12,16,20,24,28,32
			 * round up illegal bits/sample
			 */
			m0 += m0 & 0x2;
			m1 += m1 & 0x2;
			m2 += m2 & 0x2;
			m3 += m3 & 0x2;
			m4 += m4 & 0x2;

			pack(m0, py     , b, &j);
			pack(m1, py +  4, b, &j);
			pack(m2, py +  8, b, &j);
			pack(m3, py + 12, b, &j);
			pack(m4, py + 16, b, &j);

			/*
			im = (m0 - 4) << 10 + (m1 - 4) << 7 +
			     (m2 - 4) <<  4 + (m3 - 4) << 1 +
			     (m4 - 4) >>  2;
			*/
			m0 -= 4;
			m1 -= 4;
			m2 -= 4;
			m3 -= 4;
			m4 -= 4;
			im = (m0 << 10 | m1 << 7 | m2 << 4 | m3 << 1 | m4 >> 2);
			b[i] = (unsigned char)(im >> 8) | 0x80;	/* set hi bit */
			b[i + 1] = (unsigned char)im & 0xff;
		}
		else {
			/*
			 * 4,6,8,10,12,14,16,18 bits/sample
			 */
			pack(m0, py     , b, &j);
			pack(m1, py +  4, b, &j);
			pack(m2, py +  8, b, &j);
			pack(m3, py + 12, b, &j);
			pack(m4, py + 16, b, &j);

			/*
			im = (m0 - 4) << 11 + (m1 - 4) << 8 +
			     (m2 - 4) <<  5 + (m3 - 4) << 2 +
			     (m4 - 4) >>  1;
			*/
			m0 -= 4;
			m1 -= 4;
			m2 -= 4;
			m3 -= 4;
			m4 -= 4;
			im = (m0 << 11 | m1 << 8 | m2 << 5 | m3 << 2 | m4 >> 1);
			b[i] = (unsigned char)im >> 8;		/* clear hi bit */
			b[i + 1] = (unsigned char)im & 0xff;
		}
	}

	*n = j;		/* number of bytes in data field */
	free(max4);
	if (corrupt) return CANCOMP_CORRUPT;
	return CANCOMP_SUCCESS;
}

int uncancomp( unsigned char *b, unsigned int *y, int *n, int m, int *v0 )
{
	/*
	 * Uncompresses time series data according to the Canadian
	 * algorithm.
	 * b is the array of compressed data bytes
	 * y is the array of 4-byte uncompressed integer samples
	 * n is the number of bytes in b
	 * m is the number of samples (must be divisible by 20)
	 * v0 is the last value, which may be freely disregarded.
	 *
	 * note that there are m samples, but m+1 elements in array y.
	 * the last element is needed for the differences.
	 *
	 * sets *n to number of bytes used and elements of y
	 */

	int i, j, k;
	unsigned int x, *py;
	unsigned int first, save;

	if (m % 20) return CANCOMP_NOT_20;
	corrupt = 0;
	py = y;

	/*
	 * get first sample
	 */
	j = m / 10;
	first = (b[j] << 24) | (b[j + 1] << 16) | (b[j + 2] << 8) | b[j + 3];
	j += 4;

	/*
	 * unpack 20 samples at a time
	 */
	for (i = 0; i < m / 10; i += 2, py += 20) {
		if (b[i] >= 0x80) {
			/*
			 * 4,8,12,16,20,24,28 or 32 bits/sample
			 * x is index location
			 * 0x1c is 3-bit mask
			 */
			x = ((b[i] & 0x7f) << 8) | b[i + 1];
			unpack(((x >> 10) & 0x1c) + 4, py     , b, &j);
			unpack(((x >>  7) & 0x1c) + 4, py +  4, b, &j);
			unpack(((x >>  4) & 0x1c) + 4, py +  8, b, &j);
			unpack(((x >>  1) & 0x1c) + 4, py + 12, b, &j);
			unpack(((x <<  2) & 0x1c) + 4, py + 16, b, &j);
		}
		else {
			/*
			 * 4,6,8,10,12,14,16 or 18 bits/sample
			 * x is index location
			 * 0xe is 3-bit mask
			 */
			x = (b[i] << 8) | b[i + 1];
			unpack(((x >> 11) & 0xe) + 4, py     , b, &j);
			unpack(((x >>  8) & 0xe) + 4, py +  4, b, &j);
			unpack(((x >>  5) & 0xe) + 4, py +  8, b, &j);
			unpack(((x >>  2) & 0xe) + 4, py + 12, b, &j);
			unpack(((x <<  1) & 0xe) + 4, py + 16, b, &j);
		}
		if (j > *n) return CANCOMP_EXCEED;
	}

	/*
	 * undo second difference
	 */
	for (k = 1; k < m; k++) y[k] += y[k - 1];

	/*
	 * undo first difference
	 * (done so that first value gets put in first position,
	 * and last value pops off to be thrown away if necessary).
	 */
	for (k = 0; k < m; k++) {
		save = y[k];
		y[k] = first;
		first += save;
	}
	*v0 = first;

	if (corrupt) return CANCOMP_CORRUPT;
	return CANCOMP_SUCCESS;
}

static void pack( unsigned int m, unsigned int *y, unsigned char *b, int *j )
{
/*
 * m is max bits/sample
 * y is array of input data - first 4 longs are compressed
 * b is array of compressed data, *j is current reference in this array
 */
	/*
	 * pack 4 samples from y into "m" bits/sample in b
	 * assumes no errors in data
	 *
	 * input - 4 samples from y
	 * output - packed data, number of bytes is incremented on *j
	 *
	 * m must accurately reflect the max bits required.  Note that
	 * since y (in reality) may be signed, must take one extra bit
	 * (the 0 or 1) that fills the values out to the MSB.  Masks
	 * are used to remove excess 1's from negative values.
	 *
	 * Note - union is used to reduce operations.
	 * other simplifications from the original:
	 * 1) use all unsigned arithmetic - no need to check sign bit
	 * 2) use all logical bit operations (& << >> |)
	 * 3) recognize that right and left shifts off the end of a field
	 *    mean the bits drop on the floor (no need to precisely mask
	 *    bits being shifted to the edges of fields).
	 * 4) do not mask at all if bits left-shifted sufficiently.
	 */
	union longchar { unsigned int l; unsigned char b[4]; } u;
	unsigned int y0, y1, y2, y3;
	unsigned char *pb;
	
	y0 = y[0];
	y1 = y[1];
	y2 = y[2];
	y3 = y[3];

	pb = b + *j;

	switch (m) {	/* switch on bits/sample */

	case  4:
		*pb++ = (unsigned char)((y0 & 0xf) << 4 | (y1 & 0xf));
		*pb++ = (unsigned char)((y2 & 0xf) << 4 | (y3 & 0xf));
		*j += 2;
		break;

	case  6:
		/*
		*pb++ = (y0 & 0x3f) << 2 | (y1 & 0x30) >> 4;
		*pb++ = (y1 & 0x0f) << 4 | (y2 & 0x3c) >> 2;
		*pb++ = (y2 & 0x03) << 6 | (y3 & 0x3f)     ;
		*/
		u.l = y0 << 18 | (y1 & 0x3f) << 12 | (y2 & 0x3f) << 6 |
		      (y3 & 0x3f);
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 3;
		break;

	case  8:
		*pb++ = (unsigned char)y0 & 0xff;
		*pb++ = (unsigned char)y1 & 0xff;
		*pb++ = (unsigned char)y2 & 0xff;
		*pb++ = (unsigned char)y3 & 0xff;
		*j += 4;
		break;

	case 10:
		/* original (after conversion to unsigned and use of bit ops
		b[j    ] =                    (y0 & 0x3fc) >> 2;
		b[j + 1] = (y0 & 0x03) << 6 | (y1 & 0x3f0) >> 4;
		b[j + 2] = (y1 & 0x0f) << 4 | (y2 & 0x3c0) >> 6;
		b[j + 3] = (y2 & 0x3f) << 2 | (y3 & 0x300) >> 8;
		b[j + 4] = (y3 & 0xff)                         ;
		j += 5;
		(for example only)
		*/
		u.l = y0 << 22 | (y1 & 0x3ff) << 12 | (y2 & 0x3ff) << 2 |
		      (y3 & 0x3ff) >> 8;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*pb++ = (unsigned char)(y3 & 0xff);
		*j += 5;
		break;

	case 12:
		u.l = y0 << 20 | (y1 & 0xfff) << 8 | (y2 & 0xfff) >> 4;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y2 << 12 | (y3 & 0xfff);
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 6;
		break;

	case 14:
		u.l = y0 << 18 | (y1 & 0x3fff) << 4 | (y2 & 0x3fff) >> 10;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y2 << 14 | (y3 & 0x3fff);
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 7;
		break;

	case 16:
		u.l = y0 << 16 | (y1 & 0xffff);
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y2 << 16 | (y3 & 0xffff);
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 8;
		break;

	case 18:
		u.l = y0 << 14 | (y1 & 0x3ffff) >> 4;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y1 << 28 | (y2 & 0x3ffff) << 10 | (y3 & 0x3ffff) >> 8;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*pb++ = (unsigned char)(y3 & 0xff);
		*j += 9;
		break;

	case 20:
		u.l = y0 << 12 | (y1 & 0xfffff) >> 8;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y1 << 24 | (y2 & 0xfffff) << 4 | (y3 & 0xfffff) >> 16;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y3;
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 10;
		break;

	case 24:
		u.l = y0 <<  8 | (y1 & 0xffffff) >> 16;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y1 << 16 | (y2 & 0xffffff) >>  8;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y2 << 24 | (y3 & 0xffffff);
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 12;
		break;

	case 28:
		u.l = y0 <<  4 | (y1 & 0xfffffff) >> 24;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y1 <<  8 | (y2 & 0xfffffff) >> 20;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y2 << 12 | (y3 & 0xfffffff) >> 16;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y3;
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 14;
		break;

	case 32:
		u.l = y0;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y1;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y2;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		u.l = y3;
		*pb++ = u.b[CCSWAP(0)];
		*pb++ = u.b[CCSWAP(1)];
		*pb++ = u.b[CCSWAP(2)];
		*pb++ = u.b[CCSWAP(3)];
		*j += 16;
		break;

	default:
		/* No default - assume calling program is corrupt */
		corrupt = 1;
		break;
	}
}

static void unpack( unsigned int m, unsigned int *y, unsigned char *b, int *j)
{
/*
 * m is max bits/sample
 * y is array of input data - first 4 longs are compressed
 * b is array of compressed data, *j is current reference in this array
 */
	/*
	 * unpack 4 samples into y from "m" bits/sample in b
	 *
	 * output - 4 samples of y
	 * input - packed data, number of bytes is incremented on *j
	 *
	 * m must accurately reflect the max bits required.  Note that
	 * since y (in reality) may be signed, must check the extra bit
	 * (the 0 or 1) then fill the values out to the MSB.
	 *
	 * Note - union is used to reduce operations.
	 * other simplifications from the original:
	 * 1) use all unsigned arithmetic - no need to check sign bit
	 * 2) use all logical bit operations (& << >> |)
	 * 3) recognize that right and left shifts off the end of a field
	 *    mean the bits drop on the floor (no need to precisely mask
	 *    bits being shifted to the edges of fields).
	 * 4) do not mask at all if bits left-shifted sufficiently.
	 */
	union longchar { unsigned int l; unsigned char b[4]; } u, v;
	unsigned int y0, y1, y2, y3;
	unsigned char *pb;
	
#ifdef DEBUG
	u.l = v.l = 0;
#endif

	pb = b + *j;

	switch (m) {	/* switch on bits/sample */

	case  4:
		y0 = *pb >> 4;
		y1 = *pb++ & 0xf;
		y2 = *pb >> 4;
		y3 = *pb++ & 0xf;
		if (y0 & 0x8) y0 |= 0xfffffff0;
		if (y1 & 0x8) y1 |= 0xfffffff0;
		if (y2 & 0x8) y2 |= 0xfffffff0;
		if (y3 & 0x8) y3 |= 0xfffffff0;
		*j += 2;
		break;

	case  6:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		y0 = (u.l >> 26);
		y1 = (u.l >> 20) & 0x3f;
		y2 = (u.l >> 14) & 0x3f;
		y3 = (u.l >>  8) & 0x3f;
		if (y0 & 0x20) y0 |= 0xffffffc0;
		if (y1 & 0x20) y1 |= 0xffffffc0;
		if (y2 & 0x20) y2 |= 0xffffffc0;
		if (y3 & 0x20) y3 |= 0xffffffc0;
		*j += 3;
		break;

	case  8:
		y0 = *pb++;
		y1 = *pb++;
		y2 = *pb++;
		y3 = *pb++;
		if (y0 & 0x80) y0 |= 0xffffff00;
		if (y1 & 0x80) y1 |= 0xffffff00;
		if (y2 & 0x80) y2 |= 0xffffff00;
		if (y3 & 0x80) y3 |= 0xffffff00;
		*j += 4;
		break;

	case 10:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y0 = (u.l >> 22);
		y1 = (u.l >> 12) & 0x3ff;
		y2 = (u.l >>  2) & 0x3ff;
		y3 = ((u.l <<  8) & 0x3ff) | (*pb++);
		if (y0 & 0x200) y0 |= 0xfffffc00;
		if (y1 & 0x200) y1 |= 0xfffffc00;
		if (y2 & 0x200) y2 |= 0xfffffc00;
		if (y3 & 0x200) y3 |= 0xfffffc00;
		*j += 5;
		break;

	case 12:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		y0 = (u.l >> 20);
		y1 = (u.l >>  8) & 0xfff;
		y2 = ((u.l <<  4) & 0xfff) | (v.l >> 28);
		y3 = (v.l >> 16) & 0xfff;
		if (y0 & 0x800) y0 |= 0xfffff000;
		if (y1 & 0x800) y1 |= 0xfffff000;
		if (y2 & 0x800) y2 |= 0xfffff000;
		if (y3 & 0x800) y3 |= 0xfffff000;
		*j += 6;
		break;

	case 14:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		v.b[CCSWAP(2)] = *pb++;
		y0 = (u.l >> 18);
		y1 = (u.l >>  4) & 0x3fff;
		y2 = ((u.l << 10) & 0x3fff) | (v.l >> 22);
		y3 = (v.l >>  8) & 0x3fff;
		if (y0 & 0x2000) y0 |= 0xffffc000;
		if (y1 & 0x2000) y1 |= 0xffffc000;
		if (y2 & 0x2000) y2 |= 0xffffc000;
		if (y3 & 0x2000) y3 |= 0xffffc000;
		*j += 7;
		break;

	case 16:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y0 = u.l >> 16;
		y1 = u.l & 0xffff;
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y2 = u.l >> 16;
		y3 = u.l & 0xffff;
		if (y0 & 0x8000) y0 |= 0xffff0000;
		if (y1 & 0x8000) y1 |= 0xffff0000;
		if (y2 & 0x8000) y2 |= 0xffff0000;
		if (y3 & 0x8000) y3 |= 0xffff0000;
		*j += 8;
		break;

	case 18:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		v.b[CCSWAP(2)] = *pb++;
		v.b[CCSWAP(3)] = *pb++;
		y0 = (u.l >> 14);
		y1 = ((u.l <<  4) & 0x3ffff) | (v.l >> 28);
		y2 = (v.l >> 10) & 0x3ffff;
		y3 = ((v.l <<  8) & 0x3ffff) | (*pb++);
		if (y0 & 0x20000) y0 |= 0xfffc0000;
		if (y1 & 0x20000) y1 |= 0xfffc0000;
		if (y2 & 0x20000) y2 |= 0xfffc0000;
		if (y3 & 0x20000) y3 |= 0xfffc0000;
		*j += 9;
		break;

	case 20:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		v.b[CCSWAP(2)] = *pb++;
		v.b[CCSWAP(3)] = *pb++;
		y0 = (u.l >> 12);
		y1 = ((u.l <<  8) & 0xfffff) | (v.l >> 24);
		y2 = (v.l >>  4) & 0xfffff;
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		y3 = ((v.l << 16) & 0xfffff) | (u.l >> 16);
		if (y0 & 0x80000) y0 |= 0xfff00000;
		if (y1 & 0x80000) y1 |= 0xfff00000;
		if (y2 & 0x80000) y2 |= 0xfff00000;
		if (y3 & 0x80000) y3 |= 0xfff00000;
		*j += 10;
		break;

	case 24:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		v.b[CCSWAP(2)] = *pb++;
		v.b[CCSWAP(3)] = *pb++;
		y0 = (u.l >>  8);
		y1 = ((u.l << 16) & 0xffffff) | (v.l >> 16);
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y2 = ((v.l <<  8) & 0xffffff) | (u.l >> 24);
		y3 = u.l & 0xffffff;
		if (y0 & 0x800000) y0 |= 0xff000000;
		if (y1 & 0x800000) y1 |= 0xff000000;
		if (y2 & 0x800000) y2 |= 0xff000000;
		if (y3 & 0x800000) y3 |= 0xff000000;
		*j += 12;
		break;

	case 28:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		v.b[CCSWAP(2)] = *pb++;
		v.b[CCSWAP(3)] = *pb++;
		y0 = (u.l >>  4);
		y1 = ((u.l << 24) & 0xfffffff) | (v.l >>  8);
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y2 = ((v.l << 20) & 0xfffffff) | (u.l >> 12);
		v.b[CCSWAP(0)] = *pb++;
		v.b[CCSWAP(1)] = *pb++;
		y3 = ((u.l << 16) & 0xfffffff) | (v.l >> 16);
		if (y0 & 0x8000000) y0 |= 0xf0000000;
		if (y1 & 0x8000000) y1 |= 0xf0000000;
		if (y2 & 0x8000000) y2 |= 0xf0000000;
		if (y3 & 0x8000000) y3 |= 0xf0000000;
		*j += 14;
		break;

	case 32:
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y0 = u.l;
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y1 = u.l;
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y2 = u.l;
		u.b[CCSWAP(0)] = *pb++;
		u.b[CCSWAP(1)] = *pb++;
		u.b[CCSWAP(2)] = *pb++;
		u.b[CCSWAP(3)] = *pb++;
		y3 = u.l;
		*j += 16;
		break;

	default:
		/* No default - assume calling program is corrupt */
		corrupt = 1;
		break;
	}

	y[0] = y0;
	y[1] = y1;
	y[2] = y2;
	y[3] = y3;
}
