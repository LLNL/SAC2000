/*** sacsun2linux.c  - writes a byte swaped little endian Seismic Analysis Code (SAC) file
 *                     from a big endian SAC file. SAC (c) LLNL
 *                     Run from little endian machine (e.g. intel 486, pentium's, cyrix...)
 *                     saclinux2sun is gcc complied G. A. Ichinose (1996)
 ***/
#include <stdio.h>
#include "sac.h"
#define NUMHEADBYTES 632 /* floats and longs are 440 rest are characters */

int main(int ac, char **av)
{
        struct sac_header sp;
	float *data, fbuf;
	int lbuf;
	int i, j;
	FILE *fp;
	char cbuf[NUMHEADBYTES], sacfile[128];
	float float_swap(char cbuf[]);
	int long_swap(char cbuf[]);
	
	if (ac == 1) { 
		fprintf(stderr, "Usage: %s [sacfile(s)]\n", av[0]);
		exit(-1);
	}
	
	for( i = 1;  i < ac; i++) {
		if ( (fp = fopen(av[i], "rb")) == NULL) {
			fprintf(stderr, "%s Error Opening file %s\n", av[0], av[i]);
		}

	/* set some sac header defaults */
		sp = sac_null;
		sp.internal4 = 6;
		sp.internal5 = 0;
		sp.internal6 = 0;
		sp.iftype = ITIME;
		sp.idep = IUNKN;
		sp.iztype = IB;
		sp.ievtyp = IUNKN;
		sp.leven = TRUE;
		sp.lpspol = FALSE;
		sp.lcalda = TRUE;
		sp.unused27 = FALSE;

	/** read in header **/
		fread(cbuf, 440*sizeof(char), 1, fp);
		sp.delta  = float_swap(cbuf);
		sp.depmin = float_swap(cbuf+4);
		sp.depmax = float_swap(cbuf+8);
		sp.b      = float_swap(cbuf+20);
		sp.e      = float_swap(cbuf+24);
		sp.o      = float_swap(cbuf+28);
		sp.a      = float_swap(cbuf+32);

		sp.t0     = float_swap(cbuf+40);
		sp.t1     = float_swap(cbuf+44);
		sp.t2     = float_swap(cbuf+48);
		sp.t3     = float_swap(cbuf+52);
                sp.t4     = float_swap(cbuf+56);
                sp.t5     = float_swap(cbuf+60);
                sp.t6     = float_swap(cbuf+64);
                sp.t7     = float_swap(cbuf+68);
		sp.t8     = float_swap(cbuf+72);
		sp.t9     = float_swap(cbuf+76);

		sp.stla  = float_swap(cbuf+124);
		sp.stlo  = float_swap(cbuf+128);
		sp.stel  = float_swap(cbuf+132);
		sp.stdp  = float_swap(cbuf+136);

		sp.evla   = float_swap(cbuf+140);
		sp.evlo   = float_swap(cbuf+144);
		sp.evdp   = float_swap(cbuf+152);

		sp.user0 =  float_swap(cbuf+160);
		sp.user1 =  float_swap(cbuf+164);
		sp.user2 =  float_swap(cbuf+168);
		sp.user3 =  float_swap(cbuf+172);
		sp.user4 =  float_swap(cbuf+176);
		sp.user5 =  float_swap(cbuf+180);
		sp.user6 =  float_swap(cbuf+184);
		sp.user7 =  float_swap(cbuf+188);
		sp.user8 =  float_swap(cbuf+192);
		sp.user9 =  float_swap(cbuf+196);

		sp.cmpaz  = float_swap(cbuf+228);
		sp.cmpinc = float_swap(cbuf+232);

		sp.nzyear = long_swap(cbuf+280);
		sp.nzjday = long_swap(cbuf+284);
		sp.nzhour = long_swap(cbuf+288);
		sp.nzmin  = long_swap(cbuf+292);
		sp.nzsec  = long_swap(cbuf+296);
		sp.nzmsec = long_swap(cbuf+300);
		sp.npts   = long_swap(cbuf+316);
		fread(cbuf, (632-440)*sizeof(char), 1, fp);
		bcopy( cbuf, sp.kstnm, 4 );
		sp.kstnm[4] = '\0';

		fprintf(stderr, "sta=%-4.4s dt=%g b=%g e=%g\n", sp.kstnm, sp.delta, sp.b, sp.e);
		fprintf(stderr, "%d %dj %dh %dm %ds %dms   n=%d\n",
			sp.nzyear, sp.nzjday, sp.nzhour, sp.nzmin, sp.nzsec, sp.nzmsec, sp.npts);

	/** read the data **/
		data = (float *)malloc(sp.npts*sizeof(float));
		for ( j=0; j<sp.npts; j++) {
			fread(cbuf, sizeof(char), 4, fp);
			data[j] = (float) float_swap(cbuf);
		}
		fclose(fp);

        /** write the header **/
		/* sprintf(sacfile, "%s.lin", av[i]); */

		if ( (fp = fopen(av[i], "w")) == NULL ) {
			fprintf(stdout, "Error in opening output file: %s\n", sacfile);
		}
		fwrite(&sp, sizeof(struct sac_header), 1, fp);
		fwrite(&data[0], sp.npts*sizeof(float), 1, fp);

		fclose(fp);
		free(data);
	}
	return 0;
}

int long_swap(char cbuf[])
{
        union {
                char cval[4];
                int lval;
        } l_union;

	l_union.cval[3] = cbuf[0];
	l_union.cval[2] = cbuf[1];
	l_union.cval[1] = cbuf[2];
	l_union.cval[0] = cbuf[3];
        return(l_union.lval);
}

float float_swap(char cbuf[])
{
        union {
                char cval[4];
                float fval;
        } f_union;

        f_union.cval[3] = cbuf[0];
	f_union.cval[2] = cbuf[1];
	f_union.cval[1] = cbuf[2];
	f_union.cval[0] = cbuf[3];
        return(f_union.fval);
}
