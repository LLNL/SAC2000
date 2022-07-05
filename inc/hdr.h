/* ../../inc/hdr.h */

#define	MCMHDR	(MFHDR + MNHDR + MIHDR + MLHDR)
#define	MFHDR	70
#define	MHDR	(MCMHDR + MKMHDR)
#define MHDRFILE (MCMHDR + FILEMKMHDR)
#define	MIHDR	20
#define	MIV	830
#define	MKHDR	24
#define FOURBYTEHDRS 164

/* the old character header (kmhdr) size (fortran version) was
   2*MKHDR 32bit words, due to the fact that each entry was
   8 characters (2 32bit words) int.

#define MKMHDR  (2*MKHDR)
  the new character header (kmhdr) size (c version) in 32bit
  words is (9*MKHDR)/4.  Which for MKHDR=24 is 54.  If MKHDR
  is changed then this will have to be looked at, because
  (9*MKHDR)/4 may have a remainder.
*/
/*
  For purposes of maintaining backward compatibility, the
  null terminated strings comprising the character header
  data in memory are written into the file concatenated
  without the terminating nulls.  Read and write operations
  map these strings into/out of the in-memory struct.
*/
#define MKMHDR  54 /* in-memory size of character header struct */
#define FILEMKMHDR (2*MKHDR) /* size of file character header data block */
#define	MLHDR	5
#define	MNHDR	15
#define	MVHDRC	6



struct t_cmhdr {
    float fhdr[MFHDR];
	int nhdr[MNHDR], ihdr[MIHDR];
	int lhdr[MLHDR];
	int niv[MIV];
	float fundef;
	int iundef, nundef, nvhdrc;
	float exthdr[20];
	int  linc ,	/* TRUE if INC option is set on lh. maf 961212 */
	      llh ;	/* TRUE during the execution of xlh(). maf 961212 */
	}	cmhdr;
struct t_kmhdr {
	char khdr[MKHDR][9], kundef[9];
	}	kmhdr;


/* 	Note:  in the following list, ninf, nhst, and nsn were 
	changed to norid, nevid, and nwfid respectively.  maf 961031 

	mag, imagtyp, and imagsrc added to provide magnitude, 
	magnitude type (mb, ms, ml, etc.) and magnitude source ( ie
	what institution measured the magnitude).  maf 970205 */

#ifdef DOINITS
        float const* a = 0; /* = (float*)((char*)cmhdr.fhdr + 32);*/
        float const* arrivl = 0; /* = (float*)((char*)cmhdr.fhdr + 32);*/

	float *const az = 0; /*(float*)((char*)cmhdr.fhdr + 204);*/
	float *const b = 0; /*(float*)((char*)cmhdr.fhdr + 20);*/
	float *const baz = 0; /*(float*)((char*)cmhdr.fhdr + 208);*/
	float *const begin = 0; /*(float*)((char*)cmhdr.fhdr + 20);*/
	float *const cmpaz = 0; /*(float*)((char*)cmhdr.fhdr + 228);*/
	float *const cmpinc = 0; /*(float*)((char*)cmhdr.fhdr + 232);*/
	float *const delta = 0; /*(float*)cmhdr.fhdr;*/
	float *const depmax = 0; /*(float*)((char*)cmhdr.fhdr + 8);*/
	float *const depmen = 0; /*(float*)((char*)cmhdr.fhdr + 224);*/
	float *const depmin = 0; /*(float*)((char*)cmhdr.fhdr + 4);*/
	float *const depmn = 0; /*(float*)((char*)cmhdr.fhdr + 4);*/
	float *const depmx = 0; /*(float*)((char*)cmhdr.fhdr + 8);*/
	float *const dist = 0; /*(float*)((char*)cmhdr.fhdr + 200);*/
	float *const e = 0; /*(float*)((char*)cmhdr.fhdr + 24);*/
	float *const ennd = 0; /*(float*)((char*)cmhdr.fhdr + 24);*/
	float *const evdp = 0; /*(float*)((char*)cmhdr.fhdr + 152);*/
	float *const evel = 0; /*(float*)((char*)cmhdr.fhdr + 148);*/
	float *const evla = 0; /*(float*)((char*)cmhdr.fhdr + 140);*/
	float *const evlo = 0; /*(float*)((char*)cmhdr.fhdr + 144);*/
	float *const f = 0; /*(float*)((char*)cmhdr.fhdr + 80);*/
	float *const mag = 0; /*(float*)((char*)cmhdr.fhdr + 156);*/
	float *const fhdr64 = 0; /*(float*)((char*)cmhdr.fhdr + 252);*/
	float *const fhdr65 = 0; /*(float*)((char*)cmhdr.fhdr + 256);*/
	float *const fhdr66 = 0; /*(float*)((char*)cmhdr.fhdr + 260);*/
	float *const fhdr67 = 0; /*(float*)((char*)cmhdr.fhdr + 264);*/
	float *const fhdr68 = 0; /*(float*)((char*)cmhdr.fhdr + 268);*/
	float *const fhdr69 = 0; /*(float*)((char*)cmhdr.fhdr + 272);*/
	float *const fhdr70 = 0; /*(float*)((char*)cmhdr.fhdr + 276);*/
	float *const fini = 0; /*(float*)((char*)cmhdr.fhdr + 80);*/
	float *const fmean = 0; /*(float*)((char*)cmhdr.fhdr + 224);*/
	float *const fmt = 0; /*(float*)((char*)cmhdr.fhdr + 36);*/
	float *const gcarc = 0; /*(float*)((char*)cmhdr.fhdr + 212);*/
	int *const ia = 0; /*(int*)((char*)cmhdr.niv + 44);*/
	int *const iacc = 0; /*(int*)((char*)cmhdr.niv + 28);*/
	int *const iamph = 0; /*(int*)((char*)cmhdr.niv + 8);*/
	int *const ib = 0; /*(int*)((char*)cmhdr.niv + 32);*/
	int *const ichem = 0; /*(int*)((char*)cmhdr.niv + 168);*/
	int *const iday = 0; /*(int*)((char*)cmhdr.niv + 36);*/
	int *const idep = 0; /*(int*)((char*)cmhdr.ihdr + 4);*/


	int *const idisp    = 0; /*int*)((char*)cmhdr.niv + 20);*/
	int *const idown = 0; /*int*)((char*)cmhdr.niv + 116);*/
	int *const idrop = 0; /*int*)((char*)cmhdr.niv + 184);*/
	int *const ieast = 0; /*int*)((char*)cmhdr.niv + 108;*/
	int *const ievreg = 0; /*int*)((char*)cmhdr.ihdr + 24;*/
	int *const ievtyp = 0; /*int*)((char*)cmhdr.ihdr + 28;*/
	int *const iftype = 0; /*int*)cmhdr.ihdr;*/
	int *const iglch = 0; /*int*)((char*)cmhdr.niv + 180;*/
	int *const igood = 0; /*int*)((char*)cmhdr.niv + 176;*/
	int *const imagtyp = 0; /*int*)((char*)cmhdr.ihdr + 40;*//* magnitude type. maf */
	int *const imagsrc = 0; /*int*)((char*)cmhdr.ihdr + 44;*//* magnitude source. 970205 */
	int *const ihdr13 = 0; /*int*)((char*)cmhdr.ihdr + 48);*/
	int *const ihdr14 = 0; /*int*)((char*)cmhdr.ihdr + 52) ;*/
	int *const ihdr15 = 0; /*int*)((char*)cmhdr.ihdr + 56) ;*/
	int *const ihdr16 = 0; /*int*)((char*)cmhdr.ihdr + 60) ;*/
	int *const ihdr17 = 0; /*int*)((char*)cmhdr.ihdr + 64) ;*/
	int *const ihdr18 = 0; /*int*)((char*)cmhdr.ihdr + 68) ;*/
	int *const ihdr19 = 0; /*int*)((char*)cmhdr.ihdr + 72) ;*/
	int *const ihdr20 = 0; /*int*)((char*)cmhdr.ihdr + 76) ;*/
	int *const ihdr4 = 0; /*int*)((char*)cmhdr.ihdr + 12) ;*/
	int *const ihglp = 0; /*int*)((char*)cmhdr.niv + 136) ;*/
	int *const ihorza = 0; /*int*)((char*)cmhdr.niv + 112) ;*/
	int *const iinst = 0; /*int*)((char*)cmhdr.ihdr + 16) ;*/
	int *const illlbb = 0; /*int*)((char*)cmhdr.niv + 124) ;*/
	int *const ilowsn = 0; /*int*)((char*)cmhdr.niv + 188) ;*/
        /* these 18 added for magnitude info. maf 970205 */
        int *const imb = 0; /*int*)((char*)cmhdr.niv + 204) ;*/
        int *const ims = 0; /*int*)((char*)cmhdr.niv + 208) ;*/
        int *const iml = 0; /*int*)((char*)cmhdr.niv + 212) ;*/
        int *const imw = 0; /*int*)((char*)cmhdr.niv + 216) ;*/
        int *const imd = 0; /*int*)((char*)cmhdr.niv + 220) ;*/
        int *const imx = 0; /*int*)((char*)cmhdr.niv + 224) ;*/
        int *const ineic = 0; /*int*)((char*)cmhdr.niv + 228) ;*/
        int *const ipdeq = 0; /*int*)((char*)cmhdr.niv + 232) ;*/
	int *const ipdew = 0; /*int*)((char*)cmhdr.niv + 236) ;*/
	int *const ipde = 0; /*int*)((char*)cmhdr.niv + 240) ;*/
        int *const iisc = 0; /*(int*)((char*)cmhdr.niv + 244) ;*/
        int *const ireb = 0; /*(int*)((char*)cmhdr.niv + 248) ;*/
        int *const iusgs = 0; /*(int*)((char*)cmhdr.niv + 252) ;*/
        int *const ibrk = 0; /*(int*)((char*)cmhdr.niv + 256) ;*/
        int *const icaltech = 0; /*(int*)((char*)cmhdr.niv + 260) ;*/
        int *const illnl = 0; /*(int*)((char*)cmhdr.niv + 264) ;*/
        int *const ievloc = 0; /*(int*)((char*)cmhdr.niv + 268) ;*/
        int *const ijsop = 0; /*(int*)((char*)cmhdr.niv + 272) ;*/
        int *const iuser = 0; /*(int*)((char*)cmhdr.niv + 276) ;*/
        int *const iunknown = 0; /*(int*)((char*)cmhdr.niv + 280) ;*/
	/* These 17 lines added for ievtyp.  maf 970325 */
	int *const iqb = 0; /*(int*)((char*)cmhdr.niv + 284) ;*/
	int *const iqb1 = 0; /*(int*)((char*)cmhdr.niv + 288) ;*/
	int *const iqb2 = 0; /*(int*)((char*)cmhdr.niv + 292) ;*/
        int *const iqbx = 0; /*(int*)((char*)cmhdr.niv + 296) ;*/
        int *const iqmt = 0; /*(int*)((char*)cmhdr.niv + 300) ;*/
        int *const ieq = 0; /*(int*)((char*)cmhdr.niv + 304) ;*/
        int *const ieq1 = 0; /*(int*)((char*)cmhdr.niv + 308) ;*/
        int *const ieq2 = 0; /*(int*)((char*)cmhdr.niv + 312) ;*/
        int *const ime = 0; /*(int*)((char*)cmhdr.niv + 316) ;*/
        int *const iex = 0; /*(int*)((char*)cmhdr.niv + 320) ;*/
        int *const inu = 0; /*(int*)((char*)cmhdr.niv + 324) ;*/
        int *const inc = 0; /*(int*)((char*)cmhdr.niv + 328) ;*/
        int *const io_ = 0; /*(int*)((char*)cmhdr.niv + 332) ;*/
        int *const il = 0; /*(int*)((char*)cmhdr.niv + 336) ;*/
        int *const ir = 0; /*(int*)((char*)cmhdr.niv + 340) ;*/
        int *const it = 0; /*(int*)((char*)cmhdr.niv + 344) ;*/
        int *const iu = 0; /*(int*)((char*)cmhdr.niv + 348) ;*/
        /* These 9 added for ievtyp, to keep up with database. maf 000530 */
        int *const ieq3 = 0; /*(int*)((char*)cmhdr.niv + 352) ;*/
        int *const ieq0 = 0; /*(int*)((char*)cmhdr.niv + 356) ;*/
        int *const iex0 = 0; /*(int*)((char*)cmhdr.niv + 360) ;*/
        int *const iqc = 0; /*(int*)((char*)cmhdr.niv + 364) ;*/
        int *const iqb0 = 0; /*(int*)((char*)cmhdr.niv + 368) ;*/
        int *const igey = 0; /*(int*)((char*)cmhdr.niv + 372) ;*/
        int *const ilit = 0; /*(int*)((char*)cmhdr.niv + 376) ;*/
        int *const imet = 0; /*(int*)((char*)cmhdr.niv + 380) ;*/
        int *const iodor = 0; /*(int*)((char*)cmhdr.niv + 384) ;*/

	int *const inorth = 0; /*(int*)((char*)cmhdr.niv + 104) ;*/
	int *const inucl = 0; /*(int*)((char*)cmhdr.niv + 144) ;*/
	int *const io = 0; /*(int*)((char*)cmhdr.niv + 40) ;*/
	int *const iother = 0; /*(int*)((char*)cmhdr.niv + 172) ;*/
	int *const ipostn = 0; /*(int*)((char*)cmhdr.niv + 152) ;*/
	int *const ipostq = 0; /*(int*)((char*)cmhdr.niv + 164) ;*/
	int *const ipren = 0; /*(int*)((char*)cmhdr.niv + 148) ;*/
	int *const ipreq = 0; /*(int*)((char*)cmhdr.niv + 160) ;*/
	int *const iquake = 0; /*(int*)((char*)cmhdr.niv + 156) ;*/
	int *const iqual = 0; /*(int*)((char*)cmhdr.ihdr + 32) ;*/
	int *const iradev = 0; /*(int*)((char*)cmhdr.niv + 96) ;*/
	int *const iradnv = 0; /*(int*)((char*)cmhdr.niv + 88) ;*/
	int *const irldta = 0; /*(int*)((char*)cmhdr.niv + 192) ;*/
	int *const irlim = 0; /*(int*)((char*)cmhdr.niv + 4) ;*/
	int *const isro = 0; /*(int*)((char*)cmhdr.niv + 140) ;*/
	int *const istreg = 0; /*(int*)((char*)cmhdr.ihdr + 20) ;*/
	int *const isynth = 0; /*(int*)((char*)cmhdr.ihdr + 36) ;*/
	int *const it0 = 0; /*(int*)((char*)cmhdr.niv + 48) ;*/
	int *const it1 = 0; /*(int*)((char*)cmhdr.niv + 52) ;*/
	int *const it2 = 0; /*(int*)((char*)cmhdr.niv + 56) ;*/
	int *const it3 = 0; /*(int*)((char*)cmhdr.niv + 60) ;*/
	int *const it4 = 0; /*(int*)((char*)cmhdr.niv + 64) ;*/
	int *const it5 = 0; /*(int*)((char*)cmhdr.niv + 68) ;*/
	int *const it6 = 0; /*(int*)((char*)cmhdr.niv + 72) ;*/
	int *const it7 = 0; /*(int*)((char*)cmhdr.niv + 76) ;*/
	int *const it8 = 0; /*(int*)((char*)cmhdr.niv + 80) ;*/
	int *const it9 = 0; /*(int*)((char*)cmhdr.niv + 84) ;*/
	int *const itanev = 0; /*(int*)((char*)cmhdr.niv + 100) ;*/
	int *const itannv = 0; /*(int*)((char*)cmhdr.niv + 92) ;*/
	int *const itime = 0; /*(int*)cmhdr.niv;*/
	int *const iunkn = 0; /*(int*)((char*)cmhdr.niv + 16) ;*/
	int *const iup = 0; /*(int*)((char*)cmhdr.niv + 120) ;*/
	int *const ivel = 0; /*(int*)((char*)cmhdr.niv + 24) ;*/
	int *const ivolts = 0; /*(int*)((char*)cmhdr.niv + 196) ;*/
	int *const iwwsn1 = 0; /*(int*)((char*)cmhdr.niv + 128) ;*/
	int *const iwwsn2 = 0; /*(int*)((char*)cmhdr.niv + 132) ;*/
	int *const ixy = 0; /*(int*)((char*)cmhdr.niv + 12) ;*/
	int *const ixyz = 0; /*(int*)((char*)cmhdr.niv + 200) ;*/
	int *const iztype = 0; /*(int*)((char*)cmhdr.ihdr + 8) ;*/

	char *const kstnm = 0; /*(char*)kmhdr.khdr;*/
	char *const kevnm = 0; /*(char*)((char*)kmhdr.khdr + 9) ;*/
	char *const khole = 0; /*(char*)((char*)kmhdr.khdr + 27) ;*/
	char *const ko = 0; /*(char*)((char*)kmhdr.khdr + 36) ;*/
	char *const ka = 0; /*(char*)((char*)kmhdr.khdr + 45) ;*/
	char *const kt0 = 0; /*(char*)((char*)kmhdr.khdr + 54) ;*/
	char *const kt1 = 0; /*(char*)((char*)kmhdr.khdr + 63) ;*/
	char *const kt2 = 0; /*(char*)((char*)kmhdr.khdr + 72) ;*/
	char *const kt3 = 0; /*(char*)((char*)kmhdr.khdr + 81) ;*/
	char *const kt4 = 0; /*(char*)((char*)kmhdr.khdr + 90) ;*/
	char *const kt5 = 0; /*(char*)((char*)kmhdr.khdr + 99) ;*/
	char *const kt6 = 0; /*(char*)((char*)kmhdr.khdr + 108) ;*/
	char *const kt7 = 0; /*(char*)((char*)kmhdr.khdr + 117) ;*/
	char *const kt8 = 0; /*(char*)((char*)kmhdr.khdr + 126) ;*/
	char *const kt9 = 0; /*(char*)((char*)kmhdr.khdr + 135) ;*/
	char *const kf = 0; /*(char*)((char*)kmhdr.khdr + 144) ;*/
	char *const kuser0 = 0; /*(char*)((char*)kmhdr.khdr + 153) ;*/
	char *const kuser1 = 0; /*(char*)((char*)kmhdr.khdr + 162) ;*/
	char *const kuser2 = 0; /*(char*)((char*)kmhdr.khdr + 171) ;*/
	char *const kcmpnm = 0; /*(char*)((char*)kmhdr.khdr + 180) ;*/
	char *const knetwk = 0; /*(char*)((char*)kmhdr.khdr + 189) ;*/
	char *const kdatrd = 0; /*(char*)((char*)kmhdr.khdr + 198) ;*/
	char *const kinst = 0; /*(char*)((char*)kmhdr.khdr + 207) ;*/

	int *const lcalda = 0; /*(int*)((char*)cmhdr.lhdr + 12) ;*/
	int *const leven = 0; /*(int*)cmhdr.lhdr;*/
	int *const lhdr5 = 0; /*(int*)((char*)cmhdr.lhdr + 16) ;*/
	int *const lovrok = 0; /*(int*)((char*)cmhdr.lhdr + 8) ;*/
	int *const lpspol = 0; /*(int*)((char*)cmhdr.lhdr + 4) ;*/
        int *const nevid = 0; /*(int*)((char*)cmhdr.nhdr + 32) ;*/
	int *const nhdr15 = 0; /*(int*)((char*)cmhdr.nhdr + 56) ;*/
	int *const norid = 0; /*(int*)((char*)cmhdr.nhdr + 28) ;*/
	int *const npts = 0; /*(int*)((char*)cmhdr.nhdr + 36) ;*/
	int *const nsnpts = 0; /*(int*)((char*)cmhdr.nhdr + 40) ;*/
	int *const nvhdr = 0; /*(int*)((char*)cmhdr.nhdr + 24) ;*/
        int *const nwfid = 0; /*(int*)((char*)cmhdr.nhdr + 44) ;*/
	int *const nxsize = 0; /*(int*)((char*)cmhdr.nhdr + 48) ;*/
	int *const nysize = 0; /*(int*)((char*)cmhdr.nhdr + 52) ;*/
	int *const nzdttm = 0; /*(int*)cmhdr.nhdr;*/
	int *const nzhour = 0; /*(int*)((char*)cmhdr.nhdr + 8) ;*/
	int *const nzjday = 0; /*(int*)((char*)cmhdr.nhdr + 4) ;*/
	int *const nzmin = 0; /*(int*)((char*)cmhdr.nhdr + 12) ;*/
	int *const nzmsec = 0; /*(int*)((char*)cmhdr.nhdr + 20) ;*/
	int *const nzsec = 0; /*(int*)((char*)cmhdr.nhdr + 16) ;*/
	int *const nzyear = 0; /*(int*)cmhdr.nhdr;*/
	float *const o = 0; /*(float*)((char*)cmhdr.fhdr + 28) ;*/
	float *const odelta = 0; /*(float*)((char*)cmhdr.fhdr + 16) ;*/
	float *const origin = 0; /*(float*)((char*)cmhdr.fhdr + 28) ;*/
	float *const resp0 = 0; /*(float*)((char*)cmhdr.fhdr + 84) ;*/
	float *const resp1 = 0; /*(float*)((char*)cmhdr.fhdr + 88) ;*/
	float *const resp2 = 0; /*(float*)((char*)cmhdr.fhdr + 92) ;*/
	float *const resp3 = 0; /*(float*)((char*)cmhdr.fhdr + 96) ;*/
	float *const resp4 = 0; /*(float*)((char*)cmhdr.fhdr + 100) ;*/
	float *const resp5 = 0; /*(float*)((char*)cmhdr.fhdr + 104) ;*/
	float *const resp6 = 0; /*(float*)((char*)cmhdr.fhdr + 108) ;*/
	float *const resp7 = 0; /*(float*)((char*)cmhdr.fhdr + 112) ;*/
	float *const resp8 = 0; /*(float*)((char*)cmhdr.fhdr + 116) ;*/
	float *const resp9 = 0; /*(float*)((char*)cmhdr.fhdr + 120) ;*/
	float *const sb = 0; /*(float*)((char*)cmhdr.fhdr + 216) ;*/
	float *const scale = 0; /*(float*)((char*)cmhdr.fhdr + 12) ;*/
	float *const sdelta = 0; /*(float*)((char*)cmhdr.fhdr + 220) ;*/
	float *const stdp = 0; /*(float*)((char*)cmhdr.fhdr + 136) ;*/
	float *const stel = 0; /*(float*)((char*)cmhdr.fhdr + 132) ;*/
	float *const stla = 0; /*(float*)((char*)cmhdr.fhdr + 124) ;*/
	float *const stlo = 0; /*(float*)((char*)cmhdr.fhdr + 128) ;*/
	float *const t0 = 0; /*(float*)((char*)cmhdr.fhdr + 40) ;*/
	float *const t1 = 0; /*(float*)((char*)cmhdr.fhdr + 44) ;*/
	float *const t2 = 0; /*(float*)((char*)cmhdr.fhdr + 48) ;*/
	float *const t3 = 0; /*(float*)((char*)cmhdr.fhdr + 52) ;*/
	float *const t4 = 0; /*(float*)((char*)cmhdr.fhdr + 56) ;*/
	float *const t5 = 0; /*(float*)((char*)cmhdr.fhdr + 60) ;*/
	float *const t6 = 0; /*(float*)((char*)cmhdr.fhdr + 64) ;*/
	float *const t7 = 0; /*(float*)((char*)cmhdr.fhdr + 68) ;*/
	float *const t8 = 0; /*(float*)((char*)cmhdr.fhdr + 72) ;*/
	float *const t9 = 0; /*(float*)((char*)cmhdr.fhdr + 76) ;*/
	float *const time0 = 0; /*(float*)((char*)cmhdr.fhdr + 40) ;*/
	float *const time1 = 0; /*(float*)((char*)cmhdr.fhdr + 44) ;*/
	float *const time2 = 0; /*(float*)((char*)cmhdr.fhdr + 48) ;*/
	float *const time3 = 0; /*(float*)((char*)cmhdr.fhdr + 52) ;*/
	float *const time4 = 0; /*(float*)((char*)cmhdr.fhdr + 56) ;*/
	float *const time5 = 0; /*(float*)((char*)cmhdr.fhdr + 60) ;*/
	float *const time6 = 0; /*(float*)((char*)cmhdr.fhdr + 64) ;*/
	float *const time7 = 0; /*(float*)((char*)cmhdr.fhdr + 68) ;*/
	float *const time8 = 0; /*(float*)((char*)cmhdr.fhdr + 72) ;*/
	float *const time9 = 0; /*(float*)((char*)cmhdr.fhdr + 76) ;*/
	float *const user0 = 0; /*(float*)((char*)cmhdr.fhdr + 160) ;*/
	float *const user1 = 0; /*(float*)((char*)cmhdr.fhdr + 164) ;*/
	float *const user2 = 0; /*(float*)((char*)cmhdr.fhdr + 168) ;*/
	float *const user3 = 0; /*(float*)((char*)cmhdr.fhdr + 172) ;*/
	float *const user4 = 0; /*(float*)((char*)cmhdr.fhdr + 176) ;*/
	float *const user5 = 0; /*(float*)((char*)cmhdr.fhdr + 180) ;*/
	float *const user6 = 0; /*(float*)((char*)cmhdr.fhdr + 184) ;*/
	float *const user7 = 0; /*(float*)((char*)cmhdr.fhdr + 188) ;*/
	float *const user8 = 0; /*(float*)((char*)cmhdr.fhdr + 192) ;*/
	float *const user9 = 0; /*(float*)((char*)cmhdr.fhdr + 196) ;*/
	float *const xmaximum = 0; /*(float*)((char*)cmhdr.fhdr + 240) ;*/
	float *const xminimum = 0; /*(float*)((char*)cmhdr.fhdr + 236) ;*/
	float *const ymaximum = 0; /*(float*)((char*)cmhdr.fhdr + 248) ;*/
	float *const yminimum = 0; /*(float*)((char*)cmhdr.fhdr + 244) ;*/
        float *const swapHeaderByte = 0; /*(float*)((char*)cmhdr.fhdr + 252) ;*/


		/* OFFSET Vectors w/subscript range: 1 to dimension */
	float *const Exthdr = &cmhdr.exthdr[0] - 1;
	float *const Fhdr = &cmhdr.fhdr[0] - 1;
	int *const Ihdr = &cmhdr.ihdr[0] - 1;
	int *const Lhdr = &cmhdr.lhdr[0] - 1;
	int *const Nhdr = &cmhdr.nhdr[0] - 1;
	int *const Niv = &cmhdr.niv[0] - 1;

/*	int *const Nzdttm = &nzdttm[0] - 1; */
	int *const Nzdttm = (int*)&cmhdr.nhdr[0] - 1;
#else
extern float *const a;
extern float *const arrivl;
extern float *const az;
extern float *const b;
extern float *const baz;
extern float *const begin;
extern float *const cmpaz;
extern float *const cmpinc;
extern float *const delta;
extern float *const depmax;
extern float *const depmen;
extern float *const depmin;
extern float *const depmn;
extern float *const depmx;
extern float *const dist;
extern float *const e;
extern float *const ennd;
extern float *const evdp;
extern float *const evel;
extern float *const evla;
extern float *const evlo;
extern float *const f;
extern float *const mag;	/* magnitude.  maf 970205 */
extern float *const fhdr64;
extern float *const fhdr65;
extern float *const fhdr66;
extern float *const fhdr67;
extern float *const fhdr68;
extern float *const fhdr69;
extern float *const fhdr70;
extern float *const fini;
extern float *const fmean;
extern float *const fmt;
extern float *const gcarc;
extern int *const ia;
extern int *const iacc;
extern int *const iamph;
extern int *const ib;
extern int *const ichem;
extern int *const iday;
extern int *const idep;
extern int *const idisp;
extern int *const idown;
extern int *const idrop;
extern int *const ieast;
extern int *const ievreg;
extern int *const ievtyp;
extern int *const iftype;
extern int *const iglch;
extern int *const igood;
extern int *const imagtyp;		/* magnitude type. maf 970205 */
extern int *const imagsrc;		/* magnitude source. maf 970205 */
extern int *const ihdr13;
extern int *const ihdr14;
extern int *const ihdr15;
extern int *const ihdr16;
extern int *const ihdr17;
extern int *const ihdr18;
extern int *const ihdr19;
extern int *const ihdr20;
extern int *const ihdr4;
extern int *const ihglp;
extern int *const ihorza;
extern int *const iinst;
extern int *const illlbb;
extern int *const ilowsn;
extern int *const imb;		/* these 20 added to support */
extern int *const ims;		/* magnitude. maf 970205 */
extern int *const iml;
extern int *const imw;
extern int *const imd;
extern int *const imx;
extern int *const ineic;
extern int *const ipdeq;
extern int *const ipdew;
extern int *const ipde;
extern int *const iisc;
extern int *const ireb;
extern int *const iusgs;
extern int *const ibrk;
extern int *const icaltech;
extern int *const illnl;
extern int *const ievloc;
extern int *const ijsop;
extern int *const iuser;
extern int *const iunknown;
extern int *const inorth;
extern int *const inucl;
extern int *const io;
extern int *const iother;
extern int *const ipostn;
extern int *const ipostq;
extern int *const ipren;
extern int *const ipreq;
extern int *const iquake;
extern int *const iqual;
extern int *const iradev;
extern int *const iradnv;
extern int *const irldta;
extern int *const irlim;
extern int *const isro;
extern int *const istreg;
extern int *const isynth;
extern int *const it0;
extern int *const it1;
extern int *const it2;
extern int *const it3;
extern int *const it4;
extern int *const it5;
extern int *const it6;
extern int *const it7;
extern int *const it8;
extern int *const it9;
extern int *const itanev;
extern int *const itannv;
extern int *const itime;
extern int *const iunkn;
extern int *const iup;
extern int *const ivel;
extern int *const ivolts;
extern int *const iwwsn1;
extern int *const iwwsn2;
extern int *const ixy;
extern int *const ixyz;
extern int *const iztype;
extern int *const iqb;		/* These 17 for */
extern int *const iqb1;		/* ievtyp.  maf 970325 */
extern int *const iqb2;
extern int *const iqbx;
extern int *const iqmt;
extern int *const ieq;
extern int *const ieq1;
extern int *const ieq2;
extern int *const ime;
extern int *const iex;
extern int *const inu;
extern int *const inc;
extern int *const io_;
extern int *const il;
extern int *const ir;
extern int *const it;
extern int *const iu;
extern int *const ieq3;           /* These 9 for ievtype to keep */
extern int *const ieq0;           /* up with database.  maf 970325 */
extern int *const iex0;
extern int *const iqc;
extern int *const iqb0;
extern int *const igey;
extern int *const ilit;
extern int *const imet;
extern int *const iodor;

extern char *const kstnm;
extern char *const kevnm;
extern char *const khole;
extern char *const ko;
extern char *const ka;
extern char *const kt0;
extern char *const kt1;
extern char *const kt2;
extern char *const kt3;
extern char *const kt4;
extern char *const kt5;
extern char *const kt6;
extern char *const kt7;
extern char *const kt8;
extern char *const kt9;
extern char *const kf;
extern char *const kuser0;
extern char *const kuser1;
extern char *const kuser2;
extern char *const kcmpnm;
extern char *const knetwk;
extern char *const kdatrd;
extern char *const kinst;

extern int *const lcalda;
extern int *const leven;
extern int *const lhdr5;
extern int *const lovrok;
extern int *const lpspol;
extern int *const nevid;
extern int *const nhdr15;
extern int *const norid;
extern int *const npts;
extern int *const nsnpts;
extern int *const nvhdr;
extern int *const nwfid;
extern int *const nxsize;
extern int *const nysize;
extern int *const nzdttm;
extern int *const nzhour;
extern int *const nzjday;
extern int *const nzmin;
extern int *const nzmsec;
extern int *const nzsec;
extern int *const nzyear;
extern float *const o;
extern float *const odelta;
extern float *const origin;
extern float *const resp0;
extern float *const resp1;
extern float *const resp2;
extern float *const resp3;
extern float *const resp4;
extern float *const resp5;
extern float *const resp6;
extern float *const resp7;
extern float *const resp8;
extern float *const resp9;
extern float *const sb;
extern float *const scale;
extern float *const sdelta;
extern float *const stdp;
extern float *const stel;
extern float *const stla;
extern float *const stlo;
extern float *const t0;
extern float *const t1;
extern float *const t2;
extern float *const t3;
extern float *const t4;
extern float *const t5;
extern float *const t6;
extern float *const t7;
extern float *const t8;
extern float *const t9;
extern float *const time0;
extern float *const time1;
extern float *const time2;
extern float *const time3;
extern float *const time4;
extern float *const time5;
extern float *const time6;
extern float *const time7;
extern float *const time8;
extern float *const time9;
extern float *const user0;
extern float *const user1;
extern float *const user2;
extern float *const user3;
extern float *const user4;
extern float *const user5;
extern float *const user6;
extern float *const user7;
extern float *const user8;
extern float *const user9;
extern float *const xmaximum;
extern float *const xminimum;
extern float *const ymaximum;
extern float *const yminimum;
extern float *const swapHeaderByte;
extern float *const Exthdr;
extern float *const Fhdr;
extern int *const Ihdr;
extern int *const Lhdr;
extern int *const Nhdr;
extern int *const Niv;
extern int *const Nzdttm;
#endif

