
/* needs to be in an include file */
#define MAXDATA 1000000
#define MAXBANDS 50
#define MAXCHANNELS 50
#define SCRATCH_SIZE 100000
#define NPTS_GF 5000
#define START_GF 1
#define DELTA_GF 1.0
#define START_WINDOW_ADD 20
#define MAX_RESIDUAL .05
#define RHO 2.7
#define BETA 3.5e5
#define MU 3.3e5
#define MB_SLOPE 0.71
#define MB_INTERCEPT 17.5
#define MAX_MOMENT_COUNT 2
#define MW_SLOPE 0.66666
#define MW_INTERCEPT 10.7
#define FIT_FUNC_MULT 0.000001
#define FIT_FUNC_SUB  100
#define FIT_INITIAL_LENGTH 200.
#define FIT_NPTS_POW 1.4
#define NOISE_START 0.0
#define NOISE_LENGTH 10.0
#define MINIMUM_SNR 0.0
#define YVPMAX 30.0
#define YVPMIN 0.0
#define MAX_TIME_ADJUSTMENT 5.0

//different window options
#define WINDOW_FROM_DEFAULT 0
#define WINDOW_FROM_FIT 1
#define WINDOW_FROM_INTERACTIVE 2
#define WINDOW_FROM_PICKFILE 3
#define WINDOW_NOPICK 4

struct envelope{
     float       freq_low;
     float       freq_high;
     float       *envelope_data;
     int         number_of_points;
     int         window_start;
     int         window_stop;
     float       window_start_seconds;
     float       coda_amplitude;
     float       *GFenvelope;
     float       GFstart;
     int         GFnpts;
     float       GFdelta;
     float       b0;
     float       b1;
     float       b2;
     float       vel0;
     float       vel1;
     float       vel2;
     float       gamma0;
     float       gamma1;
     float       gamma2;
     float       path_p1;
     float       path_p2;
     float       tnorm;
     float       min_length;
     float       max_length;
     float       minimum_SNR;
     float       max_residual;
     float       noise_thresh;
     float       Moment_correction;
     int         fit_npoints;
     int         fit_window_picked;
     float       picked_window;
     float       picked_window_start;
     float       CodaAmp;
     float       Dist_Corrected_CodaAmp;
     float       fit_residual;
     float       fit_SNR;
     float       fit_b;
     float       fit_gamma;
     float       time_adjustment;
     float       Moment;
     float       Mb_weight;
     char        ResidStat[2];
     char        SNRStat[2];
   };
struct g_params{
  //input parameters
     float rho;
     float Beta;
     float mu;
     float Mb_slope;
     float Mb_intercept;
     float Mw_slope;
     float Mw_intercept;
     int   Max_Moment_count;
  //measurements
     float Moment;
     float Mw;
     float Mb;
     float Energy_Obs;
     float Energy_High;
     float Energy_Low;     
     float Energy_Total;
     float Stress_Total;
     float Max_Residual;
     float Minimum_SNR;

   };
int nerr;

