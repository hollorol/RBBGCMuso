/*

This is a slitely modified version of mtclim prepared to be part of RBBGCMuso.
modification is done by: Roland Hollos (hollorol@gmail.com)


mtclim43.c

MTCLIM
VERSION 4.3  

Peter Thornton
NTSG, School of Forestry
University of Montana
1/20/00

***************************************
** Questions or comments? Contact... **
** Dr. Peter E. Thornton             **
** NTSG, School of Forestry          **
** University of Montana             **
** Missoula, MT 59812                **
** email: peter@ntsg.umt.edu         **
** phone: 406-243-4326               **
***************************************

--------------------------------------------------------------------------
Code History
------------
Original code written by R.R. Nemani
Updated   4/1/1989 by J.C. Coughlan
Updated 12/23/1989 by Joe Glassy
Updated   1/4/1993 by Raymond Hunt (version 2.1)
Updated  3/26/1997 by Peter Thornton (version 3.0)
Updated  7/28/1997 by Peter Thornton (version 3.1)
Updated   5/7/1998 by Peter Thornton (version 4.0)
Updated   8/1/1998 by Peter Thornton (version 4.1) 
Updated  4/20/1999 by Peter Thornton (version 4.2) 
Updated  1/20/2000 by Peter Thornton (version 4.3)

CHANGES FROM VERSION 4.2 TO VERSION 4.3
These changes based on the results of:
Thornton, P.E., H. Hasenauer, and M.A. White, 2000. Simultaneous
estimation of daily solar radiation and humidity from observed temperature
and precipitation: an application over complex terrain in Austria.
For submission to Ag For Met.
1) ADDED THE SNOW ACCUMULATION/MELT MODEL FOR CORRECTING RADIATION 
FOR INFLUENCE OF SNOWPACK.
2) ADDED ALBEDO CORRECTION TO HORIZON OBSTRUCTION CALCULATIONS.
3) HUMIDITY ALGORITHM NOW DEPENDS ON THE ANNUAL RATION PET/PRCP:
FOR VALUES < 2.5, USING THE TMIN=TDEW ALGORITHM, FOR VALUES > 2.5,
USING THE ARID CORRECTION FROM:
Kimball, J.S., S.W. Running, and R. Nemani, 1997. An improved method for
estimating surface humidity from daily minimum temperature. Ag For Met
85:87-98. 
4) MOVED ALL MODEL PARAMETERS INTO THE PARAMETERS.H FILE

CHANGES FROM VERSION 4.1 TO VERSION 4.2
1) PUT THE SLOPE AND ASPECT CORRECTIONS TO RADIATION BACK IN. THEY
HAD BEEN REMOVED DURING THE DEVELOPMENT OF THE NEW RADIATION CODE.
This includes the estimation for diffuse radiation on sloping surfaces
during periods when the sun is above a flat horizon, but not providing
direct illumination to the slope.  Site east and west horizon corrections
have also been reintroduced.

CHANGES FROM VERSION 4.0 TO VERSION 4.1
1) ADDITIONAL REVISION OF RADIATION CODE, FOLLOWING SUBMISSION OF AG FOR MET
MANUSCRIPT DESCRIBING ANALYSIS OF SAMSON DATA.  The current code
follows exactly the algorithm detailed in
Thornton, P.E. and S.W. Running, 1999. An improved algorithm for estimating 
incident daily solar radiation from measurements of temperature, humidity,
and precipitation. Ag For Met 93:211-228.

changes from version 3.1 to version 4.0:
1) radiation code completely revamped, following analysis of samson
observations for the vemap2 project.
2) includes an iterative algorithm for estimating vapor pressure and
radiation
3) par output no longer an option, since the old par algorithm is suspect
4) 2-day tmin smoothing no longer an option
5) solar constant now calculated as an analytical function of yearday,
instead of the monthly array of values in earlier versions. removes the
discontinuities between months. 
6) boxcar function now uses the previous n days of data, making it a
"pulled boxcar" instead of a "centered boxcar"

Changes from version 2.0 to version 3.1
Modified to include the following improvements to the original MTCLIM logic:
1) Improved vapor pressure calculation
2) Improved transmissivity calculation
3) Improved daylength calculation

Some other differences between version 3.1 and previous versions of MTCLIM:
1) No english units option
2) No total or average radiation option
3) No threshold radiation option
5) No pre-rainy days correction for transmissivity
6) No radiation correction to air temperatures
7) No LAI corrections to air temperatures
8) Only one precipitation station allowed
9) Some parameters formerly in initialization file are now in mtclim_const.h

--------------------------------------------------------------------------
UNITS
-----
Temperatures           degrees C
Temp. lapse rates      degrees C / 1000 m
Precipitation          cm / day
Vapor pressure         Pa
Vapor pressure deficit Pa
Radiation              W/m2, average over daylight period
Daylength              s (sunrise to sunset, flat horizons) 
Elevation              m
Latitude               decimal degrees
Aspect                 decimal degrees
Slope                  decimal degrees
E/W horizons           decimal degrees

--------------------------------------------------------------------------
Files
-----

Parameter initialization file
Input meteorological data file
Output meteorological data file  (*.mtcout)

Example initialization file:
 
---top of init file-------------------------------------------------------
sample               (this entire line written to outfiles)
other comments       (this entire line discarded)

IOFILES                    Keyword, don't edit this line 
test.mtcin                 Name of input meteorological data file
test                       Prefix for output file
                      
CONTROL                    Keyword, don't edit this line
3                          (int) Number of header lines in input file
365                        (int) Number of days of data in input file
0                          (int) Dewpoint temperature input? (0=NO, 1=YES)
1                          (int) Humidity output flag        (0=VPD, 1=VP)
1                          (int) Year field in input file?   (0=NO, 1=YES)

PARAMETERS                 Keyword, don't edit this line
500.0                      (double) Base station elevation, meters
50.0                       (double) Base station annual precip isohyet, cm
48.0                       (double) Site latitude, degrees (- for south)
1500.0                     (double) Site elevation, meters
15.0                       (double) Site slope, degrees
180.0                      (double) Site aspect, degrees (0=N,90=E,180=S,270=W)
75.0                       (double) Site annual precip isohyet, cm
2.0                        (double) Site east horizon, degrees
5.0                        (double) Site west horizon, degrees
-6.0                       (double) Lapse rate for max temperature, deg C/1000m
-3.0                       (double) Lapse rate for min temperature, deg C/1000m

END                        Keyword, don't edit this line
---end of init file-------------------------------------------------------

*.init FILE INFO
For all lines, except the header and comment lines, the  parameter value on the
left can be followed by comments on the right, as long as there is whitespace
separating the value on the left from the following comment. Blank lines can be
inserted or deleted, but all keyword lines and parameter lines must be
included.  The order and number of non-blank lines in this file is crucial. The
keywords are there as a rudimentary quality check on the format of the
initialization file, and they ensure that the proper number of lines are read.
They DON'T ensure that the parameters are in the proper order.

NOTE: The dashed lines at the top and bottom of the example file shown above
are NOT part of the file.


INPUT FILE INFO
All input temperatures are in degrees Celcius, and precipitation is in 
centimeters.
Input data file format:
<some number of header lines (can be zero)>
<year (optional)> <yearday> <Tmax> <Tmin> <Tdew (optional)> <precipitation>
.
.
.

Example input data file... without year field, and without dewpoint temperature
---start of input data file --------------
This is a header line, which is discarded
101 16.0 2.0 1.2
102 17.0 3.0  0.1
103 16.5 5.2  0.0
104 20.1 6.4  0.0
---end of input data file ----------------



OUTPUT FILE INFO
The output file is created by appending ".mtcout" to the output filename
prefix specified in the initialization file. Existing files are overwritten,
so be careful to rename files between runs if you want to save the results, 
and for safety, don't use ".mtcout" as the extension for the input data file.

******************************
Input and Output CONTROL FLAGS
******************************
There is a flag in the initialization file that controls the input of dewpoint
temperature information. If your input file does not contain dewpoint data,
set this flag to 0. Otherwise, if you have dewpoint temperature information 
in your input file, set this flag to 1, and be sure that the dewpoint
temperature field is between the tmin and prcp fields, as specified under
the heading "INPUT FILE INFO", above.

There is another flag in the initialization file that controls the output of
humidity information. When set to 0, humidity output is the vapor pressure
deficit from the saturated vapor pressure at the daylight average temperature
to the ambient vapor pressure, in Pa. When the flag is set to 1, the output is
simply the  ambient vapor pressure in Pa.  By using the ambient vapor pressure
(flag  set to 1), you can avoid possible errors related to a difference between
the temperature chosen for saturation calculations (in this case tday) and the
actual temperature at any given time of day.

Another flag controls the treatment of a year-field in the input and output
files. When the flag is set (1), it is assumed that the first field in the
input file contains the year, which can be any integer between -32000 and
32000 (approx). This field is simply copied line-for-line into the output
file, where it preceeds the standard yearday output field.

Example output data file... (using VPD output, no PAR, no year field)
---start of output data file -----------------------------------
MTCLIM v3.1 OUTPUT FILE : Mon Mar 24 14:50:51 1997
MTCLIM version 3.1 testing
  yday    Tmax    Tmin    Tday    prcp      VPD     srad  daylen
       (deg C) (deg C) (deg C)    (cm)     (Pa)  (W m-2)     (s)
   101   10.00   -0.50    7.39    1.80   439.51   379.95   47672
   102   11.00    1.10    7.67    0.15   387.27   364.95   47880
   103   10.50    2.80    6.84    0.00   243.78   302.15   48087
   104   14.10    3.40    9.29    0.00   390.67   390.22   48293
---end of output data file -------------------------------------


*/

/*********************
**                  **
**  START OF CODE   **
**                  **
*********************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "ini.h"                  /* file structure and I/O routines */
#include "mtclim43_constants.h"   /* physical constants */
#include "mtclim43_parameters.h"  /* model parameters */

/****************************
**                         ** 
**  STRUCTURE DEFINITIONS  **
**                         **
****************************/
typedef struct
{
	file init;             /* initialization file */
	file inmet;            /* input meteorological data file */
	file outmet;           /* output meteorological data file */
	int nhead;             /* number of header lines in input met file */
	int ndays;             /* number of days of data in input file */
	int indewpt;           /* input dewpoint temperature flag (0=NO, 1=YES) */
	int outhum;            /* output humidity flag            (0=VPD, 1=VP) */
	int inyear;            /* input year flag                 (0=NO, 1=YES) */
	char systime[100];     /* system time at start of simulation */
    char header[200];      /* header string, written to all output files */
    char outprefix[200];   /* output filename prefix */
} control_struct;

typedef struct
{
	double base_elev;      /* base elevation, meters */
	double base_isoh;      /* base annual precip isohyet, cm */
	double site_lat;       /* site latitude, dec. degrees (- for south) */
	double site_elev;      /* site elevation, meters */
	double site_slp;       /* site slope, degrees */
	double site_asp;       /* site aspect, degrees */
	double site_isoh;      /* site annual precip isohyet, cm */
	double site_ehoriz;    /* site east horizon, degrees */
	double site_whoriz;    /* site west horizon, degrees */
	double tmax_lr;        /* maximum temperature lapse rate, deg C/1000m */
	double tmin_lr;        /* minimum temperature lapse rate, deg C/1000m */
} parameter_struct;

typedef struct
{
	int *year;             /* array of year values */
	int *yday;             /* array of yearday values */
	double *tmax;          /* array of base maximum temperature values */
	double *tmin;          /* array of base minimum temperature values */
	double *prcp;          /* array of base daily precipitation values */
	double *tdew;          /* array of base dewpoint temperature values */
	double *s_tmax;        /* array of site tmax values */
	double *s_tmin;        /* array of site tmin values */
	double *s_tday;        /* array of site daylight temperature values */
	double *s_prcp;        /* array of site prcp values */
	double *s_hum;         /* array of site humidity values (VPD or VP, Pa) */
	double *s_srad;        /* array of site shortwave radiation values */
	double *s_dayl;        /* array of site daylength values */
	double *s_swe;         /* array of site snow water equivalent values (cm) */
} data_struct;
	
/********************************
**                             **
**    FUNCTION PROTOTYPES      **
**                             **
********************************/
int read_init(control_struct *ctrl, parameter_struct *p);
int data_alloc(const control_struct *ctrl, data_struct *data);
int read_inmet(const control_struct *ctrl, data_struct *data);
int calc_tair(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data);
int calc_prcp(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data);
int snowpack(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data);
int calc_srad_humidity(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data);
int calc_srad_humidity_iterative(const control_struct *ctrl,
	const parameter_struct *p, data_struct *data);
int write_out(const control_struct *ctrl, const data_struct *data);
int data_free(const control_struct *ctrl, data_struct *data);
double calc_pet(double rad, double ta, double pa, double dayl);
double atm_pres(double elev);
int pulled_boxcar(double *input,double *output,int n,int w,int w_flag);
	
/********************************
**                             **
**      START OF MAIN()        **
**                             **
********************************/
void mtc(char *argv)
{
  int argc=2;
	/* variable declarations */
	control_struct ctrl; /* iofiles and program control variables */
	parameter_struct p;  /* site and base parameters */
	data_struct data;    /* site and base meteorological data arrays */
	
	/* inital progress indicator */
	printf("Starting MTCLIM version 4.3\n");
	
	/* read the name of the initialization file from the command line
	and store as init.name */
	if (argc != 2)
	{
		printf("usage: <executable name>  <initialization file name>\n");
		exit(1);
	}
	strcpy(ctrl.init.name, argv);
	
	/* read initialization file, open input files, do basic error checking
	on input parameters, and open output file */
	if (read_init(&ctrl, &p))
	{
		printf("Error in read_init()... exiting\n");
		exit(1);
	}
	printf("Completed read_init()\n");
	
	/* allocate space in the data arrays for input and output data */
	if (data_alloc(&ctrl, &data))
	{
		printf("Error in data_alloc()... exiting\n");
		exit(1);
	}
	printf("Completed data_alloc()\n");
	
	/* read meteorological data from input file into data arrays */
	if (read_inmet(&ctrl, &data))
	{
		printf("Error in read_inmet()... exiting\n");
		exit(1);
	}
	printf("Completed read_inmet()\n");
	
	/* estimate daily air temperatures */
	if (calc_tair(&ctrl, &p, &data))
	{
		printf("Error in calc_tair()... exiting\n");
		exit(1);
	}
	printf("Completed calc_tair()\n");
	
	/* estimate daily precipitation */
	if (calc_prcp(&ctrl, &p, &data))
	{
		printf("Error in calc_prcp()... exiting\n");
		exit(1);
	}
	printf("Completed calc_prcp()\n");

	/* estimate daily snowpack */
	if (snowpack(&ctrl, &p, &data))
	{
		printf("Error in snowpack()... exiting\n");
		exit(1);
	}
	printf("Completed snowpack()\n");

	/* test for the presence of Tdew observations, and branch to the
	appropriate srad and humidity algorithms */
	if (ctrl.indewpt)
	{
		/* estimate srad and humidity using real Tdew data */
		if (calc_srad_humidity(&ctrl, &p, &data))
		{
			printf("Error in calc_srad_humidity()... exiting\n");
			exit(1);
		}
		printf("Completed calc_srad_humidity()\n");
		
	}
	else /* no dewpoint temperature data */
	{	
		/* estimate srad and humidity with iterative algorithm */
		if (calc_srad_humidity_iterative(&ctrl, &p, &data))
		{
			printf("Error in calc_srad_humidity_iterative()... exiting\n");
			exit(1);
		}
		printf("Completed calc_srad_humidity_iterative()\n");
	}
	
	/* write output file */
	if (write_out(&ctrl, &data))
	{
		printf("Error in write_out()... exiting\n");
		exit(1);
	}
	printf("Completed write_out()\n");
	
	/* free previously allocated memory before returning */
	if (data_free(&ctrl, &data))
	{
		printf("Error in data_free()... exiting\n");
		exit(1);
	}
	printf("Completed data_free()\n");
	printf("Output filename: %s\n",ctrl.outmet.name);
	
}
/* end of main */

/****************************
**                         **
**    START OF FUNCTION    **
**       DEFINITIONS       **
**                         **
****************************/
/* read_init() reads data out of init file, opens input and output files */
int read_init(control_struct *ctrl, parameter_struct *p)
{
	struct tm *tm_ptr;
	time_t lt;
	int ok = 1;
	char temp[200];
	char key1[] = "IOFILES";
	char key2[] = "CONTROL";
	char key3[] = "PARAMETERS";
	char key4[] = "END";
	char keyword[80];
	char comment[200];

	/* open the main init file for ascii read and check for errors */
	if (file_open(&(ctrl->init),'i'))
	{
		printf("Error opening init file: %s\n",ctrl->init.name);
		ok = 0;
	}
	
	/* get the system time */
	if (ok)
	{
		lt = time(NULL);
		tm_ptr = localtime(&lt);
		strcpy(ctrl->systime,asctime(tm_ptr));
	}

	/* read the header line and the comment line from init file */
	if (ok && fgets(ctrl->header, 200, ctrl->init.ptr)==NULL)
	{
		printf("Error reading header line\n");
		ok = 0;
	}
	if (ok && fgets(comment, 200, ctrl->init.ptr)==NULL)
	{
		printf("Error reading comment line\n");
		ok = 0;
	}
	
	/* scan for the IOFILES keyword, exit if not next */
	if (ok && scan_value(ctrl->init, keyword, 's'))
	{
		printf("Error reading IOFILES keyword\n");
		ok=0;
	}
	if (ok && strcmp(keyword, key1))
	{
		printf("Expecting keyword %s in file %s\n",key1,ctrl->init.name);
		ok=0;
	}
	/* read input data filename and open the input data file for ascii read */
	if (ok && scan_open(ctrl->init, &ctrl->inmet, 'i'))
	{
		printf("Error opening input file \n");
		ok=0;
	}
	/* read the output filename prefix */
	if (ok && scan_value(ctrl->init, ctrl->outprefix, 's'))
	{
		printf("Error reading output file prefix\n");
		ok=0;
	}

	/* scan for the CONTROL keyword, exit if not next */
	if (ok && scan_value(ctrl->init, keyword, 's'))
	{
		printf("Error reading CONTROL keyword\n");
		ok=0;
	}
	if (ok && strcmp(keyword, key2))
	{
		printf("Expecting keyword %s in file %s\n", key2, ctrl->init.name);
		ok=0;
	}
	/* number of header lines in input data file */
	if (ok && scan_value(ctrl->init, &ctrl->nhead, 'i'))
	{
		printf("Error reading number of header lines\n");
		ok=0;
	}
	/* total number of days in input data file */
	if (ok && scan_value(ctrl->init, &ctrl->ndays, 'i'))
	{
		printf("Error reading number of days\n");
		ok=0;
	}
	/* read the binary flag for dewpoint data in input file */
	if (ok && scan_value(ctrl->init, &ctrl->indewpt, 'i'))
	{
		printf("Error reading input dewpoint flag\n");
		ok=0;
	}
	/* read the binary flag for humidity output format
	0 = VPD, 1 = VP */
	if (ok && scan_value(ctrl->init, &ctrl->outhum, 'i'))
	{
		printf("Error reading output humidity flag\n");
		ok=0;
	}
	/* read the binary flag for year field in input file */
	if (ok && scan_value(ctrl->init, &ctrl->inyear, 'i'))
	{
		printf("Error reading year field flag\n");
		ok=0;
	}
		
	/* scan for the PARAMETERS keyword, exit if not next */
	if (ok && scan_value(ctrl->init, keyword, 's'))
	{
		printf("Error reading PARAMETERS keyword\n");
		ok=0;
	}
	if (ok && strcmp(keyword, key3))
	{
		printf("Expecting keyword %s in file %s\n", key3, ctrl->init.name);
		ok=0;
	}
	/* base station elevation */
	if (ok && scan_value(ctrl->init, &p->base_elev, 'd'))
	{
		printf("Error reading base elevation\n");
		ok=0;
	}
	/* base station annual isohyet */
	if (ok && scan_value(ctrl->init, &p->base_isoh, 'd'))
	{
		printf("Error reading base station isohyet\n");
		ok=0;
	}
	/* prediction site latitude */
	if (ok && scan_value(ctrl->init, &p->site_lat, 'd'))
	{
		printf("Error reading prediction site latitude\n");
		ok=0;
	}
	/* prediction site elevation */
	if (ok && scan_value(ctrl->init, &p->site_elev, 'd'))
	{
		printf("Error reading prediction site elevation\n");
		ok=0;
	}
	/* prediction site slope */
	if (ok && scan_value(ctrl->init, &p->site_slp, 'd'))
	{
		printf("Error reading prediction site slope\n");
		ok=0;
	}
	/* prediction site aspect */
	if (ok && scan_value(ctrl->init, &p->site_asp, 'd'))
	{
		printf("Error reading prediction site aspect\n");
		ok=0;
	}
	/* predictions site annual isohyet */
	if (ok && scan_value(ctrl->init, &p->site_isoh, 'd'))
	{
		printf("Error reading prediction site isohyet\n");
		ok=0;
	}
	/* prediction site east horizon elevation angle */
	if (ok && scan_value(ctrl->init, &p->site_ehoriz, 'd'))
	{
		printf("Error reading prediction site east horizon\n");
		ok=0;
	}
	/* prediction site west horizon elevation angle */
	if (ok && scan_value(ctrl->init, &p->site_whoriz, 'd'))
	{
		printf("Error reading prediction site west horizon\n");
		ok=0;
	}
	/* maximum tempreature lapse rate */
	if (ok && scan_value(ctrl->init, &p->tmax_lr, 'd'))
	{
		printf("Error reading lapse rate for maximum temperature\n");
		ok=0;
	}
	/* minimum temperature lapse rate */
	if (ok && scan_value(ctrl->init, &p->tmin_lr, 'd'))
	{
		printf("Error reading lapse rate for minimum temperature\n");
		ok=0;
	}

	/* scan for the END keyword, exit if not next */
	if (ok && scan_value(ctrl->init, keyword, 's'))
	{
		printf("Error reading END keyword\n");
		ok=0;
	}
	if (ok && strcmp(keyword, key4))
	{
		printf("Expecting keyword %s in file %s\n", key4, ctrl->init.name);
		ok=0;
	}
	
	/* if all data read successfully, do basic error checking on parameters */
	if (ok)
	{
		/* error checking for control parameters */
		/* make sure total number of days is > 0 */
		if (ctrl->ndays <= 0) 
		{
			printf("ERROR: number of days must be > 0 ...\n");
			ok=0;
		}
		/* make sure number of header lines is > 0 */
		if (ctrl->nhead < 0)
		{
			printf("ERROR: number of header lines must be >= 0 ...\n");
			ok=0;
		}
		/* check dewpoint input flag */
		if ((ctrl->indewpt < 0) || (ctrl->indewpt > 1))
		{
			printf("WARNING: input dewpoint flag should be 0 or 1: assuming dewpoint input ...\n");
			ctrl->indewpt = 1;
		}
		/* check humidity output flag */
		if ((ctrl->outhum < 0) || (ctrl->outhum > 1))
		{
			printf("WARNING: output humidity flag should be 0 or 1: assuming vapor pressure output ...\n");
			ctrl->outhum = 1;
		}
		if ((ctrl->inyear < 0) || (ctrl->inyear > 1))
		{
			printf("WARNING: input year field flag should be 0 or 1 ...\n");
			ok=0;
		}
		
		/* error checking for parameters */
		if (p->base_elev > 5000)
		{
			printf("WARNING: base station elev = %.1lf m: be sure to use meters, not feet\n",p->base_elev);
		} 
		if (p->base_isoh <= 0.0)
		{
			printf("ERROR: base station isohyet must be > 0.0 ...\n");
			ok=0;
		}
		if ((p->site_lat < -90.0) || (p->site_lat > 90.0))
		{
			printf("ERROR: site latitude must be in the range -90.0 - 90.0 degrees ...\n");
			ok=0;
		}
		if (p->site_elev > 5000)
		{
			printf("WARNING: site elev = %.1lf m: be sure to use meters, not feet\n",p->site_elev);
		} 
		if (p->site_slp > 60.0)
		{
			printf("WARNING: site slope = %.1lf deg: be sure to use deg, not %%\n",p->site_slp);
		}
		if (p->site_slp < 0)
		{
			printf("ERROR: site slope must be >= 0.0\n");
			ok=0;
		}
		if ((p->site_asp < 0.0) || (p->site_asp > 360.0))
		{
			printf("ERROR: site aspect must be in the range 0.0 - 360.0 degrees ...\n");
			ok=0;
		}
		if (p->site_isoh < 0.0)
		{
			printf("ERROR: site isohyet must be >= 0.0 ...\n");
			ok=0;
		}
		if ((p->site_ehoriz < 0.0) || (p->site_ehoriz > 180.0))
		{
			printf("ERROR: site east horizon must be in the range 0.0 - 180.0 degrees ...\n");
			ok=0;
		}
		if ((p->site_whoriz < 0.0) || (p->site_whoriz > 180.0))
		{
			printf("ERROR: site west horizon must be in the range 0.0 - 180.0 degrees ...\n");
			ok=0;
		}
		if (p->tmax_lr > 0.0)
		{
			printf("WARNING: Tmax lapse rate = %.2lf : This should typically be negative ...\n");
		}
		if (p->tmin_lr > 0.0)
		{
			printf("WARNING: Tmin lapse rate = %.2lf : This should typically be negative ...\n");
		}
	} /* end of basic error checking routine */
	
	/* generate output filename based on outprefix and open file for ascii
	write. Also write date, time, and header information into output file */
	if (ok)
	{
		strcpy(temp,ctrl->outprefix);
		strcat(temp,POSTFIX);
		strcpy(ctrl->outmet.name,temp);
		if (file_open(&ctrl->outmet, 'o'))
		{
			printf("Error opening output file\n");
			ok=0;
		}
	}
	if (ok)
	{
		fprintf(ctrl->outmet.ptr,"%s", ctrl->header);
		fprintf(ctrl->outmet.ptr,"MTCLIM v4.3 OUTPUT FILE : %s", ctrl->systime);
	}
	
	return (!ok);
}
/* end of read_init() */

/* data_alloc() allocates space for input and output data arrays */
int data_alloc(const control_struct *ctrl, data_struct *data)
{
	int ok=1;
	int ndays;
	
	ndays = ctrl->ndays;
	
	if (ok && ctrl->inyear && !(data->year = (int*) malloc(ndays * sizeof(int))))
	{
		printf("Error allocating for year array\n");
		ok=0;
	} 
	if (ok && !(data->yday = (int*) malloc(ndays * sizeof(int))))
	{
		printf("Error allocating for yearday array\n");
		ok=0;
	} 
	if (ok && !(data->tmax = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for tmax array\n");
		ok=0;
	}
	if (ok && !(data->tmin = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for tmin array\n");
		ok=0;
	}
	if (ok && !(data->prcp = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for prcp array\n");
		ok=0;
	}
	if (ok && ctrl->indewpt && !(data->tdew = (double*) malloc(ndays *
		sizeof(double))))
	{
		printf("Error allocating for input humidity array\n");
		ok=0;
	}
	if (ok && !(data->s_tmax = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site Tmax array\n");
		ok=0;
	}
	if (ok && !(data->s_tmin = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site Tmin array\n");
		ok=0;
	}
	if (ok && !(data->s_tday = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site Tday array\n");
		ok=0;
	}
	if (ok && !(data->s_prcp = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site prcp array\n");
		ok=0;
	}
	if (ok && !(data->s_hum = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site VPD array\n");
		ok=0;
	}
	if (ok && !(data->s_srad = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site radiation array\n");
		ok=0;
	}
	if (ok && !(data->s_dayl = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site daylength array\n");
		ok=0;
	}
	if (ok && !(data->s_swe = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for site snowpack array\n");
		ok=0;
	}
	
	return (!ok);
}
/* end of data_alloc() */

/* read_inmet() reads data from the meteorological input file into arrays */	
int read_inmet(const control_struct *ctrl, data_struct *data)
{
	int ok=1;
	
	int i,ndays;
	int id,iy;
	
	char header[200];
	
	id = ctrl->indewpt;
	iy = ctrl->inyear;

	ndays = ctrl->ndays;
	
	/* read header lines and discard */
	for (i=0 ; i<ctrl->nhead ; i++)
	{
		if (ok && fgets(header, 200, ctrl->inmet.ptr)==NULL)
		{
			printf("Error reading input header line\n");
			ok = 0;
		}
	}
		
	/* read lines of data, variable read format depending on flag */
	i = 0;
	while (ok && i<ndays)
	{
		if (iy) /* year field in input file */
		{
			if (fscanf(ctrl->inmet.ptr, "%d",&(data->year[i])) != 1)
			{
				printf("Error reading year from input file line #%d \n", i+1);
				ok=0;
			}
		}
		if (id) /* dewpoint data in input file */
		{
			if (fscanf(ctrl->inmet.ptr, "%d%lf%lf%lf%lf",
				&(data->yday[i]), &(data->tmax[i]), &(data->tmin[i]),
				&(data->tdew[i]), &(data->prcp[i])) != 5)
			{
				printf("Error reading line #%d from input file\n", i+1);
				ok=0;
			}
		}
		else /* no dewpoint data in input file */
		{
			if (fscanf(ctrl->inmet.ptr, "%d%lf%lf%lf",
				&(data->yday[i]), &(data->tmax[i]), &(data->tmin[i]),
				&(data->prcp[i])) != 4)
			{
				printf("Error reading line #%d from input file\n", i+1);
				ok=0;
			}
		}
		
		/* check for valid yearday */
		if (ok && ((data->yday[i] > 366) || data->yday[i] < 1))
		{
			printf("Invalid yearday (%d) at line #%d\n",data->yday[i],i+1);
			ok=0;
		} 
		
		i++;
		
	} /* end while */
	
	/* check for gaps in yearday field */
	i = 1;
	while (ok && i<ndays)
	{
		if (data->yday[i] != 1) /* not January 1 */
		{
			if (data->yday[i] != (data->yday[i-1]+1))
			{
				printf("Gap in yeardays at line #%d\n",i+1);
				ok=0;
			}
		}
		else /* January 1 */
		{
			if ((data->yday[i-1] != 365) && (data->yday[i-1] != 366))
			{
				printf("Gap in yeardays at line #%d\n",i+1);
				ok=0;
			}
		}
		
		i++;
	}
	
	/* check for tmax < tmin */
	i=0;
	while (ok && i<ndays)
	{
		if (data->tmax[i] < data->tmin[i])
		{
			printf("ERROR: Tmax < Tmin on line #%d\n",i+1+ctrl->nhead);
			ok=0;
		}
		i++;
	}
	
	return (!ok);
}
/* end of read_inmet() */

/* calc_tair() calculates daily air temperatures */
int calc_tair(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data)
{
	int ok=1;
	int i,ndays;
	double dz;
	double tmean, tmax, tmin;
	
	ndays = ctrl->ndays;
	/* calculate elevation difference in kilometers */
	dz = (p->site_elev - p->base_elev)/1000.0;
	
	/* apply lapse rate corrections to tmax and tmin */
	/* Since tmax lapse rate usually has a larger absolute value than tmin
	lapse rate, it is possible at high elevation sites for these corrections
	to result in tmin > tmax. Check for that occurrence and force
	tmin = corrected tmax - 0.5 deg C. */
	for (i=0 ; i<ndays ; i++)
	{
		/* lapse rate corrections */
		data->s_tmax[i] = tmax = data->tmax[i] + (dz * p->tmax_lr);
		data->s_tmin[i] = tmin = data->tmin[i] + (dz * p->tmin_lr);
		
		/* derived temperatures */
		tmean = (tmax + tmin)/2.0;
		data->s_tday[i] = ((tmax - tmean)*TDAYCOEF) + tmean;
	}
	
	return (!ok);
}
/* end of calc_tair() */

/* calc_prcp() calculates daily total precipitation */
int calc_prcp(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data)
{
	int ok=1;
	int i,ndays;
	double ratio;
	
	ndays = ctrl->ndays;
	
	ratio = p->site_isoh / p->base_isoh;
	
	if (ok)
	{
		for (i=0 ; i<ndays ; i++)
		{
			data->s_prcp[i] = data->prcp[i] * ratio;
		}
	}
	
	return (!ok);
}
/* end of calc_prcp() */

/* snowpack() estimates the accumulation and melt of snow for radiation
algorithm corrections */
int snowpack(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data)
{
	int ok=1;
	int i,ndays,count;
	int start_yday,prev_yday;
	double snowpack,newsnow,snowmelt,sum;
	
	ndays = ctrl->ndays;
	
	/* first pass to initialize SWE array */
	snowpack = 0.0;
	for (i=0 ; i<ndays ; i++)
	{
		newsnow = 0.0;
		snowmelt = 0.0;
		if (data->s_tmin[i] <= SNOW_TCRIT) newsnow = data->s_prcp[i];
		else snowmelt = SNOW_TRATE * (data->s_tmin[i] - SNOW_TCRIT);
		snowpack += newsnow - snowmelt;
		if (snowpack < 0.0) snowpack = 0.0;
		data->s_swe[i] = snowpack;
	}
	
	/* use the first pass to set the initial snowpack conditions for the
	first day of data */
	start_yday = data->yday[0];
	if (start_yday == 1) prev_yday = 365;
	else prev_yday = start_yday-1;
	count = 0;
	sum = 0.0;
	for (i=1 ; i<ndays ; i++)
	{
		if (data->yday[i] == start_yday || data->yday[i] == prev_yday)
		{
			count ++;
			sum += data->s_swe[i];
		}
	}
	/* Proceed with correction if there are valid days to reinitialize
	the snowpack estiamtes. Otherwise use the first-pass estimate. */
	if (count)
	{
		snowpack = sum/(double)count;
		for (i=0 ; i<ndays ; i++)
		{
			newsnow = 0.0;
			snowmelt = 0.0;
			if (data->s_tmin[i] <= SNOW_TCRIT) newsnow = data->s_prcp[i];
			else snowmelt = SNOW_TRATE * (data->s_tmin[i] - SNOW_TCRIT);
			snowpack += newsnow - snowmelt;
			if (snowpack < 0.0) snowpack = 0.0;
			data->s_swe[i] = snowpack;
		}
	}
		
	return (!ok);
}
/* end of snowpack() */

/* when dewpoint temperature observations are available, radiation and
humidity can be estimated directly */
int calc_srad_humidity(const control_struct *ctrl, const parameter_struct *p, 
	data_struct *data)
{
	int ok=1;
	int i,ndays;
	double pva,pvs,vpd;
	int ami,yday;
	double ttmax0[366];
	double flat_potrad[366];
	double slope_potrad[366];
	double daylength[366];
	double *dtr, *sm_dtr;
	double tmax,tmin;
	double t1,t2;
	double pratio;
	double lat,coslat,sinlat,dt,dh,h;
	double cosslp,sinslp,cosasp,sinasp;
	double bsg1,bsg2,bsg3;
	double decl,cosdecl,sindecl,cosegeom,sinegeom,coshss,hss;
	double sc,dir_beam_topa;
	double sum_flat_potrad, sum_slope_potrad, sum_trans;
	double cosh,sinh;
	double cza,cbsa,coszeh,coszwh;
	double dir_flat_topa,am;
	double trans1,trans2;
	double t_tmax,b,t_fmax;
	double t_final,pdif,pdir,srad1,srad2; 
	double sky_prop;
	double avg_horizon, slope_excess;
	double horizon_scalar, slope_scalar;
	
	/* optical airmass by degrees */
	double optam[21] = {2.90,3.05,3.21,3.39,3.69,3.82,4.07,4.37,4.72,5.12,5.60,
	6.18,6.88,7.77,8.90,10.39,12.44,15.36,19.79,26.96,30.00};
	
	/* number of simulation days */
	ndays = ctrl->ndays;

	/* calculate humidity from Tdew observations */
	for (i=0 ; i<ndays ; i++)
	{
		/* convert dewpoint to vapor pressure */
		pva = 610.7 * exp(17.38 * data->tdew[i] / (239.0 + data->tdew[i]));
		if (ctrl->outhum)
		{
			/* output humidity as vapor pressure */
			data->s_hum[i] = pva;
		}
		else
		{
			/* output humidity as vapor pressure deficit */
			/* calculate saturation vapor pressure at tday */
			pvs = 610.7 * exp(17.38 * data->s_tday[i] / (239.0 + data->s_tday[i]));
			/* calculate vpd */
			vpd = pvs-pva;
			if (vpd < 0.0) vpd = 0.0;
			data->s_hum[i] = vpd;
		}
	}
	
	/* estimate radiation using Tdew observations */
	/* allocate space for DTR and smoothed DTR arrays */
	if (!(dtr = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for DTR array\n");
		ok=0;
	}
	if (!(sm_dtr = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for smoothed DTR array\n");
		ok=0;
	}

	/* calculate diurnal temperature range for transmittance calculations */
	for (i=0 ; i<ndays ; i++)
	{
		tmax = data->tmax[i];
		tmin = data->tmin[i];
		if (tmax < tmin) tmax = tmin;
		dtr[i] = tmax-tmin;
	}
	
	/* smooth dtr array using a 30-day antecedent smoothing window */
	if (ndays >= 30)
	{
		if (pulled_boxcar(dtr, sm_dtr, ndays, 30, 0))
		{
			printf("Error in boxcar smoothing, calc_srad_humidity()\n");
			ok=0;
		}
	}
	else /* smoothing window width = ndays */
	{
		if (pulled_boxcar(dtr, sm_dtr, ndays, ndays, 0))
		{
			printf("Error in boxcar smoothing, calc_srad_humidity()\n");
			ok=0;
		}
	}

	/*****************************************
	 *                                       *
	 * start of the main radiation algorithm *
	 *                                       *
	 *****************************************/
	 
	/* STEP (1) calculate pressure ratio (site/reference) = f(elevation) */
	t1 = 1.0 - (LR_STD * p->site_elev)/T_STD;
	t2 = G_STD / (LR_STD * (R/MA));
	pratio = pow(t1,t2);
	
	/* STEP (2) correct initial transmittance for elevation */ 
	trans1 = pow(TBASE,pratio);
	
	/* STEP (3) build 366-day array of ttmax0, potential rad, and daylength */

	/* precalculate the transcendentals */
	lat = p->site_lat;
	/* check for (+/-) 90 degrees latitude, throws off daylength calc */
	lat *= RADPERDEG;
	if (lat > 1.5707) lat = 1.5707;
	if (lat < -1.5707) lat = -1.5707;
	coslat = cos(lat);
	sinlat = sin(lat);
	cosslp = cos(p->site_slp * RADPERDEG);
	sinslp = sin(p->site_slp * RADPERDEG);
	cosasp = cos(p->site_asp * RADPERDEG);
	sinasp = sin(p->site_asp * RADPERDEG);
	/* cosine of zenith angle for east and west horizons */
	coszeh = cos(1.570796 - (p->site_ehoriz * RADPERDEG));
	coszwh = cos(1.570796 - (p->site_whoriz * RADPERDEG));
	
	/* sub-daily time and angular increment information */
	dt = SRADDT;                /* set timestep */ 
	dh = dt / SECPERRAD;        /* calculate hour-angle step */
	
	/* begin loop through yeardays */
	for (i=0 ; i<365 ; i++)
	{
		/* calculate cos and sin of declination */
		decl = MINDECL * cos(((double)i + DAYSOFF) * RADPERDAY);
		cosdecl = cos(decl);
		sindecl = sin(decl);
		
		/* do some precalculations for beam-slope geometry (bsg) */
		bsg1 = -sinslp * sinasp * cosdecl;
		bsg2 = (-cosasp * sinslp * sinlat + cosslp * coslat) * cosdecl;
		bsg3 = (cosasp * sinslp * coslat + cosslp * sinlat) * sindecl;
		
		/* calculate daylength as a function of lat and decl */
		cosegeom = coslat * cosdecl;
		sinegeom = sinlat * sindecl;
		coshss = -(sinegeom) / cosegeom;
		if (coshss < -1.0) coshss = -1.0;  /* 24-hr daylight */
		if (coshss > 1.0) coshss = 1.0;    /* 0-hr daylight */
		hss = acos(coshss);                /* hour angle at sunset (radians) */
		/* daylength (seconds) */
		daylength[i] = 2.0 * hss * SECPERRAD;

		/* solar constant as a function of yearday (W/m^2) */
		sc = 1368.0 + 45.5*sin((2.0*PI*(double)i/365.25) + 1.7);
		/* extraterrestrial radiation perpendicular to beam, total over
		the timestep (J) */
		dir_beam_topa = sc * dt;
		
		sum_trans = 0.0;
		sum_flat_potrad = 0.0;
		sum_slope_potrad = 0.0;

		/* begin sub-daily hour-angle loop, from -hss to hss */
		for (h=-hss ; h<hss ; h+=dh)
		{
			/* precalculate cos and sin of hour angle */
			cosh = cos(h);
			sinh = sin(h);
			
			/* calculate cosine of solar zenith angle */
			cza = cosegeom * cosh + sinegeom;
			
			/* calculate cosine of beam-slope angle */
			cbsa = sinh * bsg1 + cosh * bsg2 + bsg3;
			
			/* check if sun is above a flat horizon */
			if (cza > 0.0) 
			{
				/* when sun is above the ideal (flat) horizon, do all the
				flat-surface calculations to determine daily total
				transmittance, and save flat-surface potential radiation
				for later calculations of diffuse radiation */
				
				/* potential radiation for this time period, flat surface,
				top of atmosphere */
				dir_flat_topa = dir_beam_topa * cza;
				
				/* determine optical air mass */
				am = 1.0/(cza + 0.0000001);
				if (am > 2.9)
				{
					ami = (int)(acos(cza)/RADPERDEG) - 69;
					if (ami < 0) ami = 0;
					if (ami > 20) ami = 20;
					am = optam[ami];
				}
				
				/* correct instantaneous transmittance for this optical
				air mass */
				trans2 = pow(trans1,am);
				
				/* instantaneous transmittance is weighted by potential
				radiation for flat surface at top of atmosphere to get
				daily total transmittance */
				sum_trans += trans2 * dir_flat_topa;
				
				/* keep track of total potential radiation on a flat
				surface for ideal horizons */
				sum_flat_potrad += dir_flat_topa;
				
				/* keep track of whether this time step contributes to
				component 1 (direct on slope) */
				if ((h<0.0 && cza>coszeh && cbsa>0.0) ||
					(h>=0.0 && cza>coszwh && cbsa>0.0))
				{
					/* sun between east and west horizons, and direct on
					slope. this period contributes to component 1 */
					sum_slope_potrad += dir_beam_topa * cbsa;
				}

			} /* end if sun above ideal horizon */
			
		} /* end of sub-daily hour-angle loop */
		
		/* calculate maximum daily total transmittance and daylight average
		flux density for a flat surface and the slope */
		if (daylength[i])
		{
			ttmax0[i] = sum_trans / sum_flat_potrad;
			flat_potrad[i] = sum_flat_potrad / daylength[i];
			slope_potrad[i] = sum_slope_potrad / daylength[i];
		}
		else
		{
			ttmax0[i] = 0.0;
			flat_potrad[i] = 0.0;
			slope_potrad[i] = 0.0;
		}
	} /* end of i=365 days loop */
	
	/* force yearday 366 = yearday 365 */
	ttmax0[365] = ttmax0[364];
	flat_potrad[365] = flat_potrad[364];
	slope_potrad[365] = slope_potrad[364];
	daylength[365] = daylength[364];

	/* STEP (4)  calculate the sky proportion for diffuse radiation */
	/* uses the product of spherical cap defined by average horizon angle
	and the great-circle truncation of a hemisphere. this factor does not
	vary by yearday. */
	avg_horizon = (p->site_ehoriz + p->site_whoriz)/2.0;
	horizon_scalar = 1.0 - sin(avg_horizon * RADPERDEG);
	if (p->site_slp > avg_horizon) slope_excess = p->site_slp - avg_horizon;
	else slope_excess = 0.0;
	if (2.0*avg_horizon > 180.0) slope_scalar = 0.0;
	else
	{
		slope_scalar = 1.0 - (slope_excess/(180.0 - 2.0*avg_horizon));
		if (slope_scalar < 0.0) slope_scalar = 0.0;
	}
	sky_prop = horizon_scalar * slope_scalar;
	
	/* STEP (5)  final calculation of daily total radiation */
	for (i=0 ; i<ndays ; i++)
	{
		/* correct this day's maximum transmittance for vapor pressure */
		yday = data->yday[i]-1;
		pva = 610.7 * exp(17.38 * data->tdew[i] / (239.0 + data->tdew[i]));
		t_tmax = ttmax0[yday] + ABASE * pva;
		
		/* b parameter from 30-day average of DTR */
		b = B0 + B1 * exp(-B2 * sm_dtr[i]);
		
		/* proportion of daily maximum transmittance */
		t_fmax = 1.0 - 0.9 * exp(-b * pow(dtr[i],C));
		
		/* correct for precipitation if this is a rain day */
		if (data->prcp[i]) t_fmax *= RAIN_SCALAR;
		
		/* final daily total transmittance */
		t_final = t_tmax * t_fmax;
		
		/* estimate fraction of radiation that is diffuse, on an
		instantaneous basis, from relationship with daily total
		transmittance in Jones (Plants and Microclimate, 1992)
		Fig 2.8, p. 25, and Gates (Biophysical Ecology, 1980)
		Fig 6.14, p. 122. */
		pdif = -1.25*t_final + 1.25;
		if (pdif > 1.0) pdif = 1.0;
		if (pdif < 0.0) pdif = 0.0;
		
		/* estimate fraction of radiation that is direct, on an
		instantaneous basis */
		pdir = 1.0 - pdif;
		
		/* the daily total radiation is estimated as the sum of the
		following two components:
		1. The direct radiation arriving during the part of
		   the day when there is direct beam on the slope.
		2. The diffuse radiation arriving over the entire daylength
		   (when sun is above ideal horizon).
		*/
		
		/* component 1 (direct) */
		srad1 = slope_potrad[yday] * t_final * pdir;
		
		/* component 2 (diffuse) */
		/* includes the effect of surface albedo in raising the diffuse
		radiation for obstructed horizons */
		srad2 = flat_potrad[yday] * t_final * pdif * 
			(sky_prop + DIF_ALB*(1.0-sky_prop)); 
		
		/* snow pack influence on radiation */	
		if (data->s_swe[i] > 0.0)
		{
			/* snow correction in J/m2/day */
			sc = (1.32 + 0.096 * data->s_swe[i]) * 1e6;
			/* convert to W/m2 and check for zero daylength */
			if (daylength[yday] > 0.0) sc /= daylength[yday];
			else sc = 0.0;
			/* set a maximum correction of 100 W/m2 */
			if (sc > 100.0) sc = 100.0;
		}
		else sc = 0.0;
		
		/* save daily radiation and daylength */
		data->s_srad[i] = srad1 + srad2 + sc;
		data->s_dayl[i] = daylength[yday];
	}

	/* free local array memory */
	free(dtr);
	free(sm_dtr);
	
	return (!ok);
} /* end of calc_srad_humidity() */
	
/* without Tdew input data, an iterative estimation of shortwave radiation
and humidity is required */
int calc_srad_humidity_iterative(const control_struct *ctrl,
	const parameter_struct *p, data_struct *data)
{
	int ok=1;
	int i,j,ndays;
	int start_yday,end_yday,isloop;
	int ami,yday;
	double ttmax0[366];
	double flat_potrad[366];
	double slope_potrad[366];
	double daylength[366];
	double *dtr, *sm_dtr;
	double *parray, *window, *t_fmax, *tdew;
	double *save_pet;
	double sum_prcp,ann_prcp,effann_prcp;
	double sum_pet,ann_pet;
	double tmax,tmin;
	double t1,t2;
	double pratio;
	double lat,coslat,sinlat,dt,h,dh;
	double cosslp,sinslp,cosasp,sinasp;
	double bsg1,bsg2,bsg3;
	double decl,cosdecl,sindecl,cosegeom,sinegeom,coshss,hss;
	double sc,dir_beam_topa;
	double sum_flat_potrad,sum_slope_potrad,sum_trans;
	double cosh,sinh;
	double cza,cbsa,coszeh,coszwh;
	double dir_flat_topa,am;
	double pva,t_tmax,b;
	double tmink,pet,ratio,ratio2,ratio3,tdewk;
	double pvs,vpd;
	double trans1,trans2;
	double t_final,pdif,pdir,srad1,srad2; 
	double pa;
	double sky_prop;
	double avg_horizon, slope_excess;
	double horizon_scalar, slope_scalar;

	/* optical airmass by degrees */
	double optam[21] = {2.90,3.05,3.21,3.39,3.69,3.82,4.07,4.37,4.72,5.12,5.60,
	6.18,6.88,7.77,8.90,10.39,12.44,15.36,19.79,26.96,30.00};

	/* number of simulation days */
	ndays = ctrl->ndays;
	
	/* local array memory allocation */
	/* allocate space for DTR and smoothed DTR arrays */
	if (!(dtr = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for DTR array\n");
		ok=0;
	}
	if (!(sm_dtr = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for smoothed DTR array\n");
		ok=0;
	}
	/* allocate space for effective annual precip array */
	if (!(parray = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for effective annual precip array\n");
		ok=0;
	}
	/* allocate space for the prcp totaling array */
	if (!(window = (double*) malloc((ndays+90)*sizeof(double))))
	{
		printf("Error allocating for prcp totaling array\n");
		ok = 0;
	}
	/* allocate space for t_fmax */
	if (!(t_fmax = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for p_tt_max array\n");
		ok=0;
	}
	/* allocate space for Tdew array */
	if (!(tdew = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for Tdew array\n");
		ok=0;
	}
	/* allocate space for save_pet array */
	if (!(save_pet = (double*) malloc(ndays * sizeof(double))))
	{
		printf("Error allocating for save_pet array\n");
		ok=0;
	}
	
	/* calculate diurnal temperature range for transmittance calculations */
	for (i=0 ; i<ndays ; i++)
	{
		tmax = data->tmax[i];
		tmin = data->tmin[i];
		if (tmax < tmin) tmax = tmin;
		dtr[i] = tmax-tmin;
	}
	
	/* smooth dtr array: After Bristow and Campbell, 1984 */
	if (ndays >= 30) /* use 30-day antecedent smoothing window */
	{
		if (pulled_boxcar(dtr, sm_dtr, ndays, 30, 0))
		{
			printf("Error in boxcar smoothing, calc_srad_humidity()\n");
			ok=0;
		}
	}
	else /* smoothing window width = ndays */
	{
		if (pulled_boxcar(dtr, sm_dtr, ndays, ndays, 0))
		{
			printf("Error in boxcar smoothing, calc_srad_humidity()\n");
			ok=0;
		}
	}
	
	/* calculate the annual total precip for decision between
	simple and arid-corrected humidity algorithm */
	sum_prcp = 0.0;
	for (i=0 ; i<ndays ; i++)
	{
		sum_prcp += data->s_prcp[i];
	}
	ann_prcp = (sum_prcp/(double)ndays) * 365.25;
	if (ann_prcp == 0.0) ann_prcp = 1.0;
	
	/* Generate the effective annual precip, based on a 3-month
	moving-window. Requires some special case handling for the
	beginning of the record and for short records. */
	/* check if there are at least 90 days in this input file, if not,
	use a simple total scaled to effective annual precip */
	if (ndays < 90)
	{
		sum_prcp = 0.0;
		for (i=0 ; i<ndays ; i++)
		{
			sum_prcp += data->s_prcp[i];
		}
		effann_prcp = (sum_prcp/(double)ndays) * 365.25;
		/* if the effective annual precip for this period
		is less than 8 cm, set the effective annual precip to 8 cm
		to reflect an arid condition, while avoiding possible
		division-by-zero errors and very large ratios (PET/Pann) */
		if (effann_prcp < 8.0) effann_prcp = 8.0;
		for (i=0 ; i<ndays ; i++)
		{
			parray[i] = effann_prcp;
		}
	}
	else
	{
		/* Check if the yeardays at beginning and the end of this input file
		match up. If so, use parts of the three months at the end
		of the input file to generate effective annual precip for
		the first 3-months. Otherwise, duplicate the first 90 days
		of the record. */
		start_yday = data->yday[0];
		end_yday = data->yday[ndays-1];
		if (start_yday != 1)
		{
			isloop = (end_yday == start_yday-1) ? 1 : 0;
		}
		else
		{
			isloop = (end_yday == 365 || end_yday == 366) ? 1 : 0;
		}

		/* fill the first 90 days of window */
		for (i=0 ; i<90 ; i++)
		{
			if (isloop) window[i] = data->s_prcp[ndays-90+i];
			else window[i] = data->s_prcp[i];
		}
		/* fill the rest of the window array */
		for (i=0 ; i<ndays ; i++)
		{
			window[i+90] = data->s_prcp[i];
		}

		/* for each day, calculate the effective annual precip from 
		scaled 90-day total */
		for (i=0 ; i<ndays ; i++)
		{
			sum_prcp = 0.0;
			for (j=0 ; j<90 ; j++)
			{
				sum_prcp += window[i+j];
			}
			sum_prcp = (sum_prcp/90.0) * 365.25;
			/* if the effective annual precip for this 90-day period
			is less than 8 cm, set the effective annual precip to 8 cm
			to reflect an arid condition, while avoiding possible
			division-by-zero errors and very large ratios (PET/Pann) */
			parray[i] = (sum_prcp < 8.0) ? 8.0 : sum_prcp;
		}
	} /* end if ndays >= 90 */	
	
	/*****************************************
	 *                                       *
	 * start of the main radiation algorithm *
	 *                                       *
	 *****************************************/
	 
	/* before starting the iterative algorithm between humidity and 
	radiation, calculate all the variables that don't depend on 
	humidity so they only get done once. */
	
	/* STEP (1) calculate pressure ratio (site/reference) = f(elevation) */
	t1 = 1.0 - (LR_STD * p->site_elev)/T_STD;
	t2 = G_STD / (LR_STD * (R/MA));
	pratio = pow(t1,t2);
	
	/* STEP (2) correct initial transmittance for elevation */ 
	trans1 = pow(TBASE,pratio);
	
	/* STEP (3) build 366-day array of ttmax0, potential rad, and daylength */

	/* precalculate the transcendentals */
	lat = p->site_lat;
	/* check for (+/-) 90 degrees latitude, throws off daylength calc */
	lat *= RADPERDEG;
	if (lat > 1.5707) lat = 1.5707;
	if (lat < -1.5707) lat = -1.5707;
	coslat = cos(lat);
	sinlat = sin(lat);
	cosslp = cos(p->site_slp * RADPERDEG);
	sinslp = sin(p->site_slp * RADPERDEG);
	cosasp = cos(p->site_asp * RADPERDEG);
	sinasp = sin(p->site_asp * RADPERDEG);
	/* cosine of zenith angle for east and west horizons */
	coszeh = cos(1.570796 - (p->site_ehoriz * RADPERDEG));
	coszwh = cos(1.570796 - (p->site_whoriz * RADPERDEG));
	
	/* sub-daily time and angular increment information */
	dt = SRADDT;                /* set timestep */ 
	dh = dt / SECPERRAD;        /* calculate hour-angle step */
	
	/* begin loop through yeardays */
	for (i=0 ; i<365 ; i++)
	{
		/* calculate cos and sin of declination */
		decl = MINDECL * cos(((double)i + DAYSOFF) * RADPERDAY);
		cosdecl = cos(decl);
		sindecl = sin(decl);
		
		/* do some precalculations for beam-slope geometry (bsg) */
		bsg1 = -sinslp * sinasp * cosdecl;
		bsg2 = (-cosasp * sinslp * sinlat + cosslp * coslat) * cosdecl;
		bsg3 = (cosasp * sinslp * coslat + cosslp * sinlat) * sindecl;
		
		/* calculate daylength as a function of lat and decl */
		cosegeom = coslat * cosdecl;
		sinegeom = sinlat * sindecl;
		coshss = -(sinegeom) / cosegeom;
		if (coshss < -1.0) coshss = -1.0;  /* 24-hr daylight */
		if (coshss > 1.0) coshss = 1.0;    /* 0-hr daylight */
		hss = acos(coshss);                /* hour angle at sunset (radians) */
		/* daylength (seconds) */
		daylength[i] = 2.0 * hss * SECPERRAD;

		/* solar constant as a function of yearday (W/m^2) */
		sc = 1368.0 + 45.5*sin((2.0*PI*(double)i/365.25) + 1.7);
		/* extraterrestrial radiation perpendicular to beam, total over
		the timestep (J) */
		dir_beam_topa = sc * dt;
		
		sum_trans = 0.0;
		sum_flat_potrad = 0.0;
		sum_slope_potrad = 0.0;

		/* begin sub-daily hour-angle loop, from -hss to hss */
		for (h=-hss ; h<hss ; h+=dh)
		{
			/* precalculate cos and sin of hour angle */
			cosh = cos(h);
			sinh = sin(h);
			
			/* calculate cosine of solar zenith angle */
			cza = cosegeom * cosh + sinegeom;
			
			/* calculate cosine of beam-slope angle */
			cbsa = sinh * bsg1 + cosh * bsg2 + bsg3;
			
			/* check if sun is above a flat horizon */
			if (cza > 0.0) 
			{
				/* when sun is above the ideal (flat) horizon, do all the
				flat-surface calculations to determine daily total
				transmittance, and save flat-surface potential radiation
				for later calculations of diffuse radiation */
				
				/* potential radiation for this time period, flat surface,
				top of atmosphere */
				dir_flat_topa = dir_beam_topa * cza;
				
				/* determine optical air mass */
				am = 1.0/(cza + 0.0000001);
				if (am > 2.9)
				{
					ami = (int)(acos(cza)/RADPERDEG) - 69;
					if (ami < 0) ami = 0;
					if (ami > 20) ami = 20;
					am = optam[ami];
				}
				
				/* correct instantaneous transmittance for this optical
				air mass */
				trans2 = pow(trans1,am);
				
				/* instantaneous transmittance is weighted by potential
				radiation for flat surface at top of atmosphere to get
				daily total transmittance */
				sum_trans += trans2 * dir_flat_topa;
				
				/* keep track of total potential radiation on a flat
				surface for ideal horizons */
				sum_flat_potrad += dir_flat_topa;
				
				/* keep track of whether this time step contributes to
				component 1 (direct on slope) */
				if ((h<0.0 && cza>coszeh && cbsa>0.0) ||
					(h>=0.0 && cza>coszwh && cbsa>0.0))
				{
					/* sun between east and west horizons, and direct on
					slope. this period contributes to component 1 */
					sum_slope_potrad += dir_beam_topa * cbsa;
				}

			} /* end if sun above ideal horizon */
			
		} /* end of sub-daily hour-angle loop */
		
		/* calculate maximum daily total transmittance and daylight average
		flux density for a flat surface and the slope */
		if (daylength[i])
		{
			ttmax0[i] = sum_trans / sum_flat_potrad;
			flat_potrad[i] = sum_flat_potrad / daylength[i];
			slope_potrad[i] = sum_slope_potrad / daylength[i];
		}
		else
		{
			ttmax0[i] = 0.0;
			flat_potrad[i] = 0.0;
			slope_potrad[i] = 0.0;
		}
	} /* end of i=365 days loop */
	
	/* force yearday 366 = yearday 365 */
	ttmax0[365] = ttmax0[364];
	flat_potrad[365] = flat_potrad[364];
	slope_potrad[365] = slope_potrad[364];
	daylength[365] = daylength[364];

	/* STEP (4)  calculate the sky proportion for diffuse radiation */
	/* uses the product of spherical cap defined by average horizon angle
	and the great-circle truncation of a hemisphere. this factor does not
	vary by yearday. */
	avg_horizon = (p->site_ehoriz + p->site_whoriz)/2.0;
	horizon_scalar = 1.0 - sin(avg_horizon * RADPERDEG);
	if (p->site_slp > avg_horizon) slope_excess = p->site_slp - avg_horizon;
	else slope_excess = 0.0;
	if (2.0*avg_horizon > 180.0) slope_scalar = 0.0;
	else
	{
		slope_scalar = 1.0 - (slope_excess/(180.0 - 2.0*avg_horizon));
		if (slope_scalar < 0.0) slope_scalar = 0.0;
	}
	sky_prop = horizon_scalar * slope_scalar;
	
	/* b parameter, and t_fmax not varying with Tdew, so these can be
	calculated once, outside the iteration between radiation and humidity
	estimates. Requires storing t_fmax in an array. */
	for (i=0 ; i<ndays ; i++)
	{	
		/* b parameter from 30-day average of DTR */
		b = B0 + B1 * exp(-B2 * sm_dtr[i]);
		
		/* proportion of daily maximum transmittance */
		t_fmax[i] = 1.0 - 0.9 * exp(-b * pow(dtr[i],C));

		/* correct for precipitation if this is a rain day */
		if (data->prcp[i]) t_fmax[i] *= RAIN_SCALAR;
		
	}
	
	/* As a first approximation, calculate radiation assuming
	that Tdew = Tmin */
	for (i=0 ; i<ndays ; i++)
	{
		yday = data->yday[i]-1;
		tdew[i] = data->s_tmin[i];
		pva = 610.7 * exp(17.38 * tdew[i] / (239.0 + tdew[i]));
		t_tmax = ttmax0[yday] + ABASE * pva;
		
		/* final daily total transmittance */
		t_final = t_tmax * t_fmax[i];
		
		/* estimate fraction of radiation that is diffuse, on an
		instantaneous basis, from relationship with daily total
		transmittance in Jones (Plants and Microclimate, 1992)
		Fig 2.8, p. 25, and Gates (Biophysical Ecology, 1980)
		Fig 6.14, p. 122. */
		pdif = -1.25*t_final + 1.25;
		if (pdif > 1.0) pdif = 1.0;
		if (pdif < 0.0) pdif = 0.0;
		
		/* estimate fraction of radiation that is direct, on an
		instantaneous basis */
		pdir = 1.0 - pdif;
		
		/* the daily total radiation is estimated as the sum of the
		following two components:
		1. The direct radiation arriving during the part of
		   the day when there is direct beam on the slope.
		2. The diffuse radiation arriving over the entire daylength
		   (when sun is above ideal horizon).
		*/
		
		/* component 1 */
		srad1 = slope_potrad[yday] * t_final * pdir;
		
		/* component 2 (diffuse) */
		/* includes the effect of surface albedo in raising the diffuse
		radiation for obstructed horizons */
		srad2 = flat_potrad[yday] * t_final * pdif * 
			(sky_prop + DIF_ALB*(1.0-sky_prop)); 
		
		/* snow pack influence on radiation */	
		if (data->s_swe[i] > 0.0)
		{
			/* snow correction in J/m2/day */
			sc = (1.32 + 0.096 * data->s_swe[i]) * 1e6;
			/* convert to W/m2 and check for zero daylength */
			if (daylength[yday] > 0.0) sc /= daylength[yday];
			else sc = 0.0;
			/* set a maximum correction of 100 W/m2 */
			if (sc > 100.0) sc = 100.0;
		}
		else sc = 0.0;
		
		/* save daily radiation and daylength */
		data->s_srad[i] = srad1 + srad2 + sc;
		data->s_dayl[i] = daylength[yday];
	}
	
	/* estimate annual PET first, to decide which humidity algorithm 
	should be used */
	/* estimate air pressure at site */
	pa = atm_pres(p->site_elev);
	sum_pet = 0.0;
	for (i=0 ; i<ndays ; i++)
	{
		save_pet[i] = calc_pet(data->s_srad[i],data->s_tday[i],pa,data->s_dayl[i]);
		sum_pet += save_pet[i];
	}
	ann_pet = (sum_pet/(double)ndays) * 365.25;
	
	/* humidity algorithm decision: 
	PET/prcp >= 2.5 -> arid correction
	PET/prcp <  2.5 -> use tdew-tmin, which is already finished */
	printf("PET/PRCP = %.4lf\n",ann_pet/ann_prcp);
	
	if (ann_pet/ann_prcp >= 2.5) 
	{
		printf("Using arid-climate humidity algorithm\n");
		/* Estimate Tdew using the initial estimate of radiation for PET */
		for (i=0 ; i<ndays ; i++)
		{
			tmink = data->s_tmin[i] + 273.15;
			pet = save_pet[i];

			/* calculate ratio (PET/effann_prcp) and correct the dewpoint */
			ratio = pet/parray[i];
			ratio2 = ratio*ratio;
			ratio3 = ratio2*ratio;
			tdewk = tmink*(-0.127 + 1.121*(1.003 - 1.444*ratio + 12.312*ratio2 
				- 32.766*ratio3) + 0.0006*(dtr[i]));
			tdew[i] = tdewk - 273.15;
		}

		/* Revise estimate of radiation using new Tdew */
		for (i=0 ; i<ndays ; i++)
		{
			yday = data->yday[i]-1;
			pva = 610.7 * exp(17.38 * tdew[i] / (239.0 + tdew[i]));
			t_tmax = ttmax0[yday] + ABASE * pva;

			/* final daily total transmittance */
			t_final = t_tmax * t_fmax[i];

			/* estimate fraction of radiation that is diffuse, on an
			instantaneous basis, from relationship with daily total
			transmittance in Jones (Plants and Microclimate, 1992)
			Fig 2.8, p. 25, and Gates (Biophysical Ecology, 1980)
			Fig 6.14, p. 122. */
			pdif = -1.25*t_final + 1.25;
			if (pdif > 1.0) pdif = 1.0;
			if (pdif < 0.0) pdif = 0.0;

			/* estimate fraction of radiation that is direct, on an
			instantaneous basis */
			pdir = 1.0 - pdif;

			/* the daily total radiation is estimated as the sum of the
			following two components:
			1. The direct radiation arriving during the part of
			   the day when there is direct beam on the slope.
			2. The diffuse radiation arriving over the entire daylength
			   (when sun is above ideal horizon).
			*/

			/* component 1 */
			srad1 = slope_potrad[yday] * t_final * pdir;

			/* component 2 (diffuse) */
			/* includes the effect of surface albedo in raising the diffuse
			radiation for obstructed horizons */
			srad2 = flat_potrad[yday] * t_final * pdif * 
				(sky_prop + DIF_ALB*(1.0-sky_prop)); 

			/* snow pack influence on radiation */	
			if (data->s_swe[i] > 0.0)
			{
				/* snow correction in J/m2/day */
				sc = (1.32 + 0.096 * data->s_swe[i]) * 1e6;
				/* convert to W/m2 and check for zero daylength */
				if (daylength[yday] > 0.0) sc /= daylength[yday];
				else sc = 0.0;
				/* set a maximum correction of 100 W/m2 */
				if (sc > 100.0) sc = 100.0;
			}
			else sc = 0.0;

			/* save daily radiation */
			data->s_srad[i] = srad1 + srad2 + sc;
		}

		/* Revise estimate of Tdew using new radiation */
		for (i=0 ; i<ndays ; i++)
		{
			tmink = data->s_tmin[i] + 273.15;
			pet = calc_pet(data->s_srad[i],data->s_tday[i],pa,data->s_dayl[i]);

			/* calculate ratio (PET/effann_prcp) and correct the dewpoint */
			ratio = pet/parray[i];
			ratio2 = ratio*ratio;
			ratio3 = ratio2*ratio;
			tdewk = tmink*(-0.127 + 1.121*(1.003 - 1.444*ratio + 12.312*ratio2 
				- 32.766*ratio3) + 0.0006*(dtr[i]));
			tdew[i] = tdewk - 273.15;
		}
	} /* end of arid-correction humidity and radiation estimation */
	else
	{
		printf("Using Tdew=Tmin humidity algorithm\n");
	}
	
	/* now calculate vapor pressure from tdew */
	for (i=0 ; i<ndays ; i++)
	{
		pva = 610.7 * exp(17.38 * tdew[i] / (239.0 + tdew[i]));
		if (ctrl->outhum)
		{
			/* output humidity as vapor pressure (Pa) */
			data->s_hum[i] = pva;
		}
		else
		{
			/* output humidity as vapor pressure deficit (Pa) */
			/* calculate saturated VP at tday */
			pvs = 610.7 * exp(17.38 * data->s_tday[i]/(239.0+data->s_tday[i]));
			vpd = pvs - pva;
			if (vpd < 0.0) vpd = 0.0;
			data->s_hum[i] = vpd;
		}
	} /* end for i = ndays loop */
	
	/* free local array memory */
	free(dtr);
	free(sm_dtr);
	free(parray);
	free(window);
	free(t_fmax);
	free(tdew);
	free(save_pet);
	
	return (!ok);
} /* end of calc_srad_humidity_iterative() */
	
/* write_out writes the output file, governed by control flags */	
int write_out(const control_struct *ctrl, const data_struct *data)
{
	int ok=1;
	int i,ndays;
	char humstr[16];
	int iy;
	
	iy = ctrl->inyear;
	if (ctrl->outhum) strcpy(humstr,"VP");
	else strcpy(humstr, "VPD");
	
	ndays = ctrl->ndays;
	
	
	/* write the column header lines */
	if (iy) fprintf(ctrl->outmet.ptr,"%6s","year");
	fprintf(ctrl->outmet.ptr,"%6s%8s%8s%8s%8s%9s%9s%8s\n",
		"yday","Tmax","Tmin","Tday","prcp",humstr,"srad","daylen");
	if (iy) fprintf(ctrl->outmet.ptr,"%6s","      ");
	fprintf(ctrl->outmet.ptr,"%6s%8s%8s%8s%8s%9s%9s%8s\n",
		"    ","(deg C)","(deg C)","(deg C)","(cm)","(Pa)","(W m-2)","(s)");
	
	/* write each day's output */
	for (i=0 ; i<ndays ; i++)
	{
		if (iy) fprintf(ctrl->outmet.ptr,"%6d",data->year[i]);
		fprintf(ctrl->outmet.ptr,"%6d%8.2lf%8.2lf%8.2lf%8.2lf%9.2lf%9.2lf%8.0lf\n",
		data->yday[i],data->s_tmax[i],data->s_tmin[i],data->s_tday[i],
		data->s_prcp[i],data->s_hum[i],data->s_srad[i],data->s_dayl[i]);
	}
	
	fclose(ctrl->outmet.ptr);
	return (!ok);
}

/* data_free frees the memory previously allocated by data_alloc() */
int data_free(const control_struct *ctrl, data_struct *data)
{
	int ok=1;
	if (ctrl->inyear) free(data->year);
	free(data->yday);
	free(data->tmax);
	free(data->tmin);
	free(data->prcp);
	if (ctrl->indewpt) free(data->tdew);
	free(data->s_tmax);
	free(data->s_tmin);
	free(data->s_tday);
	free(data->s_prcp);
	free(data->s_hum);
	free(data->s_srad);
	free(data->s_dayl);
	
	return (!ok);
}
			
/* calc_pet() calculates the potential evapotranspiration for aridity 
corrections in calc_vpd(), according to Kimball et al., 1997 */
double calc_pet(double rad, double ta, double pa, double dayl)
{
	/* input parameters and units :
	double rad      (W/m2)  daylight average incident shortwave radiation
	double ta       (deg C) daylight average air temperature
	double pa       (Pa)    air pressure
	double dayl     (s)     daylength 
	*/
	
	double rnet;       /* (W m-2) absorbed shortwave radiation avail. for ET */
	double lhvap;      /* (J kg-1) latent heat of vaporization of water */ 
	double gamma;      /* (Pa K-1) psychrometer parameter */
	double dt = 0.2;   /* offset for saturation vapor pressure calculation */
	double t1, t2;     /* (deg C) air temperatures */
	double pvs1, pvs2; /* (Pa)   saturated vapor pressures */
	double pet;        /* (kg m-2 day-1) potential evapotranspiration */
	double s;          /* (Pa K-1) slope of saturated vapor pressure curve */

	/* calculate absorbed radiation, assuming albedo = 0.2  and ground
	heat flux = 10% of absorbed radiation during daylight */
	rnet = rad * 0.72;
		
    /* calculate latent heat of vaporization as a function of ta */
    lhvap = 2.5023e6 - 2430.54 * ta;
    
    /* calculate the psychrometer parameter: gamma = (cp pa)/(lhvap epsilon)
    where:
    cp       (J/kg K)   specific heat of air
    epsilon  (unitless) ratio of molecular weights of water and air
    */
    gamma = CP * pa / (lhvap * EPS);
    
    /* estimate the slope of the saturation vapor pressure curve at ta */
    /* temperature offsets for slope estimate */
    t1 = ta+dt;
    t2 = ta-dt;
    
    /* calculate saturation vapor pressures at t1 and t2, using formula from 
	Abbott, P.F., and R.C. Tabony, 1985. The estimation of humidity parameters.
	Meteorol. Mag., 114:49-56.
	*/
    pvs1 = 610.7 * exp(17.38 * t1 / (239.0 + t1));
    pvs2 = 610.7 * exp(17.38 * t2 / (239.0 + t2));

    /* calculate slope of pvs vs. T curve near ta */
    s = (pvs1-pvs2) / (t1-t2);
    
    /* calculate PET using Priestly-Taylor approximation, with coefficient
    set at 1.26. Units of result are kg/m^2/day, equivalent to mm water/day */
	pet = (1.26 * (s/(s+gamma)) * rnet * dayl)/lhvap;
	
	/* return a value in centimeters/day, because this value is used in a ratio
	to annual total precip, and precip units are centimeters */
	return (pet/10.0);
}    
		
/* atm_pres() calculates the atmospheric pressure as a function of elevation */
double atm_pres(double elev)
{
	/* daily atmospheric pressure (Pa) as a function of elevation (m) */
	/* From the discussion on atmospheric statics in:
	Iribane, J.V., and W.L. Godson, 1981. Atmospheric Thermodynamics, 2nd
		Edition. D. Reidel Publishing Company, Dordrecht, The Netherlands.
		(p. 168)
	*/
	
	int ok=1;
	double t1,t2;
	double pa;
	
	t1 = 1.0 - (LR_STD * elev)/T_STD;
	t2 = G_STD / (LR_STD * (R / MA));
	pa = P_STD * pow(t1,t2);
	
	return(pa);
}

/* pulled_boxcar() calculates a moving average of antecedent values in an
array, using either a ramped (w_flag=1) or a flat (w_flag=0) weighting */	
int pulled_boxcar(double *input,double *output,int n,int w,int w_flag)
{
	int ok=1;
    int i,j;
    double *wt;
    double total,sum_wt;

    if (w > n) {
        printf("Boxcar longer than array...\n");
        printf("Resize boxcar and try again\n");
        ok=0;
    }
    
    if (ok && !(wt = (double*) malloc(w * sizeof(double))))
    {
    	printf("Allocation error in boxcar()\n");
    	ok=0;
    }
    
    if (ok)
    {
	    /* when w_flag != 0, use linear ramp to weight tails,
	    otherwise use constant weight */
	    sum_wt = 0.0;
	    if (w_flag)
	    {
	        for (i=0 ; i<w ; i++)
	       	{
	            wt[i] = (double)(i+1);
	            sum_wt += wt[i];
	        }
	    }
	    else
	    {
	        for (i=0 ; i<w ; i++)
	        { 	
	            wt[i] = 1.0;
	            sum_wt += wt[i];
	        }
	    }
	    
	    /* fill the output array, starting with the point where a full
	    boxcar can be calculated */
	    for (i=w-1 ; i<n ; i++)
	    {
	        total = 0.0;
	        for (j=0 ; j<w ; j++)
	        {
	            total += input[i-w+j+1] * wt[j];
	        }
	        output[i] = total/sum_wt;
	    }
	    
	    /* fill the first w elements of the output array with the value from
	    the first full boxcar */
	    for (i=0 ; i<w-1 ; i++)
	    {
	    	output[i] = output[w-1];
	    }
	    
	    free(wt);
	    
	} /* end if ok */
	
    return (!ok);
}
/* end of pulled_boxcar() */  
