/* 
mtclim43_parameters.h
model parameters for MTCLIM 4.3

Some model parameters are set in the *.ini file. Others are set here.

Peter Thornton
NTSG, School of Forestry
University of Montana
1/20/2000

(dim) stands for dimensionless values
*/

/* parameters for the Tair algorithm */
#define TDAYCOEF     0.45  /* (dim) daylight air temperature coefficient (dim) */

/* parameters for the snowpack algorithm */
#define SNOW_TCRIT   -6.0  /* (deg C) critical temperature for snowmelt */   
#define SNOW_TRATE  0.042  /* (cm/degC/day) snowmelt rate */

/* parameters for the radiation algorithm */
#define TBASE       0.870  /* (dim) max inst. trans., 0m, nadir, dry atm */
#define ABASE     -6.1e-5  /* (1/Pa) vapor pressure effect on transmittance */
#define C             1.5  /* (dim) radiation parameter */
#define B0          0.013  /* (dim) radiation parameter */
#define B1          0.201  /* (dim) radiation parameter */
#define B2          0.185  /* (dim) radiation parameter */
#define RAIN_SCALAR  0.75  /* (dim) correction to trans. for rain day */
#define DIF_ALB       0.6  /* (dim) diffuse albedo for horizon correction */
#define SC_INT       1.32  /* (MJ/m2/day) snow correction intercept */
#define SC_SLOPE    0.096  /* (MJ/m2/day/cm) snow correction slope */

/* output file extension */
#define POSTFIX  ".mtc43"  /* extension added to output filename prefix */
