/* 
mtclim43_constants.h
physical constants for MTCLIM 4.3

Peter Thornton
NTSG, School of Forestry
University of Montana
1/20/2000

(dim) stands for dimensionless values
*/

#define SECPERRAD 13750.9871     /* seconds per radian of hour angle */
#define RADPERDAY 0.017214       /* radians of Earth orbit per julian day */
#define RADPERDEG 0.01745329     /* radians per degree */
#define MINDECL -0.4092797       /* minimum declination (radians) */
#define DAYSOFF 11.25            /* julian day offset of winter solstice */
#define SRADDT 600.0             /* timestep for radiation routine (seconds) */

#define MA       28.9644e-3      /* (kg mol-1) molecular weight of air */
#define MW       18.0148e-3      /* (kg mol-1) molecular weight of water */
#define R        8.3143          /* (m3 Pa mol-1 K-1) gas law constant */
#define G_STD    9.80665         /* (m s-2) standard gravitational accel. */ 
#define P_STD    101325.0        /* (Pa) standard pressure at 0.0 m elevation */
#define T_STD    288.15          /* (K) standard temp at 0.0 m elevation  */ 
#define CP       1010.0          /* (J kg-1 K-1) specific heat of air */
#define LR_STD   0.0065          /* (-K m-1) standard temperature lapse rate */
#define EPS      0.62196351      /* (MW/MA) unitless ratio of molec weights */
#define PI       3.14159265      /* pi */
