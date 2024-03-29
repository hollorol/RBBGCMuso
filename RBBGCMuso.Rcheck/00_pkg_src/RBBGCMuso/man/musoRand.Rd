% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/musoRand.R
\name{musoRand}
\alias{musoRand}
\title{musoRand}
\usage{
musoRand(
  parameters,
  iterations = 3000,
  fileType = "epc",
  constrains = NULL,
  burnin = NULL
)
}
\arguments{
\item{parameters}{This is a dataframe (heterogeneous data-matrix), where the first column is the name of the parameter, the second is a numeric vector of the rownumbers of the given variable in the input EPC file, and the last two columns describe the minimum and the maximum of the parameter (i.e. the parameter ranges), defining the interval for the randomization.}

\item{constrains}{This is a matrix wich specify the constrain rules for the sampling. Parameter dependencies are described in the Biome-BGCMuSo User's Guide. Further informations is coming soon.}

\item{iteration}{The number of samples for the Monte-Carlo experiment. We propose to use at least 3000 iteration because it is generally fast and it can be subsampled later at any time.}
}
\description{
This funtion uses the Monte Carlo technique to uniformly sample the parameter space from user defined parameters of the Biome-BGCMuSo model. The sampling algorithm ensures that the parameters are constrained by the model logic which means that parameter dependencies are fully taken into account (parameter dependency means that e.g leaf C:N ratio must be smaller than C:N ratio of litter; more complicated rules apply to the allocation parameters where the allocation fractions to different plant compartments must sum up 1). This function implements a mathematically correct solution to provide uniform distriution for all selected parameters.
}
\author{
Roland HOLLOS
}
