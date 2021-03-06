% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibration.R
\name{optiMuso}
\alias{optiMuso}
\title{optiMuso}
\usage{
optiMuso(
  measuredData,
  parameters = NULL,
  startDate = NULL,
  endDate = NULL,
  formatString = "\%Y-\%m-\%d",
  dataVar,
  outLoc = "./calib",
  preTag = "cal-",
  settings = setupMuso(),
  outVars = NULL,
  iterations = 30,
  skipSpinup = TRUE,
  plotName = "calib.jpg",
  modifyOriginal = TRUE,
  likelihood,
  uncertainity = NULL,
  naVal = NULL,
  postProcString = NULL,
  w = NULL,
  lg = FALSE,
  parallel = TRUE
)
}
\arguments{
\item{parameters}{b}

\item{startDate}{d}

\item{endDate}{e}

\item{formatString}{a}

\item{outLoc}{c}

\item{settings}{e}

\item{iterations}{c}

\item{skipSpinup}{a}

\item{plotName}{u}

\item{likelihood}{d}

\item{measuredDataFile}{a}

\item{sep}{c}

\item{filterCol}{a}

\item{filterVal}{b}

\item{selVar}{c}

\item{pretag}{a}

\item{calPar}{a}

\item{constrains}{d}

\item{leapYear}{b}
}
\description{
This function calculates the -users specified- likelihood for random model input.
}
\author{
Roland HOLLOS
}
