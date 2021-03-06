% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/musoExample.R
\name{copyMusoExampleTo}
\alias{copyMusoExampleTo}
\title{copyMusoExampleTo}
\usage{
copyMusoExampleTo(example = NULL, destination = NULL)
}
\arguments{
\item{example}{This is the name of the example file. If it is not set then a simple graphical user interface (tcl/tk menu) will open to select the target dataset (which is typically an experimental site). In the list hhs means the Hegyhatsal eddy covariance site in Hungary.}

\item{destination}{The destination where the example files will be copied.}
}
\description{
This function enables the user to download a complete, working file set to quickly start using Biome-BGCMuSo through RBBGCMuso (or in standalone mode). The user has to specify the target directory for the files. The file set contains the model executable (muso.exe in Windows), the INI files that drive the model, and other files like meteorology input, ecophysiological constants file (EPC), and other ancillary files (CO2 concentration, parameter range definition file called parameters.csv). Note that we strongly recommend to read the User's Guide of Biome-BGCMuSo to clarify the meaning of the input files. The input files (s.ini, n.ini, maize.epc, meteorology files) are simple text files, so the user can read (and modify) them with his/her favourite text editor (like Editpad Lite, vim, emacs). Note that some files use UNIX/Linux style text which means that the text will not be readable using the Windows Notepad.
}
