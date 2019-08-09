#' metReadTable
#'
#' This function reads meteorology file (mtc43) in table format.
#'
#' @author Erzsebet Kristof 
#' @param metFileName The name of the meteorology file (mtc43).
#' @param header If header is set to TRUE, the first row of the meteorolgy file is read as header.
#' @param skip Number of header lines in meteorology file. Default is 0.
#' @param sep Separator between columns. Default is 
#' @return ...
#' @export 

metReadTable <- function(metFileName, sep) {
  metTable <- readLines(metFileName)
  metTable <- strsplit(metTable, "[\ \t]")
  sapply(metTable , "[", 1) 
  
  }
  