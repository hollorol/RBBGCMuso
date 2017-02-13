#' This function calls the UNIX(-like) sed program to change specific line to other, using the row numbers.
#' @author Roland
#' @param The name of the file which is needed to be changed in some lines, the numbers of this lines(vector), and
#' the contents(vector).
#' @return void


changeSpecLine<-function(lineNumber,content,file){
    TOT=readLines(file,-1)
    TOT[lineNumber]<-content
    writeLines(TOT,file)
    }

changemulline <- function(filename,calibrationpar,contents){
  #This is the function which is capable change multiple specific lines to other using their row numbers.
  #The function uses the previous changspecline function to operate.
  varnum <- length(calibrationpar)
  if(length(contents)!=varnum)
  {
    cat("Error: number of the values is not the same as the number of the changed parameters")
  }

    TOT=readLines(filename,-1)
    TOT[calibrationpar]<-contents
    writeLines(TOT,filename)
}




