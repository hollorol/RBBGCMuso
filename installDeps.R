(function(){
  packagesToInstall <- c("shiny","shinyjs","plotly","promises","future","data.table","rhandsontable")
  installedp<- sapply(packagesToInstall, function(pkgs){
    if(!is.element(pkgs,installed.packages()[,1])){
      install.packages(pkgs)
      if(!is.element(pkgs,installed.packages([,1]))){
        return(FALSE)
      } else {
        return(TRUE)
      }
    } else {
      return(TRUE)
    }
  })
  if(any(!installedp)){
   stop("The installation process was not successful. Please try rerun the installation!") 
  }
})()
