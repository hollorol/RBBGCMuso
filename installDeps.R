(function(){
  packagesToInstall <- c("shiny","shinyjs","plotly","promises","future","data.table","rhandsontable")
  sapply(packagesToInstall, function(pkgs){
    if(!is.element(pkgs,installed.packages()[,1])){
      install.packages(pkgs)
    }
  })
})()
