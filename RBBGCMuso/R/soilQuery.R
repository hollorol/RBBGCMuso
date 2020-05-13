#' getSoilDataFull
#' 
#' This function collects soil data from a given restapi, de default is soilGrid
#'
#' @author Roland HOLLÓS
#' @name getSoilDataFull
#' @importFrom glue glue
#' @importFrom httr  config with_config GET content

getSoilDataFull <- function(lat, lon, apiURL, port) {
    if(missing(apiURL)){
        apiURL <- "https://81.169.232.36"
    }
    if(missing(port)){
        port <- 4445
    }
    apiString <- glue("{apiURL}:{port}/query?lon={lon}&lat={lat}")
    soilREST <- with_config(config(ssl_verifypeer=0L, ssl_verifyhost=0L),
                            GET(apiString)) # This is temporary solution ssl_verification wont bypass
   content(soilREST) 
}

#' createSoilFile
#' 
#' This function collects soil data from a given restapi, de default is soilGrid
#'
#' @author Roland HOLLOS
#' @name createSoilFile
#' @importFrom glue glue
#' @importFrom stats approx 
#' @importFrom magrittr '%>%'
#' @export

createSoilFile <- function(lat,lon,
                            outputFile="recent.soi",
                            method="constant",apiURL,
                            apiPort,template=system.file("examples/hhs/hhs.soi",package="RBBGCMuso")) {
    if(missing(apiURL)){
        apiURL <- "https://81.169.232.36"
    }
    if(missing(apiPort)){
        apiPort <- 4445
    }
    outFile <- suppressWarnings(readLines(template))
    outFile[1] <- sprintf("SOILPROP FILE - lat: %s, lon: %s, created in: %s",lat,lon,date())
    musoCenters <- c(1.5,6.5,20.0,45.0,75.0,105.0,135.0,175.0,300.0,700.0)
    soilGridDepths  <- c(0,5,15,30,60,100,200)
    rest<- getSoilDataFull(lat,lon, apiURL, apiPort)
    if(rest$properties$soilmask=="nodata"){
        stop("There is no data for the given coordinates");
    }

    createMusoLayers <- function(values,depths=soilGridDepths,centers=musoCenters,intMethod=method){
        approx(x=depths,y=values, xout = centers, method=intMethod,rule=2)$y %>%
            paste(.,collapse="\t") %>% paste0(.," ")
    }
    soilDepth <- unlist(rest$properties$BDRICM$M)/100
    outFile[42] <- sub("([0-9.]*\\s+){1}",paste0(soilDepth," "),outFile[42], outFile[42])
    outFile[48] <- sub("([0-9.]*\\s+){10}",createMusoLayers(unlist(rest$properties$SNDPPT$M)),outFile[48])
    outFile[49] <- sub("([0-9.]*\\s+){10}",createMusoLayers(unlist(rest$properties$SLTPPT$M)),outFile[49])
    outFile[50] <- sub("([0-9.]*\\s+){10}",createMusoLayers(unlist(rest$properties$PHIHOX$M)/10),outFile[50])
    writeLines(outFile,outputFile)
}
# createSoilFile(60,50)
