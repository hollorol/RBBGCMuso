#' getSoilDataFull
#' 
#' This function collects soil data from a given restapi, de default is soilGrid
#'
#' @author Roland HOLLÓS
#' @name getSoilDataFull
#' @importFrom glue glue
#' @importFrom httr  config with_config GET content

getSoilDataFull <- function(lat, lon, apiURL) {
    if(missing(apiURL)){
        apiURL <- "https://rest.isric.org/soilgrids/v2.0/properties"
    }
    apiString <- glue("{apiURL}/query?lon={lon}&lat={lat}")
    soilREST <- #with_config(config(ssl_verifypeer=0L, ssl_verifyhost=0L),
                            GET(apiString) # ) # This is temporary solution ssl_verification wont bypass
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
                            template=system.file("examples/hhs/hhs_MuSo7.soi",package="RBBGCMuso")) {
    if(missing(apiURL)){
        apiURL <- "https://rest.isric.org/soilgrids/v2.0/properties"
    }
    outFile <- suppressWarnings(readLines(template))
    outFile[1] <- sprintf("SOILPROP FILE - lat: %s, lon: %s, created in: %s",lat,lon,date())
    musoCenters <- c(1.5,6.5,20.0,45.0,75.0,105.0,135.0,175.0,300.0,700.0)
    # soilGridDepths  <- c(0,5,15,30,60,100,200)
    soilGridDepths  <- c(2.5, 10, 22.5, 45, 80, 150)
    Reduce(function(x,y){(y-x)/2+x},soilGridDepths,accumulate=TRUE)
    rest<- getSoilDataFull(lat,lon, apiURL)


    createMusoLayers <- function(values,depths=soilGridDepths,centers=musoCenters,intMethod=method){
        approx(x=depths,y=values, xout = centers, method=intMethod,rule=2)$y %>%
            paste(.,collapse="\t") %>% paste0(.," ")
    }

    soilDepth <- tryCatch(getMeanSoil(rest,"bdod")/100,error=function(e){stop("There is no data for the given coordinates")})
    outFile[90] <- sprintf("%s (%%) percentage of sand by volume in rock free soil",
                           paste(createMusoLayers(getMeanSoil(rest,"sand")/10), collapse="\t"))
    outFile[91] <- sprintf("%s (%%) percentage of silt by volume in rock free soil",
                           paste(createMusoLayers(getMeanSoil(rest,"silt")/10), collapse="\t"))
    outFile[92] <- sprintf("%s (dimless) soil PH",
                           paste(createMusoLayers(getMeanSoil(rest,"phh2o")/10), collapse="\t"))
    # outFile[58] <- sprintf("%s (%%) bulk density",paste(createMusoLayers(soilDepth),collapse="\t"))
    writeLines(outFile,outputFile)
}
# createSoilFile(60,50)

getMeanSoil <- function(rest, name){
    sapply(
        rest$properties$layers[sapply(rest$properties$layers,function(x){
                   x$name == name
                })][[1]]$depths,
           function(s){
                s$values$mean
           }
    )
}
