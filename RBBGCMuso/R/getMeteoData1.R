#' getMeteoData1BGC
#'
#' This function downloads hourly 2m air temperature and total precipitation values in NetCDF file format
#' at one grid point to create MTClim files which contain daily data.
#' Please note that, to download ERA5, you need Copernicus registration and API key.
#' According to default settings, data will be downloaded in the gridpoint of Martonvasar (Hungary), in 2017.
#'
#' @author Erzsebet Kristof, Roland Hollos
#' @param startYear Start year of the downloading data. It shall be greater than 1978.
#' @param endYear End year of the downloading data. It shall be smaller than 2019.
#' @param lon Geographical longitude of the selected grid point (negative values: W, positive values: E). It shall be between -180 and 180. The value is rounded to two decimal places.
#' @param lat Geographical latitude of the selected grid point (negative values: S, positive values: N). It shall be between -90 and 90. The value is rounded to two decimal places.
#' @param timeOut Time in seconds to wait to download from Copernicus.
#' @param monthList Month selector (e.g. to download data for April then write "01", to download data for September and October then write c("04","05")
#' @param dayList Day selector (e.g. to download data for the 1st day of the month(s) then write "01", to download data for the 2nd and 4th day of the month then write c("02","04")
#' @param hourList Hour selector (e.g. to download data for 00 UTC then write "00:00", to download data for 01 UTC and 15 UTC then write c("01","15")
#' @param fileDir Directory where the .ini and .mtcin files will be created
#' @param destDir Directory where the mtc43 file will be created. If it is NULL then destDir is the same as fileDir.
#' @param apiFile Directory where the cdsapirc file is located
#' @importFrom ecmwfr wf_set_key wf_request
#' @importFrom ncdf4 nc_open ncvar_get nc_close ncvar_put ncdim_def ncvar_def nc_create
#' @importFrom tcltk tk_choose.dir tk_choose.files
#' @export

getMeteoData1BG <- function(startYear=2017, endYear=2017, lon=18.8, lat=47.3, timeOut=7200,
                              monthList=sprintf("%02d",1:12),
                              dayList=sprintf("%02d",1:31),
                              hourList=sprintf("%02d:00",0:23),
                              destDir=NULL, apiFile=NULL, fileDir=NULL) {

  if ((startYear<1979)|(endYear>2018)) {
    stop("Error, please choose a year between 1979 and 2018.")
  }

  if (startYear>endYear) {
    stop("Error, start year shall be larger than end year.")
  }

   if(is.null(destDir)){
     destDir <- "mtcFiles"
   }

  # print("Please choose the file in the pop-up window which contains the CDS API key.")

  # With tk_choose.files, it is not working in R Server.
  if(is.null(apiFile)){
     apiFile <- tk_choose.files(caption = "Please choose the file which contains the CDS API key.")
   }
  
  if(is.null(fileDir)){
     apiFile <- tk_choose.files(caption = "Please choose the fileDir.")
   }

  apiCodes <- suppressWarnings(readLines(apiFile)[2])
  apiCodes <- unlist(strsplit(apiCodes,split=":"))
  userID <- trimws(apiCodes[2])
  APIkey <- trimws(apiCodes[3])

  # print("Please choose an empty folder in the pop-up window in which MTClim file will be generated.")
  currDir <- getwd()
   # fileDir <- tk_choose.dir(caption = "Please choose an empty folder in which MTClim file will be generated.")
  setwd(fileDir)
  dir.create(destDir)

  lonList <- expand.grid(lon,lat)[,1]
  latList <- expand.grid(lon,lat)[,2]

  for (grid in 1:length(lonList)) {

  if ((lonList[grid]>=180.01)|(lonList[grid]<=-180.01)) {
      stop("Error, please choose a geographical longitude between -180 and 180.")
    }

    if ((latList[grid]>=90.01)|(latList[grid]<=-90.01)) {
      stop("Error, please choose a geographical latitude between -90 and 90.")
    }

    lon <- round(lonList[grid], digits=2)
    lat <- round(latList[grid], digits=2)

    lonSub <- gsub("[.]", "_", as.character(lonList[grid]))
    latSub <- gsub("[.]", "_", as.character(latList[grid]))

    print("-----------------------------------------------------------------")
    print("1/6. WAITING TO COPERNICUS TO VALIDATE USER ID...")
    # Download 2m temperature data from ERA5:
    userID <- wf_set_key(user = userID,
                         key = APIkey,
                         service = "cds")

    print("-----------------------------------------------------------------")
    print("2/6. DOWNLOADING TEMPERATURE DATASET FROM ERA5 DATABASE. PLEASE WAIT...")

    request <- list(
                "dataset" = "reanalysis-era5-single-levels",
                "class" = "ea",
                "expver" = "1",
                "stream" = "oper",
                "product_type" = "reanalysis",
                "variable" = "167.128",
                "year" = as.character(startYear:endYear),
                "month" = monthList,
                "day" = dayList,
                "time" = hourList,
                "area" = paste0(lat,"/",lon,"/",lat,"/",lon), # N, W, S, E
                "grid" = "0.1/0.1",
                "format" = "netcdf",
                "target" = paste0("ERA5_hourly_t2m_",startYear,"-",
                                   endYear,"_lat_",lat,"_lon_",lon,".nc"))

    file <- wf_request(user     = userID,   # user ID (for authentification)
                       request  = request,  # the request
                       transfer = TRUE,     # download the file
                       path     = fileDir,
                       time_out = timeOut)

    print("-----------------------------------------------------------------")
    print("3/6. DOWNLOADING PRECIPITATION DATASET FROM ERA5 DATABASE. PLEASE WAIT...")

    # Download total precipitation data from ERA5:
    request <- list(
                "dataset" = "reanalysis-era5-single-levels",
                "class" = "ea",
                "expver" = "1",
                "stream" = "oper",
                "product_type" = "reanalysis",
                "variable" = "228.128",
                "year" = as.character(startYear:endYear),
                "month" = monthList,
                "day" = dayList,
                "time" = hourList,
                "area" = paste0(lat,"/",lon,"/",lat,"/",lon), # N, W, S, E
                "grid" = "0.1/0.1",
                "format" = "netcdf",
                "target" = paste0("ERA5_hourly_precip_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"))

      file <- wf_request(user     = userID,   # user ID (for authentification)
                         request  = request,  # the request
                         transfer = TRUE,     # download the file
                         path     = fileDir,
                         time_out = timeOut)

    print("-----------------------------------------------------------------")
    print("4/6. DOWNLOADING GEOPOTENTIAL DATASET FROM ERA5 DATABASE. PLEASE WAIT...")

    # Download geopotential height data from ERA5:
    request <- list(
                "dataset" = "reanalysis-era5-single-levels",
                "class" = "ea",
                "expver" = "1",
                "stream" = "oper",
                "product_type" = "reanalysis",
                "variable" = "129.128",
                "year" = as.character(startYear:endYear),
                "month" = monthList,
                "day" = dayList,
                "time" = hourList,
                "area" = paste0(lat,"/",lon,"/",lat,"/",lon), # N, W, S, E
                "grid" = "0.1/0.1",
                "format" = "netcdf",
                "target" = paste0("ERA5_hourly_geopot_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"))

      file <- wf_request(user     = userID,   # user ID (for authentification)
                         request  = request,  # the request
                         transfer = TRUE,     # download the file
                         path     = fileDir,
                         time_out = timeOut)

    print("-----------------------------------------------------------------")
    print("5/6. CREATING MTCLIM FILES...")

    ### Creating MTClim files:

    ## Daily minimum and maximum temperature:

    file.nc <- nc_open(paste0("ERA5_hourly_t2m_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"))
    longitude <- ncvar_get(file.nc, "longitude")
    latitude <- ncvar_get(file.nc, "latitude")
    timesteps <- ncvar_get(file.nc, "time")
    date <- as.POSIXct(timesteps*3600, origin="1900-01-01 00:00:0.0",tz="UTC")
    var <- ncvar_get(file.nc, "t2m")-273.15 # in Celsius
    nc_close(file.nc)

    # Handling leap years:
    leapYear <- c("1952-12-31","1956-12-31","1960-12-31","1964-12-31","1968-12-31",
                  "1972-12-31","1976-12-31","1980-12-31","1984-12-31","1988-12-31",
                  "1992-12-31","1996-12-31","2000-12-31","2004-12-31","2008-12-31",
                  "2012-12-31","2016-12-31","2020-12-31","2024-12-31")

    leapYearIndex <- list()
    for (index in 1:length(leapYear)){
      if((length(which(grepl(leapYear[index], date)))>0)==TRUE){
        leapYearIndex[[index]] <- which(grepl(leapYear[index], date))
      }
    }

    leapYearIndex <- unlist(leapYearIndex)

    leapYearIndex <- na.omit(leapYearIndex)

    if(length(leapYearIndex)>0){
      timesteps <- timesteps[-leapYearIndex]
      date <- date[-leapYearIndex]
      var <- var[-leapYearIndex]
    }

    dailyT2mMin <- c(0)
    dailyT2mMax <- c(0)
    dailyT2mAvg <- c(0)

    if(dim(var)%%24==0) {
      for (i in seq(1,dim(date),24)) {
        dailyT2mMin[i] <- min(var[i:(i+23)])

        dailyT2mMax[i] <- max(var[i:(i+23)])

        dailyT2mAvg[i] <- mean(var[i:(i+23)])

      }
    }

    dailyT2mMin <- na.omit(dailyT2mMin)
    dailyT2mMax <- na.omit(dailyT2mMax)

    ## Estimating elevation:
    file.nc <- nc_open(paste0("ERA5_hourly_geopot_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"))
    var2 <- ncvar_get(file.nc, "z") # in m**2/s**2
    nc_close(file.nc)

    DB <- data.frame(0)
    DB[1,1] <- 1
    DB[1,2] <- 0
    DB[1,3] <- lat
    DB[1,4] <- lon
    DB[1,5] <- mean(var2)/9.80665
    DB[1,6] <- 0
    DB[1,7] <- 0
    DB[1,8] <- mean(var)
    DB[1,9] <- mean(dailyT2mAvg, na.rm=TRUE)

    colnames(DB) <- c("new_id", "100m_rep_id",	"lat (deg)", "long (deg)",
                      "elev (m)",	"fboritas", "nuts3", "Tavg", "dTavg")

    # Save lon, lat values into csv:
    write.table(lon, "Lon.csv", sep=";", row.names=FALSE, col.names=FALSE)
    write.table(lat, "Lat.csv", sep=";", row.names=FALSE, col.names=FALSE)
    write.table(DB, "DB_GRID_TABLE.csv", sep=";", row.names=FALSE)

    # Save the data into NetCDF files: tmin and tmax

    lon_nc <- longitude
    lat_nc <- latitude
    time_nc <- timesteps[seq(1,dim(timesteps),24)]

    longitude <- ncdim_def(name="longitude", units="degree", vals=lon_nc, longname="geographical longitude")
    latitude <- ncdim_def(name="latitude", units="degree", vals=lat_nc, longname="geographical latitude")
    time <- ncdim_def(name="time", units="day", vals=time_nc, longname="timesteps")

    mv <- -999

    variable_Tmin <- ncvar_def(name="Tmin", units="Celsius", dim=list(longitude,latitude,time),
                               missval=mv, longname="Daily minimum temperature", prec="double")

    variable_Tmax <- ncvar_def(name="Tmax", units="Celsius", dim=list(longitude,latitude,time),
                               missval=mv, longname="Daily maximum temperature", prec="double")

    nc_min <- nc_create(filename=paste0("ERA5_daily_t2m_min_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"), vars=list(variable_Tmin))
    nc_max <- nc_create(filename=paste0("ERA5_daily_t2m_max_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"), vars=list(variable_Tmax))

    ncvar_put(nc_min, varid=variable_Tmin, vals=dailyT2mMin, start=c(1,1,1),
              count=c(dim(lon_nc),dim(lat_nc),length(timesteps[seq(1,dim(timesteps),24)])))

    ncvar_put(nc_max, varid=variable_Tmax, vals=dailyT2mMax, start=c(1,1,1),
              count=c(dim(lon_nc),dim(lat_nc),length(timesteps[seq(1,dim(timesteps),24)])))

    nc_close(nc_min)
    nc_close(nc_max)

    ## Daily precipitation sums:

    file.nc <- nc_open(paste0("ERA5_hourly_precip_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"))
    longitude <- ncvar_get(file.nc, "longitude")
    latitude <- ncvar_get(file.nc, "latitude")
    timesteps <- ncvar_get(file.nc, "time")
    date <- as.POSIXct(timesteps*3600, origin="1900-01-01 00:00:0.0",tz="UTC")
    var <- ncvar_get(file.nc, "tp")*1000 # in mm
    nc_close(file.nc)

    # Handling leap years:
    leapYearIndex <- list()
    for (index in 1:length(leapYear)){
      if((length(which(grepl(leapYear[index], date)))>0)==TRUE){
        leapYearIndex[[index]] <- which(grepl(leapYear[index], date))
      }
    }

    leapYearIndex <- unlist(leapYearIndex)

    leapYearIndex <- na.omit(leapYearIndex)

    if(length(leapYearIndex)>0){
      timesteps <- timesteps[-leapYearIndex]
      date <- date[-leapYearIndex]
      var <- var[-leapYearIndex]
    }

    dailyPrecipSum <- c(0)

    if(dim(var)%%24==0) {
      for (i in seq(1,dim(date),24)) {
        dailyPrecipSum[i] <- sum(var[i:(i+23)])
      }
    }

    dailyPrecipSum <- na.omit(dailyPrecipSum)
    summary(dailyPrecipSum)
    length(dailyPrecipSum)
    length(dailyPrecipSum)

    # Save the data into NetCDF files: precip

    # Dimensions:
    lon_nc <- longitude
    lat_nc <- latitude
    time_nc <- timesteps[seq(1,dim(timesteps),24)]

    longitude <- ncdim_def(name="longitude", units="degree", vals=lon_nc, longname="geographical longitude")
    latitude <- ncdim_def(name="latitude", units="degree", vals=lat_nc, longname="geographical latitude")
    time <- ncdim_def(name="time", units="day", vals=time_nc, longname="timesteps")

    mv <- -999

    variable <- ncvar_def(name="prcp", units="mm", dim=list(longitude,latitude,time),
                          missval=mv, longname="Daily precipitation sums", prec="double")

    nc <- nc_create(filename=paste0("ERA5_daily_precip_sum_",startYear,"-",endYear,"_lat_",lat,"_lon_",lon,".nc"), vars=list(variable))

    ncvar_put(nc, varid=variable, vals=dailyPrecipSum, start=c(1,1,1),
              count=c(dim(lon_nc),dim(lat_nc),length(timesteps[seq(1,dim(timesteps),24)])))

    nc_close(nc)

    listifyNCDF <- function(ncFile, dataLab, lat, lon, gridCoords){
      dataCon <- nc_open(ncFile)
      dataVal <- ncvar_get(dataCon, dataLab)
      lonVal <- lon
      latVal <- lat
      dataCoords <- expand.grid(latVal[1],lonVal[1])
      numberOfGridCells <- nrow(gridCoords)

      dataOut <- list()
      for(i in 1: numberOfGridCells){
        dataOut[[i]] <- dataVal
      }
      return(dataOut)
    }

    daily_precip.nc <- nc_open(paste0("ERA5_daily_precip_sum_", startYear,
                              "-", endYear, "_lat_", lat, "_lon_", lon, ".nc"))
    daily_tmin.nc <- nc_open(paste0("ERA5_daily_t2m_min_", startYear,
                              "-", endYear, "_lat_", lat, "_lon_", lon, ".nc"))
    daily_tmax.nc <- nc_open(paste0("ERA5_daily_t2m_max_", startYear,
                                    "-", endYear, "_lat_", lat, "_lon_", lon, ".nc"))

    tmin <- ncvar_get(daily_tmin.nc, "Tmin")
    precip <- ncvar_get(daily_precip.nc, "prcp")
    tmax <- ncvar_get(daily_tmax.nc, "Tmax")

    nc_close(daily_precip.nc)
    nc_close(daily_tmin.nc)
    nc_close(daily_tmax.nc)

    lon <- read.csv2("Lon.csv", stringsAsFactors = FALSE, header = FALSE)
    lat <- read.csv2("Lat.csv", stringsAsFactors = FALSE, header = FALSE)
    gridCoords <- read.csv2("DB_GRID_TABLE.csv", stringsAsFactors = FALSE)
    gridSkeleton <- gridCoords[,c(1,3,4)]

    precip <- listifyNCDF(ncFile = paste0("ERA5_daily_precip_sum_",startYear,"-",
                                          endYear,"_lat_",lat,"_lon_",lon,".nc"),
                          dataLab = "prcp", # tp
                          lon = lon,
                          lat = lat,
                          gridCoords = gridSkeleton)
    precip <- lapply(precip, function(x) x/10)

    tmin <- listifyNCDF(ncFile = paste0("ERA5_daily_t2m_min_",startYear,"-",
                                        endYear,"_lat_",lat,"_lon_",lon,".nc"),
                        dataLab = "Tmin",
                        lon = lon,
                        lat = lat,
                        gridCoords = gridSkeleton)

    tmax <- listifyNCDF(ncFile = paste0("ERA5_daily_t2m_max_",startYear,"-",
                                        endYear,"_lat_",lat,"_lon_",lon,".nc"),
                        dataLab = "Tmax",
                        lon = lon,
                        lat = lat,
                        gridCoords = gridSkeleton)
    numberOfSites <- length(tmax)
    tmp <- tmin

    mtcMaker <- function(simulationName = "", executable, minYear, startYear, numYears,
                         maxTemperatures, minTemperatures, precipitations, sites, destDir){
      # executable <- normalizePath(executable) # make absolute from relative path
      file.copy(executable,getwd())
      executable <- paste0("./",basename(executable))
      iniTemp <- c(paste0("Simulation is based on: ",simulationName),"","IOFILES",
                   "1.mtcin",
                   "1", "CONTROL", "0", ((endYear-startYear)+1)*365, #"365",
                   "0", "0", "1", "PARAMETERS",
                   "1000.0", "60.0", "45.0", "1000.0",
                   "0.0", "0.0", "60.0", "0.0", "0.0",
                   "-6.0", "-3.0", "END")
      endYear <- startYear + numYears - 1

      mtDate <- function(startYear, numYears, endYear){
        ## generate the first two column for mtcin
        year <- rep(startYear:endYear, each = 365)
        yearday <- rep(1:365,numYears)
        return(cbind(year,yearday))
      }

      sliceData <- function(minYear, startYear, numYears, data){
        ## get the data from the needed perion.
        baseDiff <- startYear-minYear
        startDate <- baseDiff*365+1
        endDate <- (baseDiff+numYears)*365
        return(data[startDate:endDate])
      }

      nSites <- nrow(sites)
      for(i in 1:nSites) {
        print(i)
        iniTemp[4] <- sprintf("%d.mtcin",i)
        iniTemp[8] <- numYears*365
        iniTemp[5] <- file.path(destDir,i)
        iniTemp[13] <- iniTemp[16] <- sites[i,5]
        iniTemp[15] <- sites[i,3] #LAT is the first col.

        write.table(iniTemp,paste(i, ".ini", sep=""),col.names=FALSE,row.names=FALSE,
                    sep="\n",quote=FALSE)

        itmax <- sliceData(minYear,startYear,numYears,maxTemperatures[[i]])
        itmin <- sliceData(minYear,startYear,numYears,minTemperatures[[i]])
        iprecip <- sliceData(minYear,startYear,numYears,precipitations[[i]])
        dateData <- mtDate(startYear,numYears,endYear)

        write.table(cbind(dateData,itmax,itmin,iprecip),paste(i, ".mtcin", sep="")
                    ,row.names=FALSE,col.names=FALSE)

        system2(executable, paste0(i, ".ini"), stdout=NULL)
      }
    }

    executable <- if(Sys.info()["sysname"]=="Windows"){
      paste0(system.file("",package="MTClimMaker"),"/mtclim43.exe")
    } else {
      paste0(system.file("",package="MTClimMaker"),"/mtclim43")
    }

    mtcMaker(simulationName = paste0("lat. ", gsub("_",".",latSub), ", lon. ", gsub("_",".",lonSub), ", elev. ",
             round(as.numeric(gridCoords[1,5]), digits=1), " m, ", "Note that VPD and srad are computed by MTClim."),
             executable = executable,
             # executable = paste0(system.file("",package="MTClimMaker"),"/mtclim43"),
      	     minYear = startYear,
             startYear = startYear,
             numYears = (endYear-startYear)+1,
             maxTemperatures = tmax,
             minTemperatures = tmin,
             precipitations = precip,
             sites = gridCoords,
             destDir = destDir)
    setwd(currDir)

    print("-----------------------------------------------------------------")
    # print("6/6. MOVING .mtcin, .ini AND .mtc43 FILES INTO THE CORRECT FOLDER.")
    print("6/6. RENAMING .mtcin, .ini AND .mtc43 FILES...")


    ### Move the ini, mtcin, mtc43 files into the correct folder.
    # Moving files to the correct folder:
    # dir.create(paste0("lat", latSub, "_lon", lonSub))
    # file.copy("1.ini", paste0("lat", latSub, "_lon", lonSub))
    # file.copy("1.mtcin", paste0("lat", latSub, "_lon", lonSub))
    # file.copy(paste0(destDir,"/1.mtc43"), paste0("lat", latSub, "_lon", lonSub))

    # file.rename(paste0("lat", latSub, "_lon", lonSub, "/1.ini"),
    #             paste0("lat", latSub, "_lon", lonSub,  "/", grid,
    #                    "_lat", latSub, "_lon", lonSub, ".ini"))
    #
    # file.rename(paste0("lat", latSub, "_lon", lonSub, "/1.mtcin"),
    #             paste0("lat", latSub, "_lon", lonSub,  "/", grid,
    #                    "_lat", latSub, "_lon", lonSub, ".mtcin"))
    #
    # file.rename(paste0("lat", latSub, "_lon", lonSub, "/1.mtc43"),
    #             paste0("lat", latSub, "_lon", lonSub, "/", grid,
    #                    "_lat", latSub, "_lon", lonSub, ".mtc43"))
    ###

    file.copy(paste0(destDir,"/1.mtc43"), fileDir)

    file.rename("1.ini",
                paste0(grid,
                       "_lat", latSub, "_lon", lonSub, ".ini")) # ID number (1,2,...) can be added to the title by using object grid befor "_lat"

    file.rename("1.mtcin",
                paste0(grid,
                       "_lat", latSub, "_lon", lonSub, ".mtcin")) # ID number (1,2,...) can be added to the title by using object grid befor "_lat"

    file.rename("1.mtc43",
                paste0(grid,
                       "_lat", latSub, "_lon", lonSub, ".mtc43")) # ID number (1,2,...) can be added to the title by using object grid befor "_lat"

    # file.rename("1.ini", paste0(sprintf('%0.4d', gridID),".ini"))
    #
    # file.rename("1.mtcin", paste0(sprintf('%0.4d', gridID),".mtcin"))
    #
    # file.rename("1.mtc43", paste0(sprintf('%0.4d', gridID),".mtc43"))

    print(paste0("MTCLIM FILES ARE CREATED FOR THE GRID POINT: LAT ",
                 latList[grid], " & LON ", lonList[grid], "."))

    print(paste0("SUMMARY: ",
                 "elevation: ", round(as.numeric(gridCoords[1,5]), digits=1), " m, ",
                 "yearly average temperature: ", round(as.numeric(gridCoords[1,8]), digits=1), " C"))

  # Removing unnecessary files:
     file.remove("DB_GRID_TABLE.csv", "Lat.csv", "Lon.csv", "1.ini", "1.mtcin",
                 "mtclim43.exe", list.files(pattern="ERA5_hourly*"), list.files(pattern="ERA5_daily*"))
  }

  unlink(destDir, recursive=TRUE)
}
