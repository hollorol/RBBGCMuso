library(RBBGCMuso)
file.copy("../../start_set/maize.epc","./start_set/maize.epc",overwrite=TRUE)
setwd("start_set/")

rmse  <- function(modelled, measured){
    sqrt(mean((modelled-measured)**2))
}

r2 <- function(modelled, measured){
    summary(lm("mod ~ meas", data= data.frame(mod=modelled,meas=measured)))$r.squared
}

modeff <- function(modelled, measured){
    1 - (sum((modelled-measured)**2) / sum((measured - mean(measured))**2))
}

bias <- function(modelled, measured){
    mean(modelled) - mean(measured)
}

get_stats <- function(modelled,measured){
    c(r2=r2(modelled,measured),
      rmse=rmse(modelled,measured),
      bias=bias(modelled,measured),
      modeff=modeff(modelled,measured))
}

get_modelled <- function(obsTable,settings, ...){
    simulation <- runMuso(settings, ...)
    yield <- simulation[,"fruit_DM"]
    modelled <- yield[match(as.Date(obsTable$date),as.Date(names(yield),"%d.%m.%Y"))] * 10
    modelled 
}

obsTable <- read.csv2("Martonvasar_maize.obs",stringsAsFactors=FALSE)
obsTable$mean <- obsTable$mean / 1000 * 0.85
obsTable$sd <- obsTable$sd / 1000
measured <- obsTable$mean

# apriori
settings <- setupMuso(iniInput=c("n.ini","n.ini"))
modelled <- get_modelled(obsTable, settings)
results <- matrix(ncol=4,nrow=11)
colnames(results) <- c("r2","rmse","bias","modeff")
row.names(results) <- 0:10
results[1,] <- get_stats(modelled,measured)
# max_likelihood_stats
for(i in 1:10){
    file.copy(sprintf("../%s/maize_ml.epc",i),"maize.epc",overwrite=TRUE)
    settings <- setupMuso(iniInput=c("n.ini","n.ini"))
    modelled <- get_modelled(obsTable, settings)
    results[i+1,] <- get_stats(modelled, measured)
}
# median_stats
results_med <- results

for(i in 1:10){
    file.copy(sprintf("../maize_median_step%02d_corrected.epc",i),"maize.epc",overwrite=TRUE)
    settings <- setupMuso(iniInput=c("n.ini","n.ini"))
    modelled <- get_modelled(obsTable, settings)
    results_med[i+1,] <- get_stats(modelled, measured)
}
results_med

succes_ratio <- numeric(10)
for(i in 1:10){
    succes_ratio[i] <- sum(read.csv(sprintf("../%s/result.csv",i),stringsAsFactors=FALSE)$Const[-1])/10000
}
names(succes_ratio) <- 1:10
png("../success_rate.png",height=30,width=30,res=300, units="cm")
barplot(succes_ratio,ylim=c(0,1),xlab="iteration number",ylab="Succes rate")
dev.off()

postscript("../success_rate.eps")
barplot(succes_ratio,ylim=c(0,1),xlab="iteration number",ylab="Succes rate")
dev.off()
