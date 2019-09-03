trimColorSet <- function(minim, maxim, center=NULL, nticks=6, roundPrecision=NULL, reverseColorScale=FALSE, colorSet="RdYlGn") {
  

  if (!is.element(colorSet,rownames(brewer.pal.info))) {
    stop(sprintf("Invalid colorset, please choose from the followings:\n %s", paste(availableColors, collapse=", ")))
  }
  
  numBaseColors <- brewer.pal.info[colorSet,1]
  
  nsteps <- (maxim-minim)/nticks
  
  if(is.null(center)){
    if(reverseColorScale==TRUE) {
      colorbar <- rev(colorRampPalette(brewer.pal(numBaseColors,colorSet))(length(breaks)-1))
    } else {
      colorbar <- colorRampPalette(brewer.pal(numBaseColors,colorSet))(length(breaks)-1)
    }
    return(colorbar)
  }
    
  breaks_full <- (function(delta){
    newRange <- c(center-delta,center+delta)
    breaks <- seq(newRange[1],newRange[2],nsteps+1)
    return(breaks)
    })(
    max((center - minim), (maxim-center))
    )
  
  breakConditions <- (breaks_full >= minim) & (breaks_full <= maxim)

  
  if(reverseColorScale==TRUE) {
    colorbar <- rev(colorRampPalette(brewer.pal(numBaseColors,colorSet))(length(breaks)-1))
  } else {
    colorbar <- colorRampPalette(brewer.pal(numBaseColors,colorSet))(length(breaks)-1)
  }
  return(colorbar[breakConditions])
  
}
  