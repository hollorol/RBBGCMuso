#'OtableMaker
#'
#'This function is generating A and B table for musoRandomizer
#'
#'@param paramsReal The matrix of the parameters
#'@return List of two matrices: A and B matrix for musoRandomizer
#'@import stats
#'@import magrittr
#'@import dplyr
#'@export

OtableMaker <- function(parametersReal){
  constMatrix %<>% arrange(TYPE,GROUP)

  OTF<- t(apply(parametersReal,1,function(x){
    Group <- constMatrix[constMatrix$INDEX==x[1],"GROUP"]
    Type <- constMatrix[constMatrix$INDEX==x[1],"TYPE"]
    return(unlist(c(x,GROUP=Group,TYPE=Type)))
  })) %>% tbl_df() %>% arrange(TYPE,GROUP)


  groupIDs <- unique(OTF$GROUP)[-1]
  otfIndexes <- OTF$INDEX
  zeroIndexes <- OTF[OTF$GROUP==0,"INDEX"]  %>% as.data.frame() %>% unlist()
  OTFzero <- OTF[OTF$GROUP==0,]
  OT0 <- constMatrix [constMatrix$INDEX %in% zeroIndexes,] %>%
    mutate(MIN=OTFzero$MIN,MAX=OTFzero$MAX)

  sliced <- constMatrix %>%
      dplyr::filter(GROUP %in% groupIDs)
  slicedIndexes<- which(sliced[,"INDEX"] %in% intersect(sliced[,"INDEX"],otfIndexes))
  sliced[slicedIndexes,c("MIN","MAX")] <- OTF[which(OTF["GROUP"] == groupIDs),c("MIN","MAX")]
  
  OTbig <- rbind(OT0,sliced) %>% data.frame()
  parnumbers <- nrow(OTbig)

  for(i in 1:parnumbers){
    if(OTbig[i,1] %in% otfIndexes){
      OTbig[i,3] <- OTF[OTF$INDEX==OTbig[i,1],2]
      OTbig[i,4] <- OTF[OTF$INDEX==OTbig[i,1],3]
      if(OTbig$Type[i]==2){
        OTbig$DEPENDENCE[i] <- 2
      }
    }
  }

  summaries <- OTbig %>%
    group_by(TYPE,GROUP) %>%
    summarize(nGroup=n()) %>%
    select(nGroup,TYPE)
  return(list(Otable=OTbig,driver=summaries))

}

