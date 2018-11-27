musoFilter <- function(text){
    eval(parse(paste0("filter(.,",text,")"))) %>%
        tbl_df
}
