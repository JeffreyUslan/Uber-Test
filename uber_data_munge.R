
library(RJSONIO)

#importing data
json_file="uber_data_challenge.json"
uber_data<-fromJSON(json_file)

#forcing data into data frame
uber_unlist=data.frame()
for (i in 1:length(uber_data)){
  sub_df=data.frame(t(data.frame(unlist(uber_data[[i]]))))
  if (i==1){
    uber_unlist=sub_df
  } else {
    uber_unlist=rbind.all.columns(uber_unlist,sub_df)
  }
  
}

save(uber_unlist,file="uber_test_data.rda")




rbind.all.columns <- function(x, y) {
  #only works on data frames
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  if (!is.null(x)){
    if (length(y.diff)>0 & dim(x)[2]>0) {
      x[, c(as.character(y.diff))] <- NA
    }
    
    if (length(x.diff)>0 & dim(x)[2]>0) {
      y[, c(as.character(x.diff))] <- NA
    }
  }
  
  return(rbind(x,y))
  
  
} #end of function rbind.all.columns

