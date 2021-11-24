# funtion to move the header and column one level up
setFertilityRateHeaders <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  #rownames(df) <- as.character(unlist(df[,1]))
  names(df)[1]="years"
  df[-1,]
}

setLabHeaders <- function(df, col, row) {
  if(col != 0){
    names(df) <- as.character(unlist(df[col*-1,]))
  }
  if (row != 0) {
    rownames(df) <- as.character(unlist(df[,row*-1]))
  }
  df[col,row]
}
