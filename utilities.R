# funtion to move the header and column one level up
setXY <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  #rownames(df) <- as.character(unlist(df[,1]))
  names(df)[1]="years"
  df[-1,]
}
