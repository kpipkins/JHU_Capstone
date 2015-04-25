predict_bi <- function(input) {
  bi.freq <- read.csv("bi_freq.csv")
  input_clean <- CleanData_en(input)
  input_clean <- as.character(input_clean[[1]])
  input_clean <- tail(strsplit(input_clean,split=" ")[[1]],1)
  input_clean <- paste("^\\b", input_clean, "\\b ", sep="")
  find <- bi.freq[grep(input_clean, bi.freq$terms),]
  find <- find[order(-xtfrm(find$probability)),]
  result <- as.character(find$terms)
  if(length(result)==0){
    return(NULL)
  } else {
  result <- matrix(unlist(strsplit(result, " ")), ncol=2, byrow=TRUE)
  result <- result[,2]
  return(result[1])
  }
}

predict_tri <- function(input) {
  tri.freq <- read.csv("tri_freq.csv")
  input_clean <- CleanData_en(input)
  input_clean <- as.character(input_clean[[1]])
  input_clean <- tail(strsplit(input_clean,split=" ")[[1]],2)
  input_clean <- paste(input_clean,collapse=" ")
  input_clean <- paste("^\\b", input_clean, "\\b", sep="")
  find <- tri.freq[grep(input_clean, tri.freq$terms),]
  find <- find[order(-xtfrm(find$probability)),]
  result <- as.character(find$terms)
  if(length(result)==0){
    return(NULL)
  } else {
  result <- matrix(unlist(strsplit(result, " ")), ncol=3, byrow=TRUE)
  result <- result[,3]
  return(result[1])
  }
}

predict_quad <- function(input) {
  quad.freq <- read.csv("quad_freq.csv")
  input_clean <- CleanData_en(input)
  input_clean <- as.character(input_clean[[1]])
  input_clean <- tail(strsplit(input_clean,split=" ")[[1]],3)
  input_clean <- paste(input_clean,collapse=" ")
  input_clean <- paste("^\\b", input_clean, "\\b", sep="")
  find <- quad.freq[grep(input_clean, quad.freq$terms),]
  find <- find[order(-xtfrm(find$probability)),]
  result <- as.character(find$terms)
  if(length(result)==0){
    return(NULL)
  } else {
  result <- matrix(unlist(strsplit(result, " ")), ncol=4, byrow=TRUE)
  result <- result[,4]
  return(result[1])
  }
}

predict <- function(input) {
  Qterm <- predict_quad(input)
  Tterm <- predict_tri(input)
  Bterm <- predict_bi(input)
  if(length(Qterm)==1){
    stop(return(Qterm))
  } else if(length(Tterm)==1){
    stop(return(Tterm))
  } else if(length(Bterm)==1){
    stop(return(Bterm))
  } else {
    return("Sorry, the model couldn't find a match; try another word.")
  }
}
