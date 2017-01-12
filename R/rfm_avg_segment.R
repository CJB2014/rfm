#' rfm_avg_segment fucntion
#'
#' Summarise the average value for each existing score
#'
#' @param model a data frame loaded with rfm_load or created previously
#'
#'
#' @return a Data.frame object
#'
#'
#' @export

rfm_avg_segment <- function(model){

  if(!require(data.table)){
    install.packages("data.table")
    library(data.table)
  }

  model <- as.data.table(model)

  avg_score <- hf[, .(.N, rec = mean(recency), freq = mean(frequency),mon = mean(monetary)), by= (score)]

  return(avg_score)


  }







