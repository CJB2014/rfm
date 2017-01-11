#' rfm_load fucntion
#'
#' Load data in to create model
#'
#' @param file Path of the file that need to
#' @return a Data.frame object
#'
#'
#' @export


rfm_load <- function(file){

  # load data data in csv

  rfm_data <- read.csv(file = file, header = T, sep = ',')
  print('Data have been loaded')

  return(rfm_data)

}
