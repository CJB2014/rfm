#' rfm_model fucntion
#'
#' Load data in to create model
#'
#' @param df a data frame loaded with rfm_load or created previously
#' @param n_freq an integer between 2 and 5 representing the number of quantile to create for the frequency
#' @param n_rec an integer between 2 and 5 representing the number of quantile to create for the recency
#' @param n_mon an interget between 2 and 5 representing the number of quantile to create for the monetary score
#' @param score a logical default is TRUE which add a concatenated rfm score as a variable in the return object. If FALSE then the score will not be added
#'
#'
#' @return a Data.frame object
#'
#'
#' @export


rfm <- function(df,
                n_freq = c(2:5),
                n_rec = c(2:5),
                n_mon = c(2:5),
                score = TRUE) {


  #Checks------------------------------------------------------------------

  if (is.data.frame(df)) {
    print('Starting the customers segmentation')
  } else {
    print('df needs to be a data frame')
  }

  if (n_freq <2 | n_freq>5 ){
    print('n_freq needs to be between 2 and 5')
  }

  if (n_rec <2 | n_rec>5 ){
    print('n_rec needs to be between 2 and 5')
  }

  if (n_mon <2 | n_mon>5 ){
    print('n_mon needs to be between 2 and 5')
  }

  #build quantile------------------------------------------------------------------

  if (n_freq == 2){
    f <- quantile(df$frequency, c(0.5))
  } else if (n_freq == 3){
    f <- quantile(df$frequency, c(0.33,0.66,1))
  } else if (n_freq == 4){
    f <- quantile(df$frequency, c(0.25,0.5,0.75,1))
  } else if (n_freq == 5){
    f<- quantile(df$frequency, c(0.2,0.4,0.6,0.8,1))
  }


  if (n_rec == 2){
    r <- quantile(df$recency, c(0.5))
  } else if (n_rec == 3){
    r <- quantile(df$recency, c(0.33,0.66,1))
  } else if (n_rec == 4){
    r <- quantile(df$recency, c(0.25,0.5,0.75,1))
  } else if (n_rec == 5){
    r<- quantile(df$recency, c(0.2,0.4,0.6,0.8,1))
  }


  if (n_mon == 2){
    m <- quantile(df$monetary, c(0.5))
  } else if (n_mon == 3){
    m <- quantile(df$monetary, c(0.33,0.66,1))
  } else if (n_mon == 4){
    m <- quantile(df$monetary, c(0.25,0.5,0.75,1))
  } else if (n_mon == 5){
    m<- quantile(df$monetary, c(0.2,0.4,0.6,0.8,1))
  }


  #create segment ------------------------------------------------------------------
## frequency
  if (n_freq == 2){

    df$Fr <- ifelse(df$frequency<f[1],2,1)


  } else if (n_freq == 3){

    df$Fr <- ifelse(df$frequency<f[1],3,
                    ifelse(df$frequency>=f[1] & df$frequency<f[2],2,1))

  } else if (n_freq == 4){

    df$Fr <- ifelse(df$frequency<f[1],4,
                    ifelse(df$frequency>=f[1] & df$frequency<f[2],3,
                           ifelse(df$frequency>=f[2] & df$frequency<f[3],2,1)))
  } else if (n_freq == 5){

    df$Fr <- ifelse(df$frequency<f[1],5,
                    ifelse(df$frequency>=f[1] & df$frequency<f[2],4,
                           ifelse(df$frequency>=f[2] & df$frequency<f[3],3,
                                  ifelse(df$frequency>=f[3] & df$frequency<f[4],2,1))))
  }

##recency
  if (n_rec == 2){

    df$Re <- ifelse(df$recency<r[1],2,1)


  } else  if (n_rec == 3){

    df$Re <- ifelse(df$recency<r[1],3,
                    ifelse(df$recency>=r[1] & df$recency<r[2],2,1))

  } else if (n_rec == 4){

    df$Re <- ifelse(df$recency<r[1],4,
                    ifelse(df$recency>=r[1] & df$recency<r[2],3,
                           ifelse(df$recency>=r[2] & df$recency<r[3],2,1)))
  } else if (n_rec == 5){

    df$Re <- ifelse(df$recency<r[1],5,
                    ifelse(df$recency>=r[1] & df$recency<r[2],4,
                           ifelse(df$recency>=r[2] & df$recency<r[3],3,
                                  ifelse(df$recency>=r[3] & df$recency<r[4],2,1))))
  }

##Monetary
    if (n_mon == 2){

      df$Mo <- ifelse(df$monetary<m[1],2,1)


    } else if (n_mon == 3){

      df$Mo <- ifelse(df$monetary<m[1],3,
                      ifelse(df$monetary>=m[1] & df$monetary<m[2],2,1))

    } else if (n_mon == 4){

      df$Mo <- ifelse(df$monetary<m[1],4,
                      ifelse(df$monetary>=m[1] & df$monetary<m[2],3,
                             ifelse(df$monetary>=m[2] & df$monetary<m[3],2,1)))
    } else if (n_mon == 5){

      df$Mo <- ifelse(df$monetary<m[1],5,
                      ifelse(df$monetary>=m[1] & df$monetary<m[2],4,
                             ifelse(df$monetary>=m[2] & df$monetary<m[3],3,
                                    ifelse(df$monetary>=m[3] & df$monetary<m[4],2,1))))
    }

  df$Re <- as.factor(df$Re)
  df$Mo <- as.factor(df$Mo)
  df$Fr <- as.factor(df$Fr)

  if(score == TRUE){
    df$score <- paste(df$Re,df$Fr,df$Mo, sep = '')
  } else {
    print('no score will be created')
  }

  return(df)

}
