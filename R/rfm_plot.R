#' rfm_plot fucntion
#'
#' Plot the basic info after model is done
#'
#' @param model the results of rfm function -- a rfm model data.frame
#'
#'
#'
#' @return a serie of graphs
#'
#'
#' @export

rfm_plot <- function(model){

  #check package highcharter ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  if(!require(highcharter)){
    install.packages("highcharter")
    library(highcharter)
  }

  if(!require(htmltools)){
    install.packages("htmltools")
    library(htmltools)
  }



  #create the key prompt ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  #build the plot ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  f_dist <- as.data.frame(table(model$Fr))
  r_dist <- as.data.frame(table(model$Re))
  m_dist <- as.data.frame(table(model$Mo))


  hc_freq <- highchart()%>%
    hc_add_series(name = 'Frequency segment', data = f_dist$Freq, type = 'column', color = 'green',dataLabels = list(align = 'center', enabled = T , format = "{point.y}"))%>%
    hc_xAxis(categories = f_dist$Var1)



  hc_rec <- highchart()%>%
    hc_add_series(name = 'Recency segment', data = r_dist$Freq, type = 'column', color = 'blue',dataLabels = list(align = 'center', enabled = T , format = "{point.y}"))%>%
    hc_xAxis(categories = r_dist$Var1)



  hc_mon <- highchart()%>%
    hc_add_series(name = 'Value segment', data = m_dist$Freq, type = 'column', color = 'red', dataLabels = list(align = 'center', enabled = T , format = "{point.y}"))%>%
    hc_xAxis(categories = m_dist$Var1)

  hw_grid(list(hc_freq,hc_rec, hc_mon)  , rowheight = 250)%>% browsable()

}
