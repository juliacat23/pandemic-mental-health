# function used to calculate and plot custom geom_text
# adapted from the 'Exploring ggplot2 boxplots – Defining limits and adjusting style' post by Laura DeCicco on the USGS blog

n_fun <- function(x){
  return(data.frame(y = 0.95*90, #places value of boxplot at 95% of upper limit (90)
                    label = length(x)))}