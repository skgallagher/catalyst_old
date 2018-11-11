#' Plot the % of individuals
#'
#' @param X_df Data frame with the first K columns as the number of individuals in each compartment and the next two columns as tt the time and ll the simulation number
#' @param xlab xlab title
#' @param ylab ylab title
#' @param cols colors to use for the curves
#' @param cat_names category names we see
#' @return a ggplot
plot_sum_K <- function(X_df,
                       xlab,
                       ylab,
                       cols = c("firebrickred", "blue"),
                       cat_names = c("S", "I")
                       ){
    

}




#' My custom ggplot background
#'
#' @return ggplot theme
my_theme <- function(){
    ggplot2::theme_bw() + ggplot2::theme(
                                       axis.text.x = ggplot2::element_text(size = 16, family = "Palatino"),
                                       axis.text.y= ggplot2::element_text(size = 16, family = "Palatino"),
                                       axis.title.x= ggplot2::element_text(size = 18, family = "Palatino"),
                                       axis.title.y= ggplot2::element_text(size = 18, family = "Palatino"),
                                       plot.title = ggplot2::element_text(size = 24, family = "Palatino"),
                                       legend.title = ggplot2::element_text(size = 20, family = "Palatino"),
                                       legend.text = ggplot2::element_text(family = "Palatino", size = 16),
                                       legend.key.size = ggplot2::unit(3, "line"),
                                       plot.subtitle = ggplot2::element_text(size=16, family = "Palatino")
                                   )
}


