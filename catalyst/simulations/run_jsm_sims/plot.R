## Some plotting fxns
## SKG
## July 26, 2018

library(dplyr)
library(ggplot2)
library(reshape2)
library(latex2exp)

plot_sir <- function(df, do_save = FALSE){
    cols <- c("blue", "darkred", "darkgreen")
    tex_symbol = c("\\hat{S}(t)", 
                   "\\hat{I}(t)",
                   "\\hat{R}(t)")
    legend_names_tex <- lapply(tex_symbol, function(x){
        latex2exp::TeX(sprintf("%s", x))
    })
    df_melt <- melt(df, id.vars = c("ll", "tt"))
    df_melt2 <- df_melt %>% dplyr::group_by(tt, variable) %>%
        dplyr::summarize(mu = mean(value),
                  upper = quantile(value, .95),
                  lower = quantile(value, .05))
#################################
    g <- ggplot(data = df_melt2, aes(x = tt)) + geom_line(aes(y = mu, col = variable)) + 
        labs(x = "Time", y = "# Individuals",
             title = paste0(type, " - Homogeneous Neighborhood"),
             subtitle = latex2exp::TeX(sprintf("%d individuals; %d runs; $\\beta = %.2f$; $\\gamma = %.2f$",
                                               N, L, disease_params_list$params[1],
                                               disease_params_list$params[2]))) +
        geom_ribbon(data = df_melt2, aes(ymin = lower, ymax = upper, fill = variable),
                    alpha = .4) + 
        ggplot2::scale_color_manual(values = cols,
                                    labels = legend_names_tex, name = "State") +
        ggplot2::scale_fill_manual(values = cols) + guides(fill = FALSE) + 
        my_theme()

    if(do_save)
        ggsave(g, paste0("plots/",
                         output_params_list$base_name,
                         "_", 
                         ".pdf"), width = 8, height = 6)

    

    return(g)
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


