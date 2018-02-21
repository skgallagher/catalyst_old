## SKG
## Plotting base probabilities from cataylst
## From SIR model with beta = .1, gamma = .03 N = 1000
## S0 = 950, I0 = 50, R0 = 0

library(reshape2)
library(ggplot2)

df <- read.csv("base_probs.csv", header = FALSE)
df <- df[, -ncol(df)]
colnames(df) <- c("Time", "State",
                  "Prob-to-S", "Prob-to-I", "Prob-to-R")
head(df)

df_melt <- melt(df, id.vars = c("Time", "State"))

df_melt$Compartment <- factor(df_melt$State, levels = 0:2, labels = c("S", "I", "R"))

ggplot(data = df_melt,
       aes(x = Time, y = value, group = Compartment)) +
    geom_line() + facet_wrap(variable~Compartment) + ylim(0, 1) +
    theme_bw() +
    labs(y = "Probability of Transition",
         title = "Probability of transitioning from one compartment to another",
         subtitle = "beta = .1, gamma = .03, N = 1000, S0 = 950, I0 = 50")
ggsave("../../plots/base_probs.pdf")
       

##############################
## Plot transitions over time
###############
