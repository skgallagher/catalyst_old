## SKG
## Plotting SIR from C

df <- read.table("sir-test-a.txt")
colnames(df) <- c("t", "S", "I", "R")
N <- df$S[1] + df$I[1]
head(df)

plot(c(1,1), type = "n", ylim =c(0,1), xlim = range(df$t))
lines(df$t, df$S / N, lwd = 2, col = "blue")
lines(df$t, df$I / N, lwd = 2, col = "red")
lines(df$t, df$R / N, lwd = 2, col = "darkgreen")
