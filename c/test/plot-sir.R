## SKG
## Plotting SIR from C

df <- read.table("sir.txt")
colnames(df) <- c("t", "S", "I")
N <- df$S[1] + df$I[1]
df$R <- N - df$S - df$I
head(df)

plot(c(1,1), type = "n", ylim =c(0,1), xlim = range(df$t))
lines(df$t, df$S / N, lwd = 2, col = "blue")
lines(df$t, df$I / N, lwd = 2, col = "red")
lines(df$t, df$R / N, lwd = 2, col = "darkgreen")
