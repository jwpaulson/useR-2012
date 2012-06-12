# Example Notebook

x <- rnorm(1000)
y <- rnorm(1000)

summary <- lm(y ~ x)
summary

library(ggplot2)
f2d <- with(faithful, MASS::kde2d(eruptions, waiting, h = c(1,10), n = 50))
df <- with(f2d, cbind(expand.grid(x, y), as.vector(z)))
names(df) <- c("eruptions", "waiting", "density")
erupt <- ggplot(df, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
erupt


suppressPackageStartupMessages(library(quantmod))
getSymbols("YHOO")
chartSeries(YHOO)
