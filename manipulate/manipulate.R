library(manipulate)

# slider
manipulate(
  plot(rnorm(x), rnorm(x)),
  x=slider(0, 1000, initial=1, step=100)
)

# picker
manipulate(
  barplot(as.matrix(longley[,factor]), 
          beside = TRUE, main = factor),
  factor = picker("GNP", "Unemployed", "Employed")
)

# checkbox
manipulate(
  boxplot(Freq ~ Class, data = Titanic, outline = outline),
  outline = checkbox(FALSE, "Show outliers")
)

# slider, picker, checkbox, and label
manipulate(
  plot(cars, xlim = c(x.min, x.max), type = type, 
       axes = axes, ann = label),
  x.min = slider(0,15),
  x.max = slider(15,30, initial = 25),
  type = picker("p", "l", "b", "c", "o", "h", "s", "S", "n"),
  axes = checkbox(TRUE, "Draw Axes"),
  label = checkbox(FALSE, "Draw Labels")
)
