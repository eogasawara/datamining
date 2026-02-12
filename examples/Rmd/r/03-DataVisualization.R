knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Instalacao (executar apenas uma vez)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("RColorBrewer")

library(daltoolbox)
library(ggplot2)
library(RColorBrewer)

head(iris, 3)

colors <- brewer.pal(4, "Set1")
font <- theme(text = element_text(size = 16))

data <- data.frame(
  x = iris$Sepal.Length,
  value = iris$Sepal.Width,
  variable = iris$Species
)
grf <- plot_scatter(data, label_x = "Sepal.Length", label_y = "Sepal.Width", colors = colors[1:3]) + font
plot(grf)

agg <- aggregation("Species", Sepal.Length = mean(Sepal.Length))
data <- transform(agg, iris)
grf <- plot_bar(data, colors = colors[1:3]) + font
plot(grf)

agg <- aggregation("Species", Sepal.Length = mean(Sepal.Length))
data <- transform(agg, iris)
grf <- plot_lollipop(data, colors = colors[1], max_value_gap = 0.2) + font + coord_flip()
plot(grf)

agg <- aggregation("Species", mean = mean(Sepal.Length), sd = sd(Sepal.Length))
data <- transform(agg, iris)
grf <- plot_bar(data, colors = colors[1], alpha = 1) + font
grf <- grf + geom_errorbar(data = data, inherit.aes = FALSE,
                           aes(x = Species, ymin = mean - sd, ymax = mean + sd),
                           width = 0.2, colour = "darkred", alpha = 0.8, size = 1.1)
plot(grf)

agg <- aggregation("Species", n = n())
data <- transform(agg, iris)
grf <- plot_pieplot(data, colors = colors[1:3]) + font
plot(grf)

agg <- aggregation("Species",
  Sepal.Length = mean(Sepal.Length),
  Sepal.Width = mean(Sepal.Width)
)
data <- transform(agg, iris)

grf <- plot_groupedbar(data, colors = colors[1:2]) + font
plot(grf)

agg <- aggregation("Species",
  Sepal.Length = mean(Sepal.Length),
  Sepal.Width = mean(Sepal.Width)
)
data <- transform(agg, iris)

grf <- plot_stackedbar(data, colors = colors[1:2]) + font
grf <- grf + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(grf)

x <- seq(0, 10, 0.25)
serie <- data.frame(x, y = sin(x))

grf <- plot_series(serie, colors = colors[1]) + font
plot(grf)

example <- data.frame(
  exponential = rexp(10000, rate = 1),
  uniform = runif(10000, min = 2.5, max = 3.5),
  normal = rnorm(10000, mean = 5)
)
head(example)

data <- data.frame(exponential = example$exponential)
grf <- plot_hist(data, label_x = "exponential", color = colors[1]) + font
plot(grf)

{
  library(gridExtra)
  grfe <- plot_hist(data.frame(exponential = example$exponential), label_x = "exponential", color = colors[1]) + font
  grfu <- plot_hist(data.frame(uniform = example$uniform), label_x = "uniform", color = colors[1]) + font
  grfn <- plot_hist(data.frame(normal = example$normal), label_x = "normal", color = colors[1]) + font
  grid.arrange(grfe, grfu, grfn, ncol = 3)
}

data <- data.frame(normal = example$normal)
grf <- plot_density(data, label_x = "normal", color = colors[1]) + font
plot(grf)

data <- data.frame(normal = example$normal)
grf <- plot_boxplot(data, colors = "white") + font
plot(grf)
