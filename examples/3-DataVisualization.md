#### Plotting charts


``` r
# The easiest way to get ggplot2 is to install the whole tidyverse
#install.packages("tidyverse")
#install.packages("ggplot2")
```


#### ggplot easy encapsulation through daltoolbox


``` r
library(daltoolbox)
library(ggplot2)
```

#### Color Brewer R Package


``` r
#install.packages("RColorBrewer")
library(RColorBrewer)
colors <- brewer.pal(4, 'Set1')
```


#### Basic setup for examples

Iris datasets


``` r
head(iris, 3)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
```

Options from graphics: colors and font size


``` r
colors <- brewer.pal(4, 'Set1')
font <- theme(text = element_text(size=16))
```

#### Scatter plot



``` r
library(dplyr)
data <- iris |> select(x = Sepal.Length, value = Sepal.Width, variable = Species)
#head(data)
grf <- plot_scatter(data, label_x = "Sepal.Length", label_y = "Sepal.Width", colors=colors[1:3]) + font
plot(grf)
```

![plot of chunk unnamed-chunk-6](fig/3-DataVisualization/unnamed-chunk-6-1.png)

#### Bar plot



``` r
library(dplyr)
data <- iris |> group_by(Species) |> summarize(Sepal.Length=mean(Sepal.Length))
#head(data)
grf <- plot_bar(data, colors=colors[1:3]) + font
plot(grf)
```

![plot of chunk unnamed-chunk-7](fig/3-DataVisualization/unnamed-chunk-7-1.png)


#### Lollipop plot



``` r
library(dplyr)
data <- iris |> group_by(Species) |> summarize(Sepal.Length=mean(Sepal.Length))
#head(data)
grf <- plot_lollipop(data, colors=colors[1], max_value_gap=0.2) + font + coord_flip() 
plot(grf)
```

![plot of chunk unnamed-chunk-8](fig/3-DataVisualization/unnamed-chunk-8-1.png)

#### Bar graph with error



``` r
library(dplyr)
data <- iris |> group_by(Species) |> summarize(mean=mean(Sepal.Length), sd=sd(Sepal.Length))
#head(data)
grf <- plot_bar(data, colors=colors[1], alpha=1) + font
grf <- grf + geom_errorbar(aes(x=Species, ymin=mean-sd, ymax=mean+sd), 
                           width=0.2, colour="darkred", alpha=0.8, size=1.1) 
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
```

``` r
plot(grf)
```

![plot of chunk unnamed-chunk-9](fig/3-DataVisualization/unnamed-chunk-9-1.png)



#### Pie plot



``` r
library(dplyr)
data <- iris |> group_by(Species) |> summarize(n = n())
#head(data)
grf <- plot_pieplot(data, colors=colors[1:3]) + font
plot(grf)
```

![plot of chunk unnamed-chunk-10](fig/3-DataVisualization/unnamed-chunk-10-1.png)


#### Grouped bar



``` r
library(dplyr)
data <- iris |> group_by(Species) |> summarize(Sepal.Length=mean(Sepal.Length), Sepal.Width=mean(Sepal.Width))

#head(data)
grf <- plot_groupedbar(data, colors=colors[1:2]) + font
plot(grf)
```

![plot of chunk unnamed-chunk-11](fig/3-DataVisualization/unnamed-chunk-11-1.png)


#### Stacked-bar



``` r
library(dplyr)
data <- iris |> group_by(Species) |> summarize(Sepal.Length=mean(Sepal.Length), Sepal.Width=mean(Sepal.Width))

#head(data)
grf <- plot_stackedbar(data, colors=colors[1:2]) + font
grf <- grf + theme(axis.text.x = element_text(angle=90, hjust=1))
plot(grf)
```

![plot of chunk unnamed-chunk-12](fig/3-DataVisualization/unnamed-chunk-12-1.png)

#### Line chart



``` r
x <- seq(0, 10, 0.25)
serie <- data.frame(x, y=sin(x))

#head(serie)
grf <- plot_series(serie, colors=colors[1]) + font
plot(grf)
```

![plot of chunk unnamed-chunk-13](fig/3-DataVisualization/unnamed-chunk-13-1.png)

#### Data distribution example


``` r
example <- data.frame(exponential = rexp(10000, rate = 1),
                      uniform = runif(10000, min=2.5, max = 3.5),
                      normal = rnorm(10000, mean = 5))
head(example)
```

```
##   exponential  uniform   normal
## 1  0.12260385 2.586433 5.331589
## 2  0.03942747 3.334119 4.388309
## 3  0.31239736 2.604191 6.273726
## 4  2.02476689 2.909577 5.625941
## 5  0.49887937 3.172632 4.818255
## 6  0.84701798 2.694884 5.021169
```




#### Histogram


``` r
library(dplyr)
data <- example |> select(exponential)
#head(data)
grf <- plot_hist(data, label_x = "exponential", color=colors[1]) + font
```

```
## Using  as id variables
```

``` r
plot(grf) 
```

![plot of chunk unnamed-chunk-16](fig/3-DataVisualization/unnamed-chunk-16-1.png)


#### Multiple Histograms




``` r
{
library(gridExtra)  
grfe <- plot_hist(example |> select(exponential), label_x = "exponential", color=colors[1]) + font
grfu <- plot_hist(example |> select(uniform), label_x = "uniform", color=colors[1]) + font 
grfn <- plot_hist(example |> select(normal), label_x = "normal", color=colors[1]) + font
grid.arrange(grfe, grfu, grfn, ncol=3)
}
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## Using  as id variables
## Using  as id variables
## Using  as id variables
```

![plot of chunk unnamed-chunk-17](fig/3-DataVisualization/unnamed-chunk-17-1.png)

#### Density plot



``` r
library(dplyr)
data <- example |> select(normal)
#head(data)
grf <- plot_density(data, label_x = "normal", color=colors[1]) + font
```

```
## Using  as id variables
```

``` r
plot(grf) 
```

![plot of chunk unnamed-chunk-18](fig/3-DataVisualization/unnamed-chunk-18-1.png)

#### Box plot



``` r
library(dplyr)
data <- example |> select(normal)
#head(data)
grf <- plot_boxplot(data, color="white") + font
```

```
## Using  as id variables
```

``` r
plot(grf) 
```

![plot of chunk unnamed-chunk-19](fig/3-DataVisualization/unnamed-chunk-19-1.png)

