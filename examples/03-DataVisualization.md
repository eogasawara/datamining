

# Visualizacao de Dados

## Visao Geral
Este roteiro operacionaliza os exemplos do arquivo `03-DataVisualization.pdf`, conectando cada trecho de codigo ao slide de origem.  
O objetivo e mostrar como a escolha do grafico muda a interpretacao dos dados e como o `daltoolbox` simplifica a interface com o `ggplot2` (Wickham, 2016).  
Slides: 1.

## Como ler este roteiro
Para cada grafico, avalie sempre:
1. qual pergunta o grafico responde;
2. qual variavel esta no eixo x/y (ou categoria);
3. qual a principal limitacao visual daquele tipo de representacao.

## Configuracao e Bibliotecas
Slides: 2-7.


``` r
# Instalacao (executar apenas uma vez)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("RColorBrewer")

library(daltoolbox)
library(ggplot2)
library(RColorBrewer)
```

Neste conjunto de exemplos, usamos o `iris` como base didatica para manter comparabilidade entre graficos.  
Slides: 7.


``` r
head(iris, 3)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
```

Configuramos paleta qualitativa do ColorBrewer para classes e um tema com fonte maior para leitura em aula.  
Slides: 5-6.


``` r
colors <- brewer.pal(4, "Set1")
font <- theme(text = element_text(size = 16))
```

## Grafico de Dispersao
Mostra relacao entre duas variaveis continuas e separacao por classe.  
Slides: 8.


``` r
data <- data.frame(
  x = iris$Sepal.Length,
  value = iris$Sepal.Width,
  variable = iris$Species
)
grf <- plot_scatter(data, label_x = "Sepal.Length", label_y = "Sepal.Width", colors = colors[1:3]) + font
plot(grf)
```

![plot of chunk s08_scatter](fig/03-DataVisualization/s08_scatter-1.png)

## Grafico de Barras
Resume uma estatistica por categoria; aqui usamos media por especie.  
Slides: 9.


``` r
agg <- aggregation("Species", Sepal.Length = mean(Sepal.Length))
data <- transform(agg, iris)
grf <- plot_bar(data, colors = colors[1:3]) + font
plot(grf)
```

![plot of chunk s09_bar](fig/03-DataVisualization/s09_bar-1.png)

## Lollipop
Alternativa ao grafico de barras, com menos area preenchida e foco na magnitude.  
Slides: 10.


``` r
agg <- aggregation("Species", Sepal.Length = mean(Sepal.Length))
data <- transform(agg, iris)
grf <- plot_lollipop(data, colors = colors[1], max_value_gap = 0.2) + font + coord_flip()
plot(grf)
```

![plot of chunk s10_lollipop](fig/03-DataVisualization/s10_lollipop-1.png)

## Barras com Erro
Acrescenta incerteza (desvio-padrao) ao valor medio, evitando leitura excessivamente deterministica.  
Slides: 11.


``` r
agg <- aggregation("Species", mean = mean(Sepal.Length), sd = sd(Sepal.Length))
data <- transform(agg, iris)
grf <- plot_bar(data, colors = colors[1], alpha = 1) + font
grf <- grf + geom_errorbar(data = data, inherit.aes = FALSE,
                           aes(x = Species, ymin = mean - sd, ymax = mean + sd),
                           width = 0.2, colour = "darkred", alpha = 0.8, size = 1.1)
plot(grf)
```

![plot of chunk s11_bar_error](fig/03-DataVisualization/s11_bar_error-1.png)

## Pizza
Util para composicao simples; para muitas categorias, barras costumam ser mais legiveis.  
Slides: 12.


``` r
agg <- aggregation("Species", n = n())
data <- transform(agg, iris)
grf <- plot_pieplot(data, colors = colors[1:3]) + font
plot(grf)
```

![plot of chunk s12_pie](fig/03-DataVisualization/s12_pie-1.png)

## Barras Agrupadas
Permite comparar mais de uma metrica para cada categoria.  
Slides: 13.


``` r
agg <- aggregation("Species",
  Sepal.Length = mean(Sepal.Length),
  Sepal.Width = mean(Sepal.Width)
)
data <- transform(agg, iris)

grf <- plot_groupedbar(data, colors = colors[1:2]) + font
plot(grf)
```

![plot of chunk s13_grouped_bar](fig/03-DataVisualization/s13_grouped_bar-1.png)

## Barras Empilhadas
Mostra total e composicao no mesmo eixo, com trade-off de precisao para comparar segmentos internos.  
Slides: 14.


``` r
agg <- aggregation("Species",
  Sepal.Length = mean(Sepal.Length),
  Sepal.Width = mean(Sepal.Width)
)
data <- transform(agg, iris)

grf <- plot_stackedbar(data, colors = colors[1:2]) + font
grf <- grf + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(grf)
```

![plot of chunk s14_stacked_bar](fig/03-DataVisualization/s14_stacked_bar-1.png)

## Grafico de Linhas
Ideal para series ordenadas no tempo ou em eixo continuo.  
Slides: 15.


``` r
x <- seq(0, 10, 0.25)
serie <- data.frame(x, y = sin(x))

grf <- plot_series(serie, colors = colors[1]) + font
plot(grf)
```

![plot of chunk s15_line](fig/03-DataVisualization/s15_line-1.png)

## Distribuicoes
Antes de modelar, avaliamos forma, dispersao e presenca de assimetria/outliers.  
Slides: 16-17.


``` r
example <- data.frame(
  exponential = rexp(10000, rate = 1),
  uniform = runif(10000, min = 2.5, max = 3.5),
  normal = rnorm(10000, mean = 5)
)
head(example)
```

```
##   exponential  uniform   normal
## 1   0.1096252 3.004124 5.119556
## 2   2.0550664 3.036043 6.725539
## 3   0.1786574 3.369387 4.396905
## 4   0.7781901 3.061361 6.201506
## 5   1.3307507 3.287657 6.187236
## 6   0.6654501 2.897380 6.626499
```

### Histograma
Mostra frequencias por intervalo e e sensivel ao numero/largura de bins.  
Slides: 18.


``` r
data <- data.frame(exponential = example$exponential)
grf <- plot_hist(data, label_x = "exponential", color = colors[1]) + font
plot(grf)
```

![plot of chunk s18_hist](fig/03-DataVisualization/s18_hist-1.png)

### Histogramas Multiplos
Comparacao lado a lado para identificar diferencas de forma entre distribuicoes.  
Slides: 19.


``` r
{
  library(gridExtra)
  grfe <- plot_hist(data.frame(exponential = example$exponential), label_x = "exponential", color = colors[1]) + font
  grfu <- plot_hist(data.frame(uniform = example$uniform), label_x = "uniform", color = colors[1]) + font
  grfn <- plot_hist(data.frame(normal = example$normal), label_x = "normal", color = colors[1]) + font
  grid.arrange(grfe, grfu, grfn, ncol = 3)
}
```

![plot of chunk s19_hist_multi](fig/03-DataVisualization/s19_hist_multi-1.png)

### Densidade
Estimativa suavizada da distribuicao; util para comparar formatos sem depender de bins.  
Slides: 20.


``` r
data <- data.frame(normal = example$normal)
grf <- plot_density(data, label_x = "normal", color = colors[1]) + font
plot(grf)
```

![plot of chunk s20_density](fig/03-DataVisualization/s20_density-1.png)

### Boxplot
Resumo robusto com mediana, quartis e possiveis outliers.  
Slides: 21.


``` r
data <- data.frame(normal = example$normal)
grf <- plot_boxplot(data, colors = "white") + font
plot(grf)
```

![plot of chunk s21_boxplot](fig/03-DataVisualization/s21_boxplot-1.png)

## Boas Praticas de Uso em Aula
Ao usar estes exemplos em slides e laboratorio, mantenha tres vinculos explicitos:  
1. problema analitico (o que quero mostrar);
2. tipo de variavel (continua, categorica, temporal);
3. grafico escolhido e suas limitacoes.  
Slides: 22-23.

## Referencias
Slides: 24.

- Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. Springer.
- Wickham, H., Cetinkaya-Rundel, M., & Grolemund, G. (2023). *R for Data Science* (2nd ed.). O'Reilly.
- Wilke, C. O. (2019). *Fundamentals of Data Visualization*. O'Reilly.
- Brewer, C. A. (n.d.). ColorBrewer 2.0. https://colorbrewer2.org


