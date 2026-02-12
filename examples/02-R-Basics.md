

# Fundamentos da Linguagem R

## Visão Geral
Este roteiro acompanha o conteúdo de `02-R-Basics.pdf` e foi organizado para leitura progressiva: tipos de dados, estruturas, manipulação tabular e integração com Python.  
Sugestão didática: execute os chunks em sequência e observe como os objetos são reaproveitados nas seções seguintes.  
Slides: 1-37.

## Como ler este roteiro
Siga a ordem dos blocos e, ao final de cada seção, revise:
1. quais objetos foram criados/alterados;
2. quais tipos de dados estão em uso;
3. como esse bloco prepara o próximo tópico.

## Instalação e Primeiros Objetos
Foco: instalar/carregar pacotes e consolidar operações básicas com variáveis numéricas e textuais.  
Slides: 5-8.

#### Instalacao e carregamento de pacotes R

Verificacao e instalacao de pacotes


``` r
if(!require(daltoolbox)) { 
  install.packages("daltoolbox")
}
```

Carregamento de pacotes


``` r
library(daltoolbox)
```

#### Definicao e avaliacao de variaveis

Definicao de variaveis


``` r
weight <- 60
height = 1.75
subject <- "A"
healthy <- TRUE
```

Avaliacao de variaveis


``` r
weight
```

```
## [1] 60
```

#### Funcoes para verificacao de tipo


``` r
is.numeric(weight) # variable 
```

```
## [1] TRUE
```

``` r
is.double(weight)
```

```
## [1] TRUE
```

``` r
is.integer(weight)
```

```
## [1] FALSE
```

``` r
is.character(subject)
```

```
## [1] TRUE
```

#### Funcoes para conversao de variaveis


``` r
weight <- as.integer(weight)
is.integer(weight)
```

```
## [1] TRUE
```

#### Formulas e formatacao de strings

Calculando o indice de massa corporal (IMC) a partir do peso e da altura


``` r
#Body mass index (BMI)
bmi <- weight/height^2 
bmi 
```

```
## [1] 19.59184
```

Funcoes para manipulacao de strings


``` r
message <- sprintf("%.1f", bmi)
print(message)
```

```
## [1] "19.6"
```

#### Vetores

definicao


``` r
weight <- c(60, 72, 57, 90, 95, 72) 
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
subject <- c("A", "B", "C", "D", "E", "F")
```

avaliacao


``` r
weight
```

```
## [1] 60 72 57 90 95 72
```

``` r
height
```

```
## [1] 1.75 1.80 1.65 1.90 1.74 1.91
```

``` r
subject
```

```
## [1] "A" "B" "C" "D" "E" "F"
```

#### Definindo o tamanho do vetor

vetor de tamanho 10


``` r
vec <- rep(0, 10)
vec
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0
```

tamanho do vetor


``` r
length(weight)
```

```
## [1] 6
```

#### Limites do vetor

da primeira ate a ultima posicao do vetor


``` r
weight[1]
```

```
## [1] 60
```

``` r
weight[length(weight)]
```

```
## [1] 72
```

#### Iteracao: laco for

da primeira ate a ultima posicao de weight


``` r
bmi <- 0
for (i in 1:length(weight)) {
  bmi[i] <- weight[i]/height[i]^2
}
```

avaliacao do vetor bmi


``` r
bmi
```

```
## [1] 19.59184 22.22222 20.93664 24.93075 31.37799 19.73630
```

#### Iteracao: laco while

executa enquanto i for menor ou igual ao tamanho de weight


``` r
bmi <- 0
i <- 1
while (i <= length(weight)) {
  bmi[i] <- weight[i]/height[i]^2
  i <- i + 1
}
```

Resultado do cálculo com `while`:


``` r
bmi
```

```
## [1] 19.59184 22.22222 20.93664 24.93075 31.37799 19.73630
```

Nesta etapa, compare a versão com `for` e a versão com `while`: o resultado é igual, mas a clareza do código pode mudar conforme o problema.  
Slides: 10-11.

#### Removendo uma variavel


``` r
rm(bmi)
exists("bmi")
```

```
## [1] FALSE
```

#### Processando vetores de uma vez



``` r
bmi <- weight/height^2 
bmi 
```

```
## [1] 19.59184 22.22222 20.93664 24.93075 31.37799 19.73630
```

#### Criando funcoes

name <- function(parameters) { body }


``` r
compute_bmi <- function(weight, height) {
  bmi <- weight/height^2 
  return(bmi)
}
```

chamando a funcao


``` r
bmi <- compute_bmi(60, 1.75)
bmi
```

```
## [1] 19.59184
```

#### Funcao processando vetores


``` r
bmi <- compute_bmi(weight, height)
bmi
```

```
## [1] 19.59184 22.22222 20.93664 24.93075 31.37799 19.73630
```

#### Exemplo: funcao para calcular a media

(iterando em todos os elementos do vetor)


``` r
average <- function(vec) {
    s <- 0
    n <- length(vec)
    for (x in vec) {
      s <- s + x  
    }
    return(s/n)
}
```

invocando a funcao


``` r
avg_bmi <- average(bmi)
avg_bmi
```

```
## [1] 23.13262
```

#### Exemplo de funcao para calcular a media (versao melhorada)

manipulando vetores de uma vez


``` r
average <- function(vec) {
    s <- sum(vec)
    n <- length(vec)
    return(s/n)
}
```

invocando a funcao


``` r
avg_bmi <- average(bmi)
avg_bmi
```

```
## [1] 23.13262
```

#### Exemplo: funcao usando mean

As principais funcoes estatisticas estao disponiveis no R


``` r
average <- function(vec) {
    return(mean(vec))
}
```

invocando a funcao


``` r
avg_bmi <- average(bmi)
avg_bmi
```

```
## [1] 23.13262
```

#### Valores ausentes (NA)

Operacoes com NA resultam em NA.


``` r
x <- c(10, NA, 13)
y <- average(x)
y
```

```
## [1] NA
```

Tratando NA com na.rm=TRUE


``` r
average <- function(vec) {
    return(mean(vec, na.rm=TRUE))
}
```

Teste rápido com vetor contendo NA:


``` r
x <- c(10, NA, 13)
y <- average(x)
y
```

```
## [1] 11.5
```

## Visualização e Tipos Categóricos
Foco: compreender quando usar gráficos básicos, fatores ordenados e discretização com `cut()`.  
Slides: 13-16.

#### Graficos basicos

definindo variaveis


``` r
weight <- c(60, 72, 57, 90, 95, 72) 
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2 
```

graficos de dispersao


``` r
plot(height, weight)
```

![plot of chunk unnamed-chunk-33](fig/02-R-Basics/unnamed-chunk-33-1.png)

#### A maioria das funcoes possui muitos parametros padrao


``` r
plot(height, weight, pch=2)
```

![plot of chunk unnamed-chunk-34](fig/02-R-Basics/unnamed-chunk-34-1.png)

#### A janela de plotagem permanece ativa ate um novo grafico


``` r
plot(height, weight)
hh = c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2)
```

![plot of chunk unnamed-chunk-35](fig/02-R-Basics/unnamed-chunk-35-1.png)

#### Fatores

Fatores sao usados para representar dados categoricos.


``` r
pain <- c(0,3,2,2,1)
fpain <- factor(pain,levels=0:3, ordered=TRUE)
fpain
```

```
## [1] 0 3 2 2 1
## Levels: 0 < 1 < 2 < 3
```

Os niveis fazem a correspondencia entre valores numericos e rotulos categoricos


``` r
levels(fpain) <- c("none","mild","medium","severe")
fpain
```

```
## [1] none   severe medium medium mild  
## Levels: none < mild < medium < severe
```

#### Converter height em fator com niveis: pequeno, medio e alto

definindo elemento por elemento


``` r
lev <- rep("", length(height))

for (i in 1:length(height)) {
  if (height[i] < 1.5)
    lev[i] <- "short"
  else if (height[i] < 1.9)
    lev[i] <- "medium"
  else 
    lev[i] <- "tall"
}

lev <- factor(lev, levels=c("small", "medium", "tall"), ordered=TRUE)
lev
```

```
## [1] medium medium medium tall   medium tall  
## Levels: small < medium < tall
```

#### Usando a funcao cut


``` r
lev <- cut(height, breaks=c(0, 1.5, 1.9, .Machine$double.xmax), ordered=TRUE)
lev
```

```
## [1] (1.5,1.9]      (1.5,1.9]      (1.5,1.9]      (1.5,1.9]      (1.5,1.9]      (1.9,1.8e+308]
## Levels: (0,1.5] < (1.5,1.9] < (1.9,1.8e+308]
```

``` r
levels(lev) <- c("short", "medium", "tall")
lev
```

```
## [1] medium medium medium medium medium tall  
## Levels: short < medium < tall
```

## Estruturas de Dados em R
Foco: diferenças entre matriz, lista e data frame, e quando cada estrutura é mais adequada.  
Slides: 17-24.

#### Matrizes

Matrizes podem ser preenchidas a partir de vetores ou data frames. 


``` r
x <- 1:9
x
```

```
## [1] 1 2 3 4 5 6 7 8 9
```

Convertendo um vetor em matriz


``` r
dim(x) <- c(3,3)
x
```

```
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
```

#### Multiplicacao escalar


``` r
y <- 3*x
y
```

```
##      [,1] [,2] [,3]
## [1,]    3   12   21
## [2,]    6   15   24
## [3,]    9   18   27
```

#### Matrizes manipulation

Convertendo um vetor em matriz por linha


``` r
x <- matrix(1:9,nrow=3,byrow=TRUE)
x
```

```
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    4    5    6
## [3,]    7    8    9
```

transpondo uma matriz


``` r
x <- t(x)
x
```

```
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
```

#### Listas

Listas sao usadas para trabalhar com "objetos"


``` r
a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
b <- c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335)

mybag <- list(a, b, 0, "a")
mybag
```

```
## [[1]]
##  [1] 5260 5470 5640 6180 6390 6515 6805 7515 7515 8230 8770
## 
## [[2]]
##  [1] 3910 4220 3885 5160 5645 4680 5265 5975 6790 6900 7335
## 
## [[3]]
## [1] 0
## 
## [[4]]
## [1] "a"
```

#### Adicionando elementos em uma lista


``` r
n <- length(mybag)
mybag[[n+1]] <- "b"
mybag
```

```
## [[1]]
##  [1] 5260 5470 5640 6180 6390 6515 6805 7515 7515 8230 8770
## 
## [[2]]
##  [1] 3910 4220 3885 5160 5645 4680 5265 5975 6790 6900 7335
## 
## [[3]]
## [1] 0
## 
## [[4]]
## [1] "a"
## 
## [[5]]
## [1] "b"
```

#### Fatiamento de lista


``` r
slice <- mybag[1]
slice
```

```
## [[1]]
##  [1] 5260 5470 5640 6180 6390 6515 6805 7515 7515 8230 8770
```

``` r
is.list(slice)
```

```
## [1] TRUE
```

#### Listas: member reference

Selecionando o primeiro elemento


``` r
h <- mybag[[1]]
h
```

```
##  [1] 5260 5470 5640 6180 6390 6515 6805 7515 7515 8230 8770
```

Um elemento pode ser avaliado. Neste caso, e um vetor.


``` r
is.vector(h)
```

```
## [1] TRUE
```

``` r
is.list(h)
```

```
## [1] FALSE
```

#### Criando listas com atributos

Sao propriedades da lista


``` r
mybag <- list(x=a, y=b, const=0, lit="a")
mybag
```

```
## $x
##  [1] 5260 5470 5640 6180 6390 6515 6805 7515 7515 8230 8770
## 
## $y
##  [1] 3910 4220 3885 5160 5645 4680 5265 5975 6790 6900 7335
## 
## $const
## [1] 0
## 
## $lit
## [1] "a"
```

#### Adicionando, acessando e removendo elementos


``` r
mybag$c <- mybag$x - mybag$y
mybag$const <- NULL
mybag$lit <- NULL
mybag
```

```
## $x
##  [1] 5260 5470 5640 6180 6390 6515 6805 7515 7515 8230 8770
## 
## $y
##  [1] 3910 4220 3885 5160 5645 4680 5265 5975 6790 6900 7335
## 
## $c
##  [1] 1350 1250 1755 1020  745 1835 1540 1540  725 1330 1435
```

#### Data frames (quadros de dados)
Data frames (tabelas) oferecem suporte a dados estruturados. 


``` r
a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
b <- c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335)
```

Montamos o `data.frame` base para as operações de coluna:


``` r
data <- data.frame(A=a, B=b)
head(data)
```

```
##      A    B
## 1 5260 3910
## 2 5470 4220
## 3 5640 3885
## 4 6180 5160
## 5 6390 5645
## 6 6515 4680
```

#### Adicionando uma coluna em um data frame 


``` r
data$c <- data$A + data$B
head(data)
```

```
##      A    B     c
## 1 5260 3910  9170
## 2 5470 4220  9690
## 3 5640 3885  9525
## 4 6180 5160 11340
## 5 6390 5645 12035
## 6 6515 4680 11195
```

#### Removendo uma coluna de um data frame 


``` r
data$A <- NULL
head(data)
```

```
##      B     c
## 1 3910  9170
## 2 4220  9690
## 3 3885  9525
## 4 5160 11340
## 5 5645 12035
## 6 4680 11195
```

## Leitura, Escrita e Filtros
Foco: entrada/saída de dados e seleção de subconjuntos de forma reprodutível.  
Slides: 25-27.

#### Leitura de arquivo CSV
Ha diversas funcoes para ler formatos CSV, Excel e RData.


``` r
wine = read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                  header = TRUE, sep = ",")
  colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                      'Alcalinity', 'Magnesium', 'Phenols', 
                      'Flavanoids', 'Nonflavanoids',
                      'Proanthocyanins', 'Color', 'Hue', 
                      'Dilution', 'Proline')
head(wine)
```

```
##   Type Alcohol Malic  Ash Alcalinity Magnesium Phenols Flavanoids Nonflavanoids Proanthocyanins Color  Hue Dilution Proline
## 1    1   13.20  1.78 2.14       11.2       100    2.65       2.76          0.26            1.28  4.38 1.05     3.40    1050
## 2    1   13.16  2.36 2.67       18.6       101    2.80       3.24          0.30            2.81  5.68 1.03     3.17    1185
## 3    1   14.37  1.95 2.50       16.8       113    3.85       3.49          0.24            2.18  7.80 0.86     3.45    1480
## 4    1   13.24  2.59 2.87       21.0       118    2.80       2.69          0.39            1.82  4.32 1.04     2.93     735
## 5    1   14.20  1.76 2.45       15.2       112    3.27       3.39          0.34            1.97  6.75 1.05     2.85    1450
## 6    1   14.39  1.87 2.45       14.6        96    2.50       2.52          0.30            1.98  5.25 1.02     3.58    1290
```

#### Salvando em formato binario

salvando um data frame



``` r
save(wine, file="wine.RData")
```

removendo um data frame da memoria


``` r
rm(wine)
```

carregando novamente


``` r
load("wine.RData")
head(wine, 3)
```

```
##   Type Alcohol Malic  Ash Alcalinity Magnesium Phenols Flavanoids Nonflavanoids Proanthocyanins Color  Hue Dilution Proline
## 1    1   13.20  1.78 2.14       11.2       100    2.65       2.76          0.26            1.28  4.38 1.05     3.40    1050
## 2    1   13.16  2.36 2.67       18.6       101    2.80       3.24          0.30            2.81  5.68 1.03     3.17    1185
## 3    1   14.37  1.95 2.50       16.8       113    3.85       3.49          0.24            2.18  7.80 0.86     3.45    1480
```

#### Exportando data.frame para arquivo CSV


``` r
write.table(wine, file="wine.csv", row.names=FALSE, quote = FALSE, sep = ",")
```

#### Filtrando vetores


``` r
a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)

# vetor logico
bool <- (a > 7000)
bool
```

```
##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
```

Aplicamos o filtro lógico em duas formas equivalentes:


``` r
# selecionando elementos nas posicoes verdadeiras
a[bool] 
```

```
## [1] 7515 7515 8230 8770
```

``` r
a[a > 7000] 
```

```
## [1] 7515 7515 8230 8770
```
#### Filtrando data frames


``` r
a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
b <- c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335)
data <- data.frame(a = a, b = b)
head(data[data$a > 7000,])
```

```
##       a    b
## 8  7515 5975
## 9  7515 6790
## 10 8230 6900
## 11 8770 7335
```

#### Filtrando data frames (linhas e colunas)


``` r
head(data[data$a > 7000,c(1)])
```

```
## [1] 7515 7515 8230 8770
```

#### Comparando desempenho entre matriz e data frame


``` r
rheight <- rnorm(100000, 1.8, sd=0.2)
rweight <- rnorm(100000, 72, sd=15)
```

#### Calculando uma coluna de uma vez no data frame


``` r
start_time <- Sys.time()

hw <- data.frame(height=rheight, weight=rweight)
hw$bmi <- hw$weight/hw$height^2

end_time <- Sys.time()
end_time - start_time
```

```
## Time difference of 0.002305984 secs
```

``` r
object.size(hw)
```

```
## 2400984 bytes
```

#### Calculando uma coluna no data frame celula a celula


``` r
start_time <- Sys.time()
hw <- data.frame(height=rheight, weight=rweight)
for (i in 1:nrow(hw)) {
  hw$bmi[i] <- hw$weight[i]/hw$height[i]^2
}
end_time <- Sys.time()
end_time - start_time
```

```
## Time difference of 12.70396 secs
```

#### Convertendo data frame para matriz, processando e retornando


``` r
start_time <- Sys.time()
hw <- data.frame(height=rheight, weight=rweight)

hw <- as.matrix(hw)

hw <- cbind(hw, 0)
for (i in 1:nrow(hw)) {
  hw[i,3] <- hw[i,2]/hw[i,1]^2
}

hw <- as.data.frame(hw)

end_time <- Sys.time()
end_time - start_time
```

```
## Time difference of 0.262686 secs
```

## Pipelines e Consultas em Tabelas
Foco: operações de consulta, agregação e junção para análise tabular.  
Slides: 28-34.

#### Pipelines
O operador |> cria um pipeline. 

O primeiro parametro da proxima funcao recebe os dados do pipeline. 

A biblioteca $dplyr$ contem funcoes que apoiam operacoes de algebra relacional.


``` r
flight_data <- read.table(text = "Year Quarter Flights Delays
                     2016 1 11 6
                     2016 2 12 5
                     2016 3 13 3
                     2016 4 12 5
                     2017 1 10 4
                     2017 2 9 3
                     2017 3 11 4
                     2017 4 25 15
                     2018 1 14 3
                     2018 2 12 5
                     2018 3 13 3
                     2018 4 15 4",
                     header = TRUE,sep = "")  
```

#### Exibindo o data frame


``` r
head(flight_data)
```

```
##   Year Quarter Flights Delays
## 1 2016       1      11      6
## 2 2016       2      12      5
## 3 2016       3      13      3
## 4 2016       4      12      5
## 5 2017       1      10      4
## 6 2017       2       9      3
```

#### Consulta basica



``` r
# dplyr nao e obrigatorio para os exemplos abaixo (base R)
```

Consulta base para selecionar voos com maior atraso:


``` r
result <- flight_data[flight_data$Delays > 5, c("Year", "Quarter", "Flights")]
head(result)
```

```
##   Year Quarter Flights
## 1 2016       1      11
## 8 2017       4      25
```

#### Consulta agregada



``` r
agg <- aggregate(
  Flights ~ Year,
  data = flight_data,
  FUN = function(x) c(mean = mean(x), sd = sd(x))
)
result <- data.frame(
  Year = agg$Year,
  mean = agg$Flights[, "mean"],
  sd = agg$Flights[, "sd"]
)
head(result)
```

```
##   Year  mean        sd
## 1 2016 12.00 0.8164966
## 2 2017 13.75 7.5443135
## 3 2018 13.50 1.2909944
```

#### Juncao de tabelas

Tabela de lojas


``` r
stores <- data.frame(
  city = c("Rio de Janeiro", "Sao Paulo", "Paris", "New York", "Tokyo"),
  value = c(10, 12, 20, 25, 18))
head(stores)
```

```
##             city value
## 1 Rio de Janeiro    10
## 2      Sao Paulo    12
## 3          Paris    20
## 4       New York    25
## 5          Tokyo    18
```

Tabela de divisoes


``` r
divisions <- data.frame(
  city = c("Rio de Janeiro", "Sao Paulo", "Paris", "New York", "Tokyo"),
  country = c("Brazil", "Brazil", "France", "US", "Japan"))
head(divisions)
```

```
##             city country
## 1 Rio de Janeiro  Brazil
## 2      Sao Paulo  Brazil
## 3          Paris  France
## 4       New York      US
## 5          Tokyo   Japan
```

#### Funcao merge

A funcao $merge$ pode ser usada para juntar data frames. Ela permite juncoes internas, esquerda, direita e externas. 


``` r
stdiv <- merge(stores, divisions, by.x="city", by.y="city")
head(stdiv)
```

```
##             city value country
## 1       New York    25      US
## 2          Paris    20  France
## 3 Rio de Janeiro    10  Brazil
## 4      Sao Paulo    12  Brazil
## 5          Tokyo    18   Japan
```
#### Agregando data frame mesclado


``` r
count_tbl <- aggregate(value ~ country, data = stdiv, FUN = length)
sum_tbl <- aggregate(value ~ country, data = stdiv, FUN = sum)
result <- merge(count_tbl, sum_tbl, by = "country")
colnames(result) <- c("country", "count", "amount")
head(result)
```

```
##   country count amount
## 1  Brazil     2     22
## 2  France     1     20
## 3   Japan     1     18
## 4      US     1     25
```

#### Analise estatistica
Ha muitos testes estatisticos no R.
Um dos mais usados e o teste t. Ele verifica se a media observada difere de um valor teorico.


``` r
weight <- c(60, 72, 57, 90, 95, 72) 
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2 
```

Executamos o teste t para comparar a média observada de IMC com o valor teórico `22.5`:


``` r
t.test(bmi, mu=22.5)
```

```
## 
## 	One Sample t-test
## 
## data:  bmi
## t = 0.34488, df = 5, p-value = 0.7442
## alternative hypothesis: true mean is not equal to 22.5
## 95 percent confidence interval:
##  18.41734 27.84791
## sample estimates:
## mean of x 
##  23.13262
```

## Integração R + Python
Foco: reaproveitar funções Python dentro de um fluxo R via `reticulate`.  
Slides: 36.

#### Integracao Python + R


Codigo Python em retic.py
```text
import pyreadr
import pandas

def add(x, y):
  return x + y

def read_rdata_mem(data):
  x = data["x"]
  print(x)
  y = data["y"]
  data["z"] = x + y
  return(data)
```

A biblioteca reticulate permite integracao direta com Python

``` r
library(reticulate)
source_python('https://raw.githubusercontent.com/eogasawara/datamining/refs/heads/main/python/retic.py')
x <- add(5, 10)
x
```

```
## [1] 15
```


#### Integracao de dados entre Python + R


``` r
data <- data.frame(x = c(1:5), y=c(11:15))
dfm <- read_rdata_mem(data)
```

```
## 0    1
## 1    2
## 2    3
## 3    4
## 4    5
## Name: x, dtype: int32
```

``` r
head(dfm)
```

```
##   x  y  z
## 1 1 11 12
## 2 2 12 14
## 3 3 13 16
## 4 4 14 18
## 5 5 15 20
```

## Referências
Slides: 37.

- Wickham, H., Cetinkaya-Rundel, M., & Grolemund, G. (2023). *R for Data Science* (2nd ed.). O'Reilly.
- Chambers, J. M. (2016). *Extending R*. Chapman and Hall/CRC.
- Ushey, K., Allaire, J., & Tang, Y. (2024). *reticulate: Interface to Python* (R package).


