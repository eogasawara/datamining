knitr::opts_chunk$set(message = FALSE, warning = FALSE)

if(!require(daltoolbox)) { 
  install.packages("daltoolbox")
}

library(daltoolbox)

weight <- 60
height = 1.75
subject <- "A"
healthy <- TRUE

weight

is.numeric(weight) # variable 
is.double(weight)
is.integer(weight)
is.character(subject)

weight <- as.integer(weight)
is.integer(weight)

#Body mass index (BMI)
bmi <- weight/height^2 
bmi 

message <- sprintf("%.1f", bmi)
print(message)

weight <- c(60, 72, 57, 90, 95, 72) 
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
subject <- c("A", "B", "C", "D", "E", "F")

weight
height
subject

vec <- rep(0, 10)
vec

length(weight)

weight[1]
weight[length(weight)]

bmi <- 0
for (i in 1:length(weight)) {
  bmi[i] <- weight[i]/height[i]^2
}

bmi

bmi <- 0
i <- 1
while (i <= length(weight)) {
  bmi[i] <- weight[i]/height[i]^2
  i <- i + 1
}

bmi

rm(bmi)
exists("bmi")

bmi <- weight/height^2 
bmi 

compute_bmi <- function(weight, height) {
  bmi <- weight/height^2 
  return(bmi)
}


bmi <- compute_bmi(60, 1.75)
bmi

bmi <- compute_bmi(weight, height)
bmi

average <- function(vec) {
    s <- 0
    n <- length(vec)
    for (x in vec) {
      s <- s + x  
    }
    return(s/n)
}

avg_bmi <- average(bmi)
avg_bmi

average <- function(vec) {
    s <- sum(vec)
    n <- length(vec)
    return(s/n)
}

avg_bmi <- average(bmi)
avg_bmi

average <- function(vec) {
    return(mean(vec))
}

avg_bmi <- average(bmi)
avg_bmi

x <- c(10, NA, 13)
y <- average(x)
y

average <- function(vec) {
    return(mean(vec, na.rm=TRUE))
}

x <- c(10, NA, 13)
y <- average(x)
y

weight <- c(60, 72, 57, 90, 95, 72) 
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2 

plot(height, weight)

plot(height, weight, pch=2)

plot(height, weight)
hh = c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2)

pain <- c(0,3,2,2,1)
fpain <- factor(pain,levels=0:3, ordered=TRUE)
fpain

levels(fpain) <- c("none","mild","medium","severe")
fpain

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

lev <- cut(height, breaks=c(0, 1.5, 1.9, .Machine$double.xmax), ordered=TRUE)
lev

levels(lev) <- c("short", "medium", "tall")
lev

x <- 1:9
x

dim(x) <- c(3,3)
x

y <- 3*x
y

x <- matrix(1:9,nrow=3,byrow=TRUE)
x

x <- t(x)
x

a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
b <- c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335)

mybag <- list(a, b, 0, "a")
mybag

n <- length(mybag)
mybag[[n+1]] <- "b"
mybag

slice <- mybag[1]
slice
is.list(slice)

h <- mybag[[1]]
h

is.vector(h)
is.list(h)

mybag <- list(x=a, y=b, const=0, lit="a")
mybag

mybag$c <- mybag$x - mybag$y
mybag$const <- NULL
mybag$lit <- NULL
mybag

a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
b <- c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335)

data <- data.frame(A=a, B=b)
head(data)

data$c <- data$A + data$B
head(data)

data$A <- NULL
head(data)

wine = read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                  header = TRUE, sep = ",")
  colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                      'Alcalinity', 'Magnesium', 'Phenols', 
                      'Flavanoids', 'Nonflavanoids',
                      'Proanthocyanins', 'Color', 'Hue', 
                      'Dilution', 'Proline')
head(wine)

save(wine, file="wine.RData")

rm(wine)

load("wine.RData")
head(wine, 3)

write.table(wine, file="wine.csv", row.names=FALSE, quote = FALSE, sep = ",")

a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)

# logical vector
bool <- (a > 7000)
bool

# selecting elements from positions that are true
a[bool] 

a[a > 7000] 


a <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
b <- c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335)
data <- data.frame(a = a, b = b)
head(data[data$a > 7000,])

head(data[data$a > 7000,c(1)])

rheight <- rnorm(100000, 1.8, sd=0.2)
rweight <- rnorm(100000, 72, sd=15)

start_time <- Sys.time()

hw <- data.frame(height=rheight, weight=rweight)
hw$bmi <- hw$weight/hw$height^2

end_time <- Sys.time()
end_time - start_time

object.size(hw)

start_time <- Sys.time()
hw <- data.frame(height=rheight, weight=rweight)
for (i in 1:nrow(hw)) {
  hw$bmi[i] <- hw$weight[i]/hw$height[i]^2
}
end_time <- Sys.time()
end_time - start_time

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

head(flight_data)

# dplyr nao e obrigatorio para os exemplos abaixo (base R)

result <- flight_data[flight_data$Delays > 5, c("Year", "Quarter", "Flights")]
head(result)

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

stores <- data.frame(
  city = c("Rio de Janeiro", "Sao Paulo", "Paris", "New York", "Tokyo"),
  value = c(10, 12, 20, 25, 18))
head(stores)

divisions <- data.frame(
  city = c("Rio de Janeiro", "Sao Paulo", "Paris", "New York", "Tokyo"),
  country = c("Brazil", "Brazil", "France", "US", "Japan"))
head(divisions)

stdiv <- merge(stores, divisions, by.x="city", by.y="city")
head(stdiv)

count_tbl <- aggregate(value ~ country, data = stdiv, FUN = length)
sum_tbl <- aggregate(value ~ country, data = stdiv, FUN = sum)
result <- merge(count_tbl, sum_tbl, by = "country")
colnames(result) <- c("country", "count", "amount")
head(result)

weight <- c(60, 72, 57, 90, 95, 72) 
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2 

t.test(bmi, mu=22.5)

library(reticulate)
source_python('https://raw.githubusercontent.com/eogasawara/datamining/refs/heads/main/python/retic.py')
x <- add(5, 10)
x

data <- data.frame(x = c(1:5), y=c(11:15))
dfm <- read_rdata_mem(data)
head(dfm)
