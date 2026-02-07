# Slides 1–3: panorama do pré-processamento
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Pacote '%s' não instalado. Instale com install.packages('%s').", pkg, pkg))
  }
  invisible(TRUE)
}

pkgs <- c("daltoolbox","dplyr","tidyr","ggplot2","caret","scales")
invisible(lapply(pkgs, require_pkg))

suppressPackageStartupMessages({
  library(daltoolbox)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(caret)
  library(scales)
})

# Slide 6: Como lidar com dados ausentes (remoção)
iris <- datasets::iris
iris.na <- iris
iris.na$Sepal.Length[2] <- NA
iris.na.omit <- na.omit(iris.na)
head(iris.na.omit)

# Slide 6: Imputação simples (média/mediana)
iris_na <- iris
iris_na$Sepal.Length[c(2, 10, 25)] <- NA
iris_imputed <- iris_na %>%
  mutate(
    Sepal.Length = if_else(
      is.na(Sepal.Length),
      median(Sepal.Length, na.rm = TRUE),
      Sepal.Length
    )
  )
summary(iris_imputed$Sepal.Length)

# Slide 10: Remoção de outliers (boxplot)
out_obj <- outliers_boxplot()
out_obj <- fit(out_obj, iris)
iris.clean <- transform(out_obj, iris)
head(iris.clean)

# Slide 8–10: Remoção de outliers (regra 3σ)
out_obj <- outliers_gaussian()
out_obj <- fit(out_obj, iris)
iris.clean <- transform(out_obj, iris)
head(iris.clean)

# Slide 8: Suavização por regressão (LOESS)
set.seed(123)
x <- seq(1, 100)
y <- sin(x / 10) + rnorm(100, sd = 0.2)
dat <- data.frame(x, y)
ggplot(dat, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Suavização LOESS", x = "x", y = "y")

# Slides 20–22: PCA
iris <- datasets::iris
mypca <- dt_pca("Species")
mypca <- fit(mypca, datasets::iris)
iris.pca <- transform(mypca, iris)
head(iris.pca)

# Slides 23–24: Seleção de atributos (correlação alta)
num_iris <- iris[, vapply(iris, is.numeric, logical(1)), drop = FALSE]
cor_mat <- cor(num_iris)
high_corr <- caret::findCorrelation(cor_mat, cutoff = 0.9)
colnames(num_iris)[high_corr]

# Slide 26: Geração de features
iris_feat <- iris %>%
  mutate(
    Sepal.Area = Sepal.Length * Sepal.Width,
    Petal.Area = Petal.Length * Petal.Width,
    Sepal.Ratio = Sepal.Length / Sepal.Width
  )
head(iris_feat)

# Slide 27: Agregação de dados
iris %>%
  group_by(Species) %>%
  summarise(
    mean_sepal = mean(Sepal.Length),
    sd_sepal = sd(Sepal.Length),
    n = n()
  )

# Slide 29: Normalização Min-Max
norm <- minmax()
norm <- fit(norm, iris)
ndata <- transform(norm, iris)
summary(ndata)

# Slide 29: Normalização Z-Score
norm <- zscore()
norm <- fit(norm, iris)
ndata <- transform(norm, iris)
summary(ndata)

# Slide 30: Comparação visual de normalização
vals <- data.frame(
  x = iris$Sepal.Length,
  minmax = scales::rescale(iris$Sepal.Length),
  zscore = as.numeric(scale(iris$Sepal.Length))
)

vals_long <- vals %>%
  pivot_longer(cols = c(x, minmax, zscore), names_to = "method", values_to = "value")

ggplot(vals_long, aes(value, fill = method)) +
  geom_histogram(bins = 20, alpha = 0.6, position = "identity") +
  facet_wrap(~ method, scales = "free") +
  labs(title = "Comparação visual de normalização")

# Slides 33–35: Binning por intervalos
obj <- smoothing_inter(n = 2)
obj <- fit(obj, iris$Sepal.Length)
sl.bi <- transform(obj, iris$Sepal.Length)
table(sl.bi)

# Slides 33–35: Binning por frequência (quantis)
obj <- smoothing_freq(n = 2)
obj <- fit(obj, iris$Sepal.Length)
sl.bi <- transform(obj, iris$Sepal.Length)
table(sl.bi)

# Slide 35: Discretização via clustering
obj <- smoothing_cluster(n = 2)
obj <- fit(obj, iris$Sepal.Length)
sl.bi <- transform(obj, iris$Sepal.Length)
table(sl.bi)

# Slides 38–39: Mapeamento categórico (one-hot)
cm <- categ_mapping("Species")
iris_cm <- transform(cm, iris)
head(iris_cm)

# Slides 36–37: Hierarquia de conceitos (exemplo simples)
iris %>%
  mutate(
    Sepal.Length.Level = cut(
      Sepal.Length,
      breaks = c(-Inf, 5.5, 6.5, Inf),
      labels = c("baixo", "medio", "alto")
    )
  ) %>%
  count(Sepal.Length.Level)

# Slides 40–41: Amostragem aleatória
tt <- train_test(sample_random(), iris)
table(tt$train$Species)

# Slides 41–43: Amostragem estratificada
tt <- train_test(sample_stratified("Species"), iris)
table(tt$train$Species)

# Slide 42: Amostragem com e sem reposição
set.seed(123)
srswor <- sample(iris$Sepal.Length, size = 10, replace = FALSE)
srswr <- sample(iris$Sepal.Length, size = 10, replace = TRUE)
srswor
srswr

# Slide 43: Amostragem por cluster (exemplo simples)
set.seed(123)
clusters <- split(iris, iris$Species)
sampled_clusters <- sample(names(clusters), 2)
cluster_sample <- bind_rows(clusters[sampled_clusters])
table(cluster_sample$Species)

# Slides 45–46: Balanceamento de classes (up/down sampling)
set.seed(123)
iris_imb <- iris %>% filter(Species != "setosa")
iris_imb$Species <- droplevels(iris_imb$Species)

# Garantir data.frame puro para compatibilidade com versões antigas
iris_x <- iris_imb[, setdiff(names(iris_imb), "Species"), drop = FALSE]
down <- downSample(x = iris_x, y = iris_imb$Species)
table(down$Class)

up <- upSample(x = iris_x, y = iris_imb$Species)
table(up$Class)
