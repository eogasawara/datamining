knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Forma mais simples de usar clustering no daltoolbox: instalando pacotes basicos
#install.packages("daltoolbox")
#install.packages("ggplot2")
#install.packages("cluster")
#install.packages("mclust")
#install.packages("e1071")
#install.packages("igraph")

  library(daltoolbox)
  library(ggplot2)
  library(cluster)
  library(mclust)
  library(e1071)
  library(igraph)

# Slides 4: conjunto de dados de exemplo
iris <- datasets::iris
head(iris)

# Preparacao
X <- iris[, 1:4]

# Slides 25–29: K-means
model_km <- cluster_kmeans(k = 3)
model_km <- daltoolbox::fit(model_km, X)
clu_km <- daltoolbox::cluster(model_km, X)
table(clu_km)

eval_km <- daltoolbox::evaluate(model_km, clu_km, iris$Species)
eval_km

# Slides 29: normalizacao
iris_minmax <- transform(daltoolbox::fit(minmax(), iris), iris)
model_km <- daltoolbox::fit(model_km, iris_minmax[, 1:4])
clu_km <- daltoolbox::cluster(model_km, iris_minmax[, 1:4])
eval_km_norm <- daltoolbox::evaluate(model_km, clu_km, iris_minmax$Species)
eval_km_norm

# Slides 30–31: K-medoids (PAM)
model_pam <- cluster_pam(k = 3)
model_pam <- daltoolbox::fit(model_pam, X)
clu_pam <- daltoolbox::cluster(model_pam, X)
table(clu_pam)

eval_pam <- daltoolbox::evaluate(model_pam, clu_pam, iris$Species)
eval_pam

# Slides 32–35: clustering hierarquico e dendrograma (R base)
# Distancia euclidiana e metodo de Ward
model_hc <- cluster_hclust(k = 3, method = "ward.D2", dist = "euclidean", scale = TRUE)
model_hc <- daltoolbox::fit(model_hc, X)
plot(model_hc[["hc"]])

# Cortando em 3 grupos
hc_groups <- daltoolbox::cluster(model_hc, X)
table(hc_groups)

# Slides 36–41: DBSCAN
model_db <- cluster_dbscan(minPts = 3)
model_db <- daltoolbox::fit(model_db, X)
clu_db <- daltoolbox::cluster(model_db, X)
table(clu_db)

eval_db <- daltoolbox::evaluate(model_db, clu_db, iris$Species)
eval_db

# Slides 42–45: avaliacao e escolha de modelos
# Ajuste automatico de k para k-means
model_tune <- clu_tune(cluster_kmeans(k = 0), ranges = list(k = 2:10))
model_tune <- daltoolbox::fit(model_tune, X)
model_tune$k

clu_tune <- daltoolbox::cluster(model_tune, X)
eval_tune <- daltoolbox::evaluate(model_tune, clu_tune, iris$Species)
eval_tune

# Slides 47–56: clustering probabilistico (GMM via mclust)
{
set.seed(1)
model_gmm <- cluster_gmm()
model_gmm <- daltoolbox::fit(model_gmm, X)
clu_gmm <- daltoolbox::cluster(model_gmm, X)
}

# classificação
table(clu_gmm)

# log-likelihood (EM)
head(model_gmm$model$loglik)

# Slides 50–51: clustering fuzzy (cmeans)
set.seed(1)
model_fuzzy <- cluster_cmeans(centers = 3, m = 2)
model_fuzzy <- daltoolbox::fit(model_fuzzy, X)
clu_fuzzy <- daltoolbox::cluster(model_fuzzy, X)

table(clu_fuzzy)

# Slides 63–64: clustering de grafos e redes (exemplo simples)
set.seed(1)
g <- igraph::sample_gnp(n = 20, p = 0.15)
model_louvain <- cluster_louvain_graph()
model_louvain <- daltoolbox::fit(model_louvain, g)
comm <- daltoolbox::cluster(model_louvain, g)
length(unique(comm))

# Grupos de comunidade
comm
