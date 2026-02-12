knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Slides 1–3: contexto e definição
library(daltoolbox)

# Slides 9: conjunto de dados de exemplo
iris <- datasets::iris
head(iris)
table(iris$Species)

# Preparacao treino/teste (usado nos exemplos)
set.seed(1)
split_random <- sample_random()
split_random <- train_test(split_random, iris)
iris_train <- split_random$train
iris_test <- split_random$test

slevels <- levels(iris$Species)

# Distribuicao das classes
class_tbl <- rbind(
  table(iris[, "Species"]),
  table(iris_train[, "Species"]),
  table(iris_test[, "Species"])
)
rownames(class_tbl) <- c("dados", "treino", "teste")
class_tbl

# Helper: avaliacao padrao DALToolbox
# Slides 21–25: avaliacao

eval_model <- function(model, train, test, target_col) {
  evaluate_safe <- function(data, prediction, target_col) {
    # Padroniza o alvo real no formato esperado pelo avaliador
    predictand <- adjust_class_label(data[, target_col])
    eval <- evaluate(model, predictand, prediction)

    # Caminho alternativo para modelos cuja saida nao e aceita diretamente por evaluate()
    if (is.null(eval) || is.null(eval$metrics)) {
      proxy <- classification(target_col, colnames(predictand))

      # Caso 1: saida em rótulos (vetor/fator)
      if (is.factor(prediction) || is.character(prediction) || is.vector(prediction)) {
        pred <- factor(as.vector(prediction), levels = colnames(predictand))
        prediction <- adjust_class_label(pred)
      } else {
      # Caso 2: saida matricial (probabilidades ou escores)
        prediction <- as.matrix(prediction)
        if (is.null(colnames(prediction))) {
          colnames(prediction) <- colnames(predictand)[seq_len(ncol(prediction))]
        }
        # Alinha colunas previstas com as colunas reais para evitar desalinhamento de classes
        aligned <- matrix(0, nrow(prediction), ncol(predictand))
        colnames(aligned) <- colnames(predictand)
        common <- intersect(colnames(prediction), colnames(predictand))
        aligned[, common] <- prediction[, common, drop = FALSE]
        prediction <- aligned
      }

      eval <- evaluate(proxy, predictand, prediction)
    }

    list(eval = eval, predictand = predictand)
  }

  train_prediction <- predict(model, train)
  train_res <- evaluate_safe(train, train_prediction, target_col)
  print(train_res$eval$metrics)

  test_prediction <- predict(model, test)
  test_res <- evaluate_safe(test, test_prediction, target_col)
  print(test_res$eval$metrics)

  list(
    train_prediction = train_prediction,
    train_predictand = train_res$predictand,
    test_prediction = test_prediction,
    test_predictand = test_res$predictand
  )
}

pred_to_label <- function(pred) {
  # Converte probabilidades/scores em classe vencedora (argmax por linha)
  if (is.data.frame(pred) || is.matrix(pred)) {
    if (ncol(pred) == 1) {
      return(as.vector(pred[, 1]))
    }
    cols <- colnames(pred)
    if (is.null(cols)) cols <- seq_len(ncol(pred))
    return(apply(pred, 1, function(r) cols[which.max(r)]))
  }
  if (is.list(pred)) {
    return(unlist(pred))
  }
  as.vector(pred)
}

# Slides 10: Regra Zero (baseline)
model_majority <- cla_majority("Species", slevels)
model_majority <- fit(model_majority, iris_train)
res_majority <- eval_model(model_majority, iris_train, iris_test, "Species")

# Slides 11–20: Arvore de Decisao
model_dtree <- cla_dtree("Species", slevels)
model_dtree <- fit(model_dtree, iris_train)
res_dtree <- eval_model(model_dtree, iris_train, iris_test, "Species")

# Slides 21–25: avaliacao e matriz de confusao (exemplo com arvore)
# predict() pode retornar data.frame; usar vetor para a tabela
conf_dtree <- table(
  Pred = pred_to_label(res_dtree$test_prediction),
  Actual = pred_to_label(res_dtree$test_predictand)
)
conf_dtree

# Slides 33–40: Naive Bayes
model_nb <- cla_nb("Species", slevels)
model_nb <- fit(model_nb, iris_train)
res_nb <- eval_model(model_nb, iris_train, iris_test, "Species")

# Slides 41–47: Regressao Logistica (versicolor vs nao-versicolor)
# Problema binario
fg_bin <- feature_generation(
  IsVersicolor = ifelse(Species == "versicolor", "versicolor", "not_versicolor")
)
iris_bin_train <- transform(fg_bin, iris_train)
iris_bin_test <- transform(fg_bin, iris_test)
iris_bin_train$IsVersicolor <- factor(iris_bin_train$IsVersicolor)
iris_bin_test$IsVersicolor <- factor(iris_bin_test$IsVersicolor)

# Modelo completo
model_glm_full <- cla_glm(
  attribute = "IsVersicolor",
  positive = "versicolor",
  features = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)
model_glm_full <- fit(model_glm_full, iris_bin_train)
res_glm_full <- eval_model(model_glm_full, iris_bin_train, iris_bin_test, "IsVersicolor")
conf_full <- table(
  Pred = pred_to_label(res_glm_full$test_prediction),
  Actual = pred_to_label(res_glm_full$test_predictand)
)
conf_full

# Modelo simplificado (petal length/width)
model_glm_simple <- cla_glm(
  attribute = "IsVersicolor",
  positive = "versicolor",
  features = c("Petal.Length", "Petal.Width")
)
model_glm_simple <- fit(model_glm_simple, iris_bin_train)
res_glm_simple <- eval_model(model_glm_simple, iris_bin_train, iris_bin_test, "IsVersicolor")
conf_simple <- table(
  Pred = pred_to_label(res_glm_simple$test_prediction),
  Actual = pred_to_label(res_glm_simple$test_predictand)
)
conf_simple

# Slides 48–53: Aprendizagem preguiçosa (k-NN)
model_knn <- cla_knn("Species", slevels, k = 1)
model_knn <- fit(model_knn, iris_train)
res_knn <- eval_model(model_knn, iris_train, iris_test, "Species")
