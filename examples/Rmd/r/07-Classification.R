knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Slides 1–3: contexto e definição
library(daltoolbox)

# Slide 9: conjunto de dados de exemplo
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
rownames(class_tbl) <- c("dataset", "training", "test")
class_tbl

# Helper: avaliacao padrao DALToolbox
# Slides 21–25: avaliacao

eval_model <- function(model, train, test, target_col) {
  train_prediction <- predict(model, train)
  train_predictand <- adjust_class_label(train[, target_col])
  train_eval <- evaluate(model, train_predictand, train_prediction)
  print(train_eval$metrics)

  test_prediction <- predict(model, test)
  test_predictand <- adjust_class_label(test[, target_col])
  test_eval <- evaluate(model, test_predictand, test_prediction)
  print(test_eval$metrics)

  list(
    train_prediction = train_prediction,
    train_predictand = train_predictand,
    test_prediction = test_prediction,
    test_predictand = test_predictand
  )
}

pred_to_label <- function(pred) {
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

# Slide 10: Regra Zero (baseline)
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

binary_metrics <- function(actual, pred, positive = "versicolor") {
  actual <- factor(actual, levels = c(positive, setdiff(levels(actual), positive)))
  pred <- factor(pred, levels = levels(actual))
  tp <- sum(pred == positive & actual == positive)
  tn <- sum(pred != positive & actual != positive)
  fp <- sum(pred == positive & actual != positive)
  fn <- sum(pred != positive & actual == positive)

  acc <- (tp + tn) / length(actual)
  prec <- if ((tp + fp) == 0) NA else tp / (tp + fp)
  rec <- if ((tp + fn) == 0) NA else tp / (tp + fn)
  f1 <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA else 2 * prec * rec / (prec + rec)

  c(accuracy = acc, precision = prec, recall = rec, f1 = f1)
}

# Modelo completo
glm_full <- glm(
  IsVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris_bin_train,
  family = binomial
)

prob_full <- predict(glm_full, newdata = iris_bin_test, type = "response")
pred_full <- ifelse(prob_full >= 0.5, "versicolor", "not_versicolor")
pred_full <- factor(pred_full, levels = levels(iris_bin_train$IsVersicolor))
conf_full <- table(Pred = pred_full, Actual = iris_bin_test$IsVersicolor)
conf_full
binary_metrics(iris_bin_test$IsVersicolor, pred_full)

# Modelo simplificado (petal length/width)
glm_simple <- glm(
  IsVersicolor ~ Petal.Length + Petal.Width,
  data = iris_bin_train,
  family = binomial
)

prob_simple <- predict(glm_simple, newdata = iris_bin_test, type = "response")
pred_simple <- ifelse(prob_simple >= 0.5, "versicolor", "not_versicolor")
pred_simple <- factor(pred_simple, levels = levels(iris_bin_train$IsVersicolor))
conf_simple <- table(Pred = pred_simple, Actual = iris_bin_test$IsVersicolor)
conf_simple
binary_metrics(iris_bin_test$IsVersicolor, pred_simple)

# Slides 48–53: Aprendizagem preguiçosa (k-NN)
model_knn <- cla_knn("Species", slevels, k = 1)
model_knn <- fit(model_knn, iris_train)
res_knn <- eval_model(model_knn, iris_train, iris_test, "Species")

# Slides 7–8: familias de metodos (exemplos adicionais do DALToolbox)
# SVM
model_svm <- cla_svm("Species", slevels, epsilon = 0.0, cost = 20.000)
model_svm <- fit(model_svm, iris_train)
res_svm <- eval_model(model_svm, iris_train, iris_test, "Species")

# Random Forest
model_rf <- cla_rf("Species", slevels, mtry = 3, ntree = 5)
model_rf <- fit(model_rf, iris_train)
res_rf <- eval_model(model_rf, iris_train, iris_test, "Species")

# MLP
model_mlp <- cla_mlp("Species", slevels, size = 3, decay = 0.03)
model_mlp <- fit(model_mlp, iris_train)
res_mlp <- eval_model(model_mlp, iris_train, iris_test, "Species")

# Tuning (SVM)
tune <- cla_tune(
  cla_svm("Species", slevels),
  ranges = list(
    epsilon = seq(0, 1, 0.2),
    cost = seq(20, 100, 20),
    kernel = c("linear", "radial", "polynomial", "sigmoid")
  )
)

model_tuned <- fit(tune, iris_train)
res_tuned <- eval_model(model_tuned, iris_train, iris_test, "Species")
