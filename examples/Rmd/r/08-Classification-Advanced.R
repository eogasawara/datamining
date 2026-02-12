knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Slides 1–3: contexto
library(daltoolbox)
library(nnet)
library(ipred)
library(randomForest)
library(e1071)
library(glmnet)
library(rpart)
library(adabag)
library(xgboost)

# Conjunto de dados base
iris <- datasets::iris
head(iris)

# Preparacao treino/teste
set.seed(1)
split_random <- sample_random()
split_random <- train_test(split_random, iris)
iris_train <- split_random$train
iris_test <- split_random$test
slevels <- levels(iris$Species)

# Helper: avaliacao DALToolbox
# Slides 20–24: avaliacao comparativa

eval_model <- function(model, train, test, target_col) {
  evaluate_safe <- function(data, prediction, target_col) {
    # Garante representacao consistente do alvo verdadeiro
    predictand <- adjust_class_label(data[, target_col])
    eval <- evaluate(model, predictand, prediction)

    # Caminho alternativo para padronizar formatos de saida diferentes
    if (is.null(eval) || is.null(eval$metrics)) {
      proxy <- classification(target_col, colnames(predictand))

      # Saida por rotulos
      if (is.factor(prediction) || is.character(prediction) || is.vector(prediction)) {
        pred <- factor(as.vector(prediction), levels = colnames(predictand))
        prediction <- adjust_class_label(pred)
      } else {
        # Saida matricial (escores/probabilidades)
        prediction <- as.matrix(prediction)
        if (is.null(colnames(prediction))) {
          colnames(prediction) <- colnames(predictand)[seq_len(ncol(prediction))]
        }
        # Alinha colunas previstas com o espaco de classes observado
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

# Slides 2–8: MLP
model_mlp <- cla_mlp("Species", slevels, size = 3, decay = 0.03)
model_mlp <- fit(model_mlp, iris_train)
res_mlp <- eval_model(model_mlp, iris_train, iris_test, "Species")

# Slides 9–19: SVM
model_svm <- cla_svm("Species", slevels, epsilon = 0.0, cost = 20.000)
model_svm <- fit(model_svm, iris_train)
res_svm <- eval_model(model_svm, iris_train, iris_test, "Species")

# Slides 20–24: Bagging e Random Forest
# Bagging com arvores (ipred)
set.seed(1)
model_bag <- cla_bagging("Species", nbagg = 25)
model_bag <- fit(model_bag, iris_train)
res_bag <- eval_model(model_bag, iris_train, iris_test, "Species")

# Random Forest (daltoolbox)
model_rf <- cla_rf("Species", slevels, mtry = 3, ntree = 50)
model_rf <- fit(model_rf, iris_train)
res_rf <- eval_model(model_rf, iris_train, iris_test, "Species")

# Slides 25: Boosting
set.seed(1)
model_boost <- cla_boosting("Species", mfinal = 50)
model_boost <- fit(model_boost, iris_train)
res_boost <- eval_model(model_boost, iris_train, iris_test, "Species")

# XGBoost (daltoolbox)
model_xgb <- cla_xgboost("Species", nrounds = 20)
model_xgb <- fit(model_xgb, iris_train)
res_xgb <- eval_model(model_xgb, iris_train, iris_test, "Species")

# Slides 26–36: selecao de atributos
# Conjunto de dados binario para alguns metodos
iris_bin <- iris
tr_fg_bin <- feature_generation(
  IsVersicolor = ifelse(Species == "versicolor", "versicolor", "not_versicolor")
)
tmp_bin <- try(transform(tr_fg_bin, iris), silent = TRUE)
if (!inherits(tmp_bin, "try-error") && "IsVersicolor" %in% names(tmp_bin)) {
  iris_bin$IsVersicolor <- tmp_bin$IsVersicolor
} else {
  iris_bin$IsVersicolor <- ifelse(iris$Species == "versicolor", "versicolor", "not_versicolor")
}
iris_bin$IsVersicolor <- factor(iris_bin$IsVersicolor)

# Slides 32: Information Gain (discretizacao simples)
entropy <- function(y) {
  # Entropia de Shannon da distribuicao de classes
  p <- prop.table(table(y))
  -sum(p * log2(p))
}

make_bins <- function(x, bins = 3) {
  # Discretiza variavel numerica via quantis para computar ganho de informacao
  q <- quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
  q <- unique(q)
  if (length(q) < 2) {
    return(factor(rep("all", length(x))))
  }
  tmp <- data.frame(x = x)
  tr_hcut_bin <- hierarchy_cut(
    attribute = "x",
    breaks = q,
    labels = paste0("Q", seq_len(length(q) - 1)),
    new_attribute = "x_bin"
  )
  tmp <- transform(tr_hcut_bin, tmp)
  factor(tmp$x_bin)
}

info_gain <- function(x, y, bins = 3) {
  if (is.numeric(x)) {
    x <- make_bins(x, bins = bins)
  }
  # IG = H(Y) - H(Y|X)
  total <- entropy(y)
  cond <- 0
  for (lvl in levels(x)) {
    idx <- which(x == lvl)
    if (length(idx) > 0) {
      cond <- cond + (length(idx) / length(y)) * entropy(y[idx])
    }
  }
  total - cond
}

ig_scores <- sapply(iris[, 1:4], info_gain, y = iris$Species)
ig_scores[!is.finite(ig_scores)] <- 0
ig_scores

# Slides 33: Forward Stepwise Selection (glm binario)
full_glm <- glm(IsVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                data = iris_bin, family = binomial)
null_glm <- glm(IsVersicolor ~ 1, data = iris_bin, family = binomial)
step_model <- step(null_glm, scope = list(lower = null_glm, upper = full_glm), direction = "forward", trace = 0)
summary(step_model)

# Slides 34: LASSO (glmnet)
iris_lasso <- iris_bin[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "IsVersicolor")]
iris_lasso$IsVersicolor <- factor(iris_lasso$IsVersicolor)
set.seed(1)
model_lasso <- cla_glmnet("IsVersicolor", lambda = "lambda.min")
model_lasso <- fit(model_lasso, iris_lasso)
coef(model_lasso$model, s = "lambda.min")

# Slides 35: CFS (correlation-based)
# CFS simples: correlacao com classe (numerica) e penalidade por correlacao entre features
class_num <- as.numeric(iris$Species)
cor_cf <- abs(cor(iris[, 1:4], class_num))
cor_ff <- abs(cor(iris[, 1:4]))
mean_cf <- mean(cor_cf)
mean_ff <- mean(cor_ff[upper.tri(cor_ff)])
cfs_score <- mean_cf / sqrt(mean_ff + 1e-6)
cfs_score

# Slides 36: RELIEF simplificado (binario)
relief_simple <- function(df, target, m = 50) {
  X <- as.matrix(df)
  y <- target
  n <- nrow(X)
  w <- rep(0, ncol(X))
  set.seed(1)
  idxs <- sample(seq_len(n), size = min(m, n))
  for (i in idxs) {
    xi <- X[i, , drop = FALSE]
    yi <- y[i]
    # Distancias euclidianas entre a instancia atual e as demais
    d <- rowSums((X - matrix(xi, n, ncol(X), byrow = TRUE))^2)
    same <- which(y == yi & seq_len(n) != i)
    diff <- which(y != yi)
    nh <- same[which.min(d[same])]
    nm <- diff[which.min(d[diff])]
    w <- w - abs(X[i, ] - X[nh, ]) + abs(X[i, ] - X[nm, ])
  }
  w
}

relief_w <- relief_simple(
  iris_bin[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
  iris_bin$IsVersicolor
)
relief_w

# Slides 37–40: topicos avancados
# Imbalanced: downsample simples
sb_down <- sample_balance("IsVersicolor", method = "down", seed = 1)
iris_bal <- transform(sb_down, iris_bin)

bal_glm <- glm(IsVersicolor ~ Petal.Length + Petal.Width, data = iris_bal, family = binomial)
summary(bal_glm)

# Multiclasse: modelo multinomial (nnet)
multinom_model <- cla_multinom("Species")
multinom_model <- fit(multinom_model, iris_train)
multinom_pred <- predict(multinom_model, iris_test)
mean(multinom_pred == iris_test$Species)

# Slides 41: semi-supervisionado (pseudo-label simples)
set.seed(1)
mask <- sample(seq_len(nrow(iris_train)), size = floor(0.5 * nrow(iris_train)))
semi_train <- iris_train
semi_train$Species[mask] <- NA

labeled <- semi_train[!is.na(semi_train$Species), ]
unlabeled <- semi_train[is.na(semi_train$Species), ]

semi_model <- cla_multinom("Species")
semi_model <- fit(semi_model, labeled)
probs <- stats::predict(semi_model$model, unlabeled, type = "probs")
pseudo <- colnames(probs)[apply(probs, 1, which.max)]
# adiciona pseudo-rotulos
unlabeled$Species <- factor(pseudo, levels = levels(iris$Species))
semi_aug <- rbind(labeled, unlabeled)

semi_model2 <- cla_multinom("Species")
semi_model2 <- fit(semi_model2, semi_aug)
semi_pred <- predict(semi_model2, iris_test)
mean(semi_pred == iris_test$Species)

# Slides 42–43: Transfer Learning (exemplo conceitual)
# Reuso de um modelo treinado (ajuste fino com subconjunto)
pre_model <- cla_multinom("Species")
pre_model <- fit(pre_model, iris_train)

fine_idx <- sample(seq_len(nrow(iris_train)), size = 20)
fine_train <- iris_train[fine_idx, ]

fine_model <- cla_multinom("Species")
fine_model <- fit(fine_model, fine_train)

pre_pred <- predict(pre_model, iris_test)
fine_pred <- predict(fine_model, iris_test)

mean(pre_pred == iris_test$Species)
mean(fine_pred == iris_test$Species)
