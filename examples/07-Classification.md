

# Classificação

## Visão Geral
A classificação é uma tarefa de aprendizado supervisionado que aprende a mapear vetores de atributos para rótulos de classe. Ela envolve duas etapas: construção do modelo a partir de dados rotulados e aplicação do modelo a novos casos.  
Slides: 1–6.

## Como ler este roteiro
Use este roteiro como comparação incremental de modelos:
1. estabeleça baseline (Regra Zero);
2. avance para modelos mais expressivos;
3. compare sempre matriz de confusão e métricas no conjunto de teste.
Assim, o ganho de desempenho fica conectado ao custo de complexidade.

## Configuração

Esta seção prepara os dados e define dois helpers centrais:
1. `eval_model()`: executa predição e avaliação de forma robusta para diferentes formatos de saída dos modelos;
2. `pred_to_label()`: converte saídas de predição em rótulos, facilitando matriz de confusão e comparação entre modelos.

### Como funciona o `eval_model()`
O helper resolve um problema comum em aulas práticas: cada algoritmo pode retornar predições em formato diferente (vetor de classe, fator ou matriz de probabilidades).
O fluxo interno é:
1. padronizar o alvo real com `adjust_class_label()`;
2. tentar avaliação direta com `evaluate()`;
3. se falhar, reconstruir a predição em formato compatível;
4. avaliar treino e teste com o mesmo protocolo.
Assim, a comparação entre modelos fica consistente e reproduzível.


``` r
# Slides 1–3: contexto e definição
library(daltoolbox)

# Slides 9: conjunto de dados de exemplo
iris <- datasets::iris
head(iris)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```

``` r
table(iris$Species)
```

```
## 
##     setosa versicolor  virginica 
##         50         50         50
```

``` r
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
```

```
##        setosa versicolor virginica
## dados      50         50        50
## treino     39         38        43
## teste      11         12         7
```

``` r
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
```

## Regra Zero (Baseline)
A Regra Zero define uma referência mínima de desempenho, sempre prevendo a classe majoritária. Qualquer modelo útil deve superar esse baseline.  
Slides: 10.


``` r
# Slides 10: Regra Zero (baseline)
model_majority <- cla_majority("Species", slevels)
model_majority <- fit(model_majority, iris_train)
res_majority <- eval_model(model_majority, iris_train, iris_test, "Species")
```

```
##    accuracy TP TN FP FN precision recall sensitivity specificity  f1
## 1 0.3583333  0 81  0 39       NaN      0           0           1 NaN
##    accuracy TP TN FP FN precision recall sensitivity specificity  f1
## 1 0.2333333  0 19  0 11       NaN      0           0           1 NaN
```

## Árvores de Decisão
Árvores de decisão particionam o espaço de atributos recursivamente usando critérios como ganho de informação, produzindo modelos interpretáveis.  
Slides: 11–20.


``` r
# Slides 11–20: Arvore de Decisao
model_dtree <- cla_dtree("Species", slevels)
model_dtree <- fit(model_dtree, iris_train)
res_dtree <- eval_model(model_dtree, iris_train, iris_test, "Species")
```

```
##   accuracy TP TN FP FN precision recall sensitivity specificity f1
## 1    0.975 39 81  0  0         1      1           1           1  1
##    accuracy TP TN FP FN precision recall sensitivity specificity f1
## 1 0.9666667 11 19  0  0         1      1           1           1  1
```

### Matriz de Confusão
A matriz de confusão permite analisar erros específicos de classificação e derivar métricas como precisão e revocação.  
Slides: 23–25.


``` r
# Slides 21–25: avaliacao e matriz de confusao (exemplo com arvore)
# predict() pode retornar data.frame; usar vetor para a tabela
conf_dtree <- table(
  Pred = pred_to_label(res_dtree$test_prediction),
  Actual = pred_to_label(res_dtree$test_predictand)
)
conf_dtree
```

```
##             Actual
## Pred         setosa versicolor virginica
##   setosa         11          0         0
##   versicolor      0         12         1
##   virginica       0          0         6
```

## Classificação Bayesiana
Naive Bayes assume independência condicional entre atributos. Apesar da suposição forte, tende a ser eficiente e competitivo em diversos cenários.  
Slides: 33–40.


``` r
# Slides 33–40: Naive Bayes
model_nb <- cla_nb("Species", slevels)
model_nb <- fit(model_nb, iris_train)
res_nb <- eval_model(model_nb, iris_train, iris_test, "Species")
```

```
##    accuracy TP TN FP FN precision recall sensitivity specificity f1
## 1 0.9583333 39 81  0  0         1      1           1           1  1
##    accuracy TP TN FP FN precision recall sensitivity specificity f1
## 1 0.9666667 11 19  0  0         1      1           1           1  1
```

## Regressão Logística
Modela a probabilidade de pertencimento a uma classe via função logística. Aqui, reduzimos para um problema binário (versicolor vs. não-versicolor), comparando modelo completo e simplificado.  
Slides: 41–47.

Leitura didática desta seção:
1. o modelo completo tende a capturar mais sinal;
2. o modelo simplificado testa se menos variáveis mantêm desempenho;
3. a matriz de confusão mostra onde ocorre perda/ganho por simplificação.


``` r
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
```

```
##    accuracy TP TN FP FN precision    recall sensitivity specificity        f1
## 1 0.7583333 73 18 20  9 0.7849462 0.8902439   0.8902439   0.4736842 0.8342857
##    accuracy TP TN FP FN precision    recall sensitivity specificity        f1
## 1 0.6666667 16  4  8  2 0.6666667 0.8888889   0.8888889   0.3333333 0.7619048
```

``` r
conf_full <- table(
  Pred = pred_to_label(res_glm_full$test_prediction),
  Actual = pred_to_label(res_glm_full$test_predictand)
)
conf_full
```

```
##                 Actual
## Pred             not_versicolor versicolor
##   not_versicolor             16          8
##   versicolor                  2          4
```

``` r
# Modelo simplificado (petal length/width)
model_glm_simple <- cla_glm(
  attribute = "IsVersicolor",
  positive = "versicolor",
  features = c("Petal.Length", "Petal.Width")
)
model_glm_simple <- fit(model_glm_simple, iris_bin_train)
res_glm_simple <- eval_model(model_glm_simple, iris_bin_train, iris_bin_test, "IsVersicolor")
```

```
##   accuracy TP TN FP FN precision    recall sensitivity specificity        f1
## 1    0.625 69  6 32 13 0.6831683 0.8414634   0.8414634   0.1578947 0.7540984
##    accuracy TP TN FP FN precision    recall sensitivity specificity        f1
## 1 0.5666667 17  0 12  1 0.5862069 0.9444444   0.9444444           0 0.7234043
```

``` r
conf_simple <- table(
  Pred = pred_to_label(res_glm_simple$test_prediction),
  Actual = pred_to_label(res_glm_simple$test_predictand)
)
conf_simple
```

```
##                 Actual
## Pred             not_versicolor versicolor
##   not_versicolor             17         12
##   versicolor                  1          0
```

## k-NN (Aprendizagem Preguiçosa)
Métodos baseados em instâncias adiam o processamento para a fase de predição. O k-NN decide pela maioria dos k vizinhos mais próximos.  
Slides: 48–53.


``` r
# Slides 48–53: Aprendizagem preguiçosa (k-NN)
model_knn <- cla_knn("Species", slevels, k = 1)
model_knn <- fit(model_knn, iris_train)
res_knn <- eval_model(model_knn, iris_train, iris_test, "Species")
```

```
##   accuracy TP TN FP FN precision recall sensitivity specificity f1
## 1        1 39 81  0  0         1      1           1           1  1
##    accuracy TP TN FP FN precision recall sensitivity specificity f1
## 1 0.9333333 11 19  0  0         1      1           1           1  1
```

## Próxima Aula
Métodos avançados (SVM, ensembles, boosting, tuning e seleção de atributos) foram concentrados em `08-Classification-Advanced.Rmd` para evitar repetição e manter a progressão didática.

## Referências
- Han, J., Pei, J., & Tong, H. (2022). *Data Mining: Concepts and Techniques* (4th ed.). Morgan Kaufmann.
- Mitchell, T. (1997). *Machine Learning*. McGraw-Hill.
- Bishop, C. M. (2006). *Pattern Recognition and Machine Learning*. Springer.
- Cortes, C., & Vapnik, V. (1995). Support-Vector Networks. *Machine Learning*, 20(3), 273–297.
- Breiman, L. (2001). Random Forests. *Machine Learning*, 45(1), 5–32.
- Cover, T., & Hart, P. (1967). Nearest neighbor pattern classification. *IEEE Trans. Information Theory*, 13(1), 21–27.
- Hosmer, D., Lemeshow, S., & Sturdivant, R. (2013). *Applied Logistic Regression* (3rd ed.). Wiley.


