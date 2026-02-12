

# Mineração de Padrões

## Visão Geral
A mineração de padrões frequentes busca regularidades em dados transacionais e sequenciais. A partir de padrões frequentes, derivam-se regras de associação com medidas como suporte, confiança e lift.  
Slides: 1–5.

## Como ler este roteiro
O fluxo está organizado em quatro passos:
1. preparar dados transacionais;
2. minerar regras/itemsets;
3. reduzir redundância e interpretar qualidade;
4. avançar para sequências e grafos.
Em cada passo, compare sempre quantidade de padrões versus utilidade prática.

## Configuração


``` r
# Slides 1–3: contexto
# DAL ToolBox
# version 1.0.77

library(daltoolbox)

library(arules)
library(arulesViz)
library(arulesSequences)
library(igraph)
```


``` r
# Slides 2–4: exemplo transacional
data(AdultUCI)
myAdultUCI <- as.data.frame(AdultUCI)
dim(myAdultUCI)
```

```
## [1] 48842    15
```

``` r
head(myAdultUCI)
```

```
##   age        workclass fnlwgt education education-num     marital-status        occupation  relationship  race    sex capital-gain capital-loss
## 1  39        State-gov  77516 Bachelors            13      Never-married      Adm-clerical Not-in-family White   Male         2174            0
## 2  50 Self-emp-not-inc  83311 Bachelors            13 Married-civ-spouse   Exec-managerial       Husband White   Male            0            0
## 3  38          Private 215646   HS-grad             9           Divorced Handlers-cleaners Not-in-family White   Male            0            0
## 4  53          Private 234721      11th             7 Married-civ-spouse Handlers-cleaners       Husband Black   Male            0            0
## 5  28          Private 338409 Bachelors            13 Married-civ-spouse    Prof-specialty          Wife Black Female            0            0
## 6  37          Private 284582   Masters            14 Married-civ-spouse   Exec-managerial          Wife White Female            0            0
##   hours-per-week native-country income
## 1             40  United-States  small
## 2             13  United-States  small
## 3             40  United-States  small
## 4             40  United-States  small
## 5             40           Cuba  small
## 6             40  United-States  small
```

## Pré-processamento e Hierarquias Conceituais
A discretização e a criação de níveis agregados ajudam a reduzir a cardinalidade e a revelar padrões em diferentes granularidades.  
Slides: 5–9.


``` r
# Limpeza básica e remoção de atributos
na_obj <- na_removal()
myAdultUCI <- transform(na_obj, myAdultUCI)

myAdultUCI$fnlwgt <- NULL
myAdultUCI$"education-num" <- NULL
```


``` r
# Hierarquia conceitual e binning com transformacoes do DALToolbox
hc_age <- hierarchy_cut(
  attribute = "age",
  breaks = c(15, 25, 45, 65, 100),
  labels = c("Young", "Middle-aged", "Senior", "Old"),
  new_attribute = "age_level"
)
myAdultUCI <- transform(hc_age, myAdultUCI)
myAdultUCI$age <- ordered(myAdultUCI$age_level, levels = c("Young", "Middle-aged", "Senior", "Old"))
myAdultUCI$age_level <- NULL

hc_hours <- hierarchy_cut(
  attribute = "hours-per-week",
  breaks = c(0, 25, 40, 60, 168),
  labels = c("Part-time", "Full-time", "Over-time", "Workaholic"),
  new_attribute = "hours_level"
)
myAdultUCI <- transform(hc_hours, myAdultUCI)
myAdultUCI$"hours-per-week" <- ordered(
  myAdultUCI$hours_level,
  levels = c("Part-time", "Full-time", "Over-time", "Workaholic")
)
myAdultUCI$hours_level <- NULL

gain_positive_median <- median(myAdultUCI$"capital-gain"[myAdultUCI$"capital-gain" > 0])
hc_gain <- hierarchy_cut(
  attribute = "capital-gain",
  breaks = c(-Inf, 0, gain_positive_median, Inf),
  labels = c("None", "Low", "High"),
  new_attribute = "capital_gain_level"
)
myAdultUCI <- transform(hc_gain, myAdultUCI)
myAdultUCI$"capital-gain" <- ordered(myAdultUCI$capital_gain_level, levels = c("None", "Low", "High"))
myAdultUCI$capital_gain_level <- NULL

loss_positive_median <- median(myAdultUCI$"capital-loss"[myAdultUCI$"capital-loss" > 0])
hc_loss <- hierarchy_cut(
  attribute = "capital-loss",
  breaks = c(-Inf, 0, loss_positive_median, Inf),
  labels = c("None", "Low", "High"),
  new_attribute = "capital_loss_level"
)
myAdultUCI <- transform(hc_loss, myAdultUCI)
myAdultUCI$"capital-loss" <- ordered(myAdultUCI$capital_loss_level, levels = c("None", "Low", "High"))
myAdultUCI$capital_loss_level <- NULL

head(myAdultUCI)
```

```
##           age        workclass education     marital-status        occupation  relationship  race    sex capital-gain capital-loss
## 1 Middle-aged        State-gov Bachelors      Never-married      Adm-clerical Not-in-family White   Male          Low         None
## 2      Senior Self-emp-not-inc Bachelors Married-civ-spouse   Exec-managerial       Husband White   Male         None         None
## 3 Middle-aged          Private   HS-grad           Divorced Handlers-cleaners Not-in-family White   Male         None         None
## 4      Senior          Private      11th Married-civ-spouse Handlers-cleaners       Husband Black   Male         None         None
## 5 Middle-aged          Private Bachelors Married-civ-spouse    Prof-specialty          Wife Black Female         None         None
## 6 Middle-aged          Private   Masters Married-civ-spouse   Exec-managerial          Wife White Female         None         None
##   hours-per-week native-country income
## 1      Full-time  United-States  small
## 2      Part-time  United-States  small
## 3      Full-time  United-States  small
## 4      Full-time  United-States  small
## 5      Full-time           Cuba  small
## 6      Full-time  United-States  small
```

## Conversão para Transações
Dados transacionais são a base para Apriori, ECLAT e regras de associação.  
Slides: 2–5.


``` r
# Convert to transactions
AdultTrans <- as(myAdultUCI, "transactions")
```

## Apriori
Apriori explora a propriedade anti-monótona do suporte para gerar e podar candidatos.  
Slides: 10–20.

Leitura recomendada dos resultados: comece por suporte/confiança, depois use lift para priorização.


``` r
# A Priori
pm_apriori <- pat_apriori(
  parameter = list(supp = 0.5, conf = 0.9, minlen = 2, maxlen = 10, target = "rules"),
  appearance = list(rhs = c("capital-gain=None"), default = "lhs"),
  control = NULL
)
pm_apriori <- fit(pm_apriori, AdultTrans)
rules <- discover(pm_apriori, AdultTrans)
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
##         0.9    0.1    1 none FALSE            TRUE       5     0.5      2     10  rules TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 15081 
## 
## set item appearances ...[1 item(s)] done [0.00s].
## set transactions ...[114 item(s), 30162 transaction(s)] done [0.03s].
## sorting and recoding items ... [9 item(s)] done [0.00s].
## creating transaction tree ... done [0.02s].
## checking subsets of size 1 2 3 4 5 done [0.00s].
## writing ... [30 rule(s)] done [0.00s].
## creating S4 object  ... done [0.00s].
```

``` r
inspect(rules)
```

```
##      lhs                                rhs                   support confidence  coverage      lift count
## [1]  {hours-per-week=Full-time}      => {capital-gain=None} 0.5445925  0.9278129 0.5869637 1.0130572 16426
## [2]  {sex=Male}                      => {capital-gain=None} 0.6101386  0.9029931 0.6756846 0.9859571 18403
## [3]  {workclass=Private}             => {capital-gain=None} 0.6824150  0.9235843 0.7388767 1.0084401 20583
## [4]  {income=small}                  => {capital-gain=None} 0.7198130  0.9583738 0.7510775 1.0464260 21711
## [5]  {race=White}                    => {capital-gain=None} 0.7846960  0.9126595 0.8597905 0.9965116 23668
## [6]  {native-country=United-States}  => {capital-gain=None} 0.8336649  0.9142307 0.9118759 0.9982271 25145
## [7]  {capital-loss=None}             => {capital-gain=None} 0.8685432  0.9116757 0.9526888 0.9954373 26197
## [8]  {capital-loss=None,                                                                                  
##       hours-per-week=Full-time}      => {capital-gain=None} 0.5202573  0.9246906 0.5626285 1.0096481 15692
## [9]  {race=White,                                                                                         
##       sex=Male}                      => {capital-gain=None} 0.5384921  0.9004324 0.5980373 0.9831611 16242
## [10] {sex=Male,                                                                                           
##       native-country=United-States}  => {capital-gain=None} 0.5549367  0.9012492 0.6157417 0.9840529 16738
## [11] {workclass=Private,                                                                                  
##       income=small}                  => {capital-gain=None} 0.5545388  0.9607122 0.5772164 1.0489792 16726
## [12] {workclass=Private,                                                                                  
##       race=White}                    => {capital-gain=None} 0.5841456  0.9206772 0.6344738 1.0052659 17619
## [13] {workclass=Private,                                                                                  
##       native-country=United-States}  => {capital-gain=None} 0.6152775  0.9216787 0.6675618 1.0063594 18558
## [14] {workclass=Private,                                                                                  
##       capital-loss=None}             => {capital-gain=None} 0.6505868  0.9201444 0.7070486 1.0046842 19623
## [15] {race=White,                                                                                         
##       income=small}                  => {capital-gain=None} 0.6058948  0.9571069 0.6330482 1.0450427 18275
## [16] {native-country=United-States,                                                                       
##       income=small}                  => {capital-gain=None} 0.6514488  0.9580672 0.6799615 1.0460912 19649
## [17] {capital-loss=None,                                                                                  
##       income=small}                  => {capital-gain=None} 0.6969365  0.9570661 0.7282010 1.0449981 21021
## [18] {race=White,                                                                                         
##       native-country=United-States}  => {capital-gain=None} 0.7316491  0.9112231 0.8029308 0.9949432 22068
## [19] {race=White,                                                                                         
##       capital-loss=None}             => {capital-gain=None} 0.7421922  0.9081173 0.8172867 0.9915521 22386
## [20] {capital-loss=None,                                                                                  
##       native-country=United-States}  => {capital-gain=None} 0.7896028  0.9098758 0.8678138 0.9934722 23816
## [21] {workclass=Private,                                                                                  
##       capital-loss=None,                                                                                  
##       income=small}                  => {capital-gain=None} 0.5380943  0.9595601 0.5607718 1.0477213 16230
## [22] {workclass=Private,                                                                                  
##       race=White,                                                                                         
##       native-country=United-States}  => {capital-gain=None} 0.5401499  0.9189982 0.5877594 1.0034327 16292
## [23] {workclass=Private,                                                                                  
##       race=White,                                                                                         
##       capital-loss=None}             => {capital-gain=None} 0.5555003  0.9169266 0.6058285 1.0011707 16755
## [24] {workclass=Private,                                                                                  
##       capital-loss=None,                                                                                  
##       native-country=United-States}  => {capital-gain=None} 0.5855049  0.9180226 0.6377893 1.0023674 17660
## [25] {race=White,                                                                                         
##       native-country=United-States,                                                                       
##       income=small}                  => {capital-gain=None} 0.5610371  0.9571808 0.5861349 1.0451234 16922
## [26] {race=White,                                                                                         
##       capital-loss=None,                                                                                  
##       income=small}                  => {capital-gain=None} 0.5858365  0.9557034 0.6129899 1.0435102 17670
## [27] {capital-loss=None,                                                                                  
##       native-country=United-States,                                                                       
##       income=small}                  => {capital-gain=None} 0.6302301  0.9567165 0.6587428 1.0446164 19009
## [28] {race=White,                                                                                         
##       capital-loss=None,                                                                                  
##       native-country=United-States}  => {capital-gain=None} 0.6908030  0.9064648 0.7620847 0.9897477 20836
## [29] {workclass=Private,                                                                                  
##       race=White,                                                                                         
##       capital-loss=None,                                                                                  
##       native-country=United-States}  => {capital-gain=None} 0.5125323  0.9150044 0.5601419 0.9990720 15459
## [30] {race=White,                                                                                         
##       capital-loss=None,                                                                                  
##       native-country=United-States,                                                                       
##       income=small}                  => {capital-gain=None} 0.5418407  0.9557310 0.5669385 1.0435403 16343
```


``` r
rules_a <- as(rules, "data.frame")
head(rules_a)
```

```
##                                                   rules   support confidence  coverage      lift count
## 1     {hours-per-week=Full-time} => {capital-gain=None} 0.5445925  0.9278129 0.5869637 1.0130572 16426
## 2                     {sex=Male} => {capital-gain=None} 0.6101386  0.9029931 0.6756846 0.9859571 18403
## 3            {workclass=Private} => {capital-gain=None} 0.6824150  0.9235843 0.7388767 1.0084401 20583
## 4                 {income=small} => {capital-gain=None} 0.7198130  0.9583738 0.7510775 1.0464260 21711
## 5                   {race=White} => {capital-gain=None} 0.7846960  0.9126595 0.8597905 0.9965116 23668
## 6 {native-country=United-States} => {capital-gain=None} 0.8336649  0.9142307 0.9118759 0.9982271 25145
```

## ECLAT e Itemsets Frequentes
ECLAT usa representação vertical, eficiente para contagem de suportes.  
Slides: 22–24.


``` r
# ECLAT (itemsets frequentes)
pm_eclat <- pat_eclat(parameter = list(supp = 0.5, maxlen = 3))
pm_eclat <- fit(pm_eclat, AdultTrans)
itemsets_eclat <- discover(pm_eclat, AdultTrans)
```

```
## Eclat
## 
## parameter specification:
##  tidLists support minlen maxlen            target  ext
##     FALSE     0.5      1      3 frequent itemsets TRUE
## 
## algorithmic control:
##  sparse sort verbose
##       7   -2    TRUE
## 
## Absolute minimum support count: 15081 
## 
## create itemset ... 
## set transactions ...[114 item(s), 30162 transaction(s)] done [0.03s].
## sorting and recoding items ... [9 item(s)] done [0.00s].
## creating bit matrix ... [9 row(s), 30162 column(s)] done [0.00s].
## writing  ... [58 set(s)] done [0.00s].
## Creating S4 object  ... done [0.00s].
```

``` r
inspect(head(sort(itemsets_eclat, by = "support")))
```

```
##     items                                             support   count
## [1] {capital-loss=None}                               0.9526888 28735
## [2] {capital-gain=None}                               0.9158544 27624
## [3] {native-country=United-States}                    0.9118759 27504
## [4] {capital-gain=None, capital-loss=None}            0.8685432 26197
## [5] {capital-loss=None, native-country=United-States} 0.8678138 26175
## [6] {race=White}                                      0.8597905 25933
```

## Padrões Fechados e Max-Padrões
Padrões fechados preservam suporte; max-padrões reduzem ainda mais o volume, mas perdem detalhes.  
Slides: 6–9.


``` r
# Padrões fechados e max-padrões
closed_sets <- itemsets_eclat[is.closed(itemsets_eclat)]
max_sets <- itemsets_eclat[is.maximal(itemsets_eclat)]

inspect(head(sort(closed_sets, by = "support")))
```

```
##     items                                             support   count
## [1] {capital-loss=None}                               0.9526888 28735
## [2] {capital-gain=None}                               0.9158544 27624
## [3] {native-country=United-States}                    0.9118759 27504
## [4] {capital-gain=None, capital-loss=None}            0.8685432 26197
## [5] {capital-loss=None, native-country=United-States} 0.8678138 26175
## [6] {race=White}                                      0.8597905 25933
```

``` r
inspect(head(sort(max_sets, by = "support")))
```

```
##     items                                                                support   count
## [1] {capital-gain=None, capital-loss=None, native-country=United-States} 0.7896028 23816
## [2] {race=White, capital-loss=None, native-country=United-States}        0.7620847 22986
## [3] {race=White, capital-gain=None, capital-loss=None}                   0.7421922 22386
## [4] {race=White, capital-gain=None, native-country=United-States}        0.7316491 22068
## [5] {capital-gain=None, capital-loss=None, income=small}                 0.6969365 21021
## [6] {capital-loss=None, native-country=United-States, income=small}      0.6587428 19869
```

## Medidas de Interesse
Além de suporte e confiança, métricas como lift, leverage e conviction ajudam a priorizar regras.  
Slides: 4–5, 21.

Aqui, o foco é transformar regras "frequentes" em regras "úteis" para decisão.


``` r
# Analise das regras
imrules <- interestMeasure(rules, transactions = AdultTrans)
head(imrules)
```

```
##     support confidence      lift count   addedValue boost casualConfidence casualSupport centeredConfidence   certainty chiSquared
## 1 0.5445925  0.9278129 1.0130572 16426  0.011958537   Inf        0.9999961      1.418076        0.011958537  0.14211718   79.53924
## 2 0.6101386  0.9029931 0.9859571 18403 -0.012861256   Inf        0.9999944      1.460447       -0.012861256 -0.15284523  134.87936
## 3 0.6824150  0.9235843 1.0084401 20583  0.007729927   Inf        0.9999954      1.541808        0.007729927  0.09186369   66.17278
## 4 0.7198130  0.9583738 1.0464260 21711  0.042519411   Inf        0.9999975      1.604403        0.042519411  0.50530751 2134.99731
## 5 0.7846960  0.9126595 0.9965116 23668 -0.003194841   Inf        0.9999944      1.625456       -0.003194841 -0.03796800   24.49716
## 6 0.8336649  0.9142307 0.9982271 25145 -0.001623729   Inf        0.9999943      1.671308       -0.001623729 -0.01929666   10.67751
##   collectiveStrength confirmedConfidence conviction    cosine counterexample  coverage         doc fishersExactTest         gini
## 1          1319.7337           0.8556258  1.1656604 0.7427674      0.9221965 0.5869637  0.02895275     4.862879e-19 4.064519e-04
## 2           527.2165           0.8059863  0.8674191 0.7756097      0.8925719 0.6756846 -0.03965663     1.000000e+00 6.892444e-04
## 3           875.2118           0.8471686  1.1011563 0.8295629      0.9172618 0.7388767  0.02960260     1.058767e-15 3.381483e-04
## 4          2138.0018           0.9167476  2.0214578 0.8678888      0.9565658 0.7510775  0.17081386     0.000000e+00 1.091001e-02
## 5           264.7989           0.8253191  0.9634208 0.8842843      0.9043012 0.8597905 -0.02278619     9.999999e-01 1.251825e-04
## 6           175.3826           0.8284613  0.9810687 0.9122428      0.9061841 0.9118759 -0.01842548     9.996579e-01 5.456296e-05
##   hyperConfidence hyperLift   imbalance implicationIndex   importance improvement   jaccard     jMeasure       kappa kulczynski lambda
## 1    1.000000e+00 1.0095882 0.343228842       -5.4852702  0.013776595   0.9278129 0.5683344 5.698697e-04  0.03282532  0.7612204      0
## 2    1.735796e-33 0.9831713 0.244721462        6.3295047 -0.018643247   0.9029931 0.6217020 6.939638e-04 -0.04911334  0.7845946      0
## 3    1.000000e+00 1.0060117 0.182016572       -3.9781000  0.014178630   0.9235843 0.7018447 2.947970e-04  0.03790910  0.8343486      0
## 4    1.000000e+00 1.0439486 0.173976966      -22.0619525  0.085275211   0.9583738 0.7600028 1.066092e-02  0.21935396  0.8721604      0
## 5    1.381495e-07 0.9948718 0.056575998        1.7736169 -0.010629368   0.9126595 0.7918632 5.629598e-05 -0.02736507  0.8847254      0
## 6    3.421033e-04 0.9969471 0.004002268        0.9283158 -0.008528572   0.9142307 0.8386419 1.550798e-05 -0.01880903  0.9122449      0
##     laplace leastContradiction     lerman     leverage LIC maxconfidence mutualInformation oddsRatio         phi ralambondrainy relativeRisk
## 1 0.9277646          0.5946279  1.6626489  0.007019228 Inf     0.9278129      0.0045044541 1.4462090  0.05135239     0.04237120    1.0322105
## 2 0.9029536          0.6661961 -1.9185462 -0.008690153 Inf     0.9029931      0.0082397525 0.5663264 -0.06687175     0.06554605    0.9579307
## 3 0.9235463          0.7451129  1.2058082  0.005711463 Inf     0.9235843      0.0036318019 1.4333299  0.04683922     0.05646177    1.0331132
## 4 0.9583333          0.7859470  6.6872332  0.031935373 Inf     0.9583738      0.1040536397 6.2104196  0.26605327     0.03126451    1.2168900
## 5 0.9126277          0.8567912 -0.5376038 -0.002746893 Inf     0.9126595      0.0015008997 0.7211070 -0.02849888     0.07509449    0.9756414
## 6 0.9142005          0.9102592 -0.2813833 -0.001480639 Inf     0.9142307      0.0006502794 0.7696623 -0.01881503     0.07821099    0.9802441
##   rhsSupport        RLD rulePowerFactor     sebag    stdLift table.n11 table.n01 table.n10 table.n00 varyingLiaison      yuleQ       yuleY
## 1  0.9158544 0.14211718       0.5052800 12.852895 0.27812924     16426     11198      1278      1260    0.013057247  0.1824084  0.09197572
## 2  0.9158544 0.31844059       0.5509510  9.308548 0.02993131     18403      9221      1977       561   -0.014042905 -0.2768731 -0.14119647
## 3  0.9158544 0.09186369       0.6302678 12.086318 0.23584313     20583      7041      1703       835    0.008440126  0.1780810  0.08975787
## 4  0.9158544 0.50530751       0.6898499 23.023330 0.58373797     21711      5913       943      1595    0.046425951  0.7226236  0.42727402
## 5  0.9158544 0.23282670       0.7161603 10.449448 0.10756501     23668      3956      2265       273   -0.003488372 -0.1620428 -0.08156036
## 6  0.9158544 0.19967465       0.7621620 10.659178 0.07052797     25145      2479      2359       179   -0.001772912 -0.1301591 -0.06535756
```


``` r
imrules2 <- interestMeasure(rules, c("support", "confidence", "lift", "leverage", "conviction"), AdultTrans)
head(imrules2[order(imrules2[, "lift"], decreasing = TRUE), ])
```

```
##      support confidence     lift   leverage conviction
## 11 0.5545388  0.9607122 1.048979 0.02589268   2.141777
## 21 0.5380943  0.9595601 1.047721 0.02450895   2.080759
## 4  0.7198130  0.9583738 1.046426 0.03193537   2.021458
## 16 0.6514488  0.9580672 1.046091 0.02870308   2.006677
## 25 0.5610371  0.9571808 1.045123 0.02422287   1.965139
## 15 0.6058948  0.9571069 1.045043 0.02611486   1.961754
```

## Redundância e Interpretação
Redução de redundância evita regras repetitivas ou pouco informativas.  
Slides: 21–24.


``` r
# Removendo regras redundantes
nrules <- rules[!is.redundant(rules)]

arules::inspect(nrules)
```

```
##     lhs                                  rhs                 support   confidence coverage  lift      count
## [1] {hours-per-week=Full-time}        => {capital-gain=None} 0.5445925 0.9278129  0.5869637 1.0130572 16426
## [2] {sex=Male}                        => {capital-gain=None} 0.6101386 0.9029931  0.6756846 0.9859571 18403
## [3] {workclass=Private}               => {capital-gain=None} 0.6824150 0.9235843  0.7388767 1.0084401 20583
## [4] {income=small}                    => {capital-gain=None} 0.7198130 0.9583738  0.7510775 1.0464260 21711
## [5] {race=White}                      => {capital-gain=None} 0.7846960 0.9126595  0.8597905 0.9965116 23668
## [6] {native-country=United-States}    => {capital-gain=None} 0.8336649 0.9142307  0.9118759 0.9982271 25145
## [7] {capital-loss=None}               => {capital-gain=None} 0.8685432 0.9116757  0.9526888 0.9954373 26197
## [8] {workclass=Private, income=small} => {capital-gain=None} 0.5545388 0.9607122  0.5772164 1.0489792 16726
```

## Transações de Suporte
Analisar quais transações suportam regras ajuda a validar padrões.  
Slides: 4–5.


``` r
# Mostrando as transacoes que suportam as regras
st <- supportingTransactions(nrules[1], AdultTrans)
trans <- unique(st@data@i)
length(trans)
```

```
## [1] 16426
```

``` r
print(c(length(trans)/length(AdultTrans), nrules[1]@quality$support))
```

```
## [1] 0.5445925 0.5445925
```


``` r
# Transacoes de suporte para multiplas regras
st <- supportingTransactions(nrules[1:2], AdultTrans)
trans <- unique(st@data@i)
length(trans)
```

```
## [1] 24426
```

``` r
print(c(length(trans)/length(AdultTrans), nrules[1:2]@quality$support))
```

```
## [1] 0.8098269 0.5445925 0.6101386
```

## Visualização de Regras
Visualizações auxiliam a interpretação de padrões descobertos.  
Slides: 21.


``` r
# Visualizacao de regras
options(repr.plot.width=10, repr.plot.height=5)
plot(rules)
```

![plot of chunk unnamed-chunk-15](fig/06-PatternMining/unnamed-chunk-15-1.png)


``` r
options(repr.plot.width=10, repr.plot.height=5)
plot(rules, method="paracoord", control=list(reorder=TRUE))
```

![plot of chunk unnamed-chunk-16](fig/06-PatternMining/unnamed-chunk-16-1.png)

## Padrões Raros e Negativos
Padrões raros podem ser estratégicos; padrões negativos indicam correlação inversa (lift < 1).  
Slides: 39.


``` r
# Padrões raros e negativos
pm_apriori_rare <- pat_apriori(parameter = list(supp = 0.05, conf = 0.6, minlen = 2, maxlen = 3))
pm_apriori_rare <- fit(pm_apriori_rare, AdultTrans)
rare_rules <- discover(pm_apriori_rare, AdultTrans)
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
##         0.6    0.1    1 none FALSE            TRUE       5    0.05      2      3  rules TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 1508 
## 
## set item appearances ...[0 item(s)] done [0.00s].
## set transactions ...[114 item(s), 30162 transaction(s)] done [0.04s].
## sorting and recoding items ... [37 item(s)] done [0.01s].
## creating transaction tree ... done [0.02s].
## checking subsets of size 1 2 3
```

```
##  done [0.14s].
## writing ... [2146 rule(s)] done [0.02s].
## creating S4 object  ... done [0.01s].
```

``` r
neg_rules <- subset(rare_rules, lift < 1)
length(neg_rules)
```

```
## [1] 652
```

``` r
inspect(head(neg_rules))
```

```
##     lhs                               rhs                            support    confidence coverage   lift      count
## [1] {occupation=Machine-op-inspct} => {race=White}                   0.05268218 0.8082401  0.06518135 0.9400431 1589 
## [2] {occupation=Machine-op-inspct} => {native-country=United-States} 0.05593130 0.8580875  0.06518135 0.9410135 1687 
## [3] {workclass=Local-gov}          => {race=White}                   0.05649493 0.8243832  0.06852994 0.9588187 1704 
## [4] {workclass=Local-gov}          => {capital-gain=None}            0.06226378 0.9085631  0.06852994 0.9920389 1878 
## [5] {workclass=Local-gov}          => {capital-loss=None}            0.06435250 0.9390421  0.06852994 0.9856756 1941 
## [6] {workclass=Self-emp-not-inc}   => {income=small}                 0.05918043 0.7142857  0.08285260 0.9510146 1785
```

## Associações Multi-Nível e Multi-Dimensionais
Regras podem incorporar dimensões adicionais, como atributos demográficos, com suportes adequados por nível.  
Slides: 30–35.


``` r
# Associações multi-nível e multi-dimensionais
adult_ml <- myAdultUCI
fg_edu <- feature_generation(
  edu_level = ifelse(
    education %in% c("Bachelors", "Masters", "Doctorate", "Prof-school"),
    "Higher",
    "Lower"
  )
)
adult_ml <- transform(fg_edu, adult_ml)
adult_ml$edu_level <- factor(adult_ml$edu_level)

AdultTransML <- as(adult_ml, "transactions")
income_labels <- paste0("income=", levels(adult_ml$income))

pm_apriori_md <- pat_apriori(
  parameter = list(supp = 0.2, conf = 0.6, minlen = 2, maxlen = 3),
  appearance = list(rhs = income_labels, default = "lhs")
)
pm_apriori_md <- fit(pm_apriori_md, AdultTransML)
rules_md <- discover(pm_apriori_md, AdultTransML)
```

```
## Apriori
## 
## Parameter specification:
##  confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target  ext
##         0.6    0.1    1 none FALSE            TRUE       5     0.2      2      3  rules TRUE
## 
## Algorithmic control:
##  filter tree heap memopt load sort verbose
##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
## 
## Absolute minimum support count: 6032 
## 
## set item appearances ...[2 item(s)] done [0.00s].
## set transactions ...[116 item(s), 30162 transaction(s)] done [0.03s].
## sorting and recoding items ... [21 item(s)] done [0.01s].
## creating transaction tree ... done [0.03s].
## checking subsets of size 1 2 3
```

```
##  done [0.01s].
## writing ... [71 rule(s)] done [0.00s].
## creating S4 object  ... done [0.00s].
```

``` r
inspect(rules_md)
```

```
##      lhs                                                             rhs            support   confidence coverage  lift      count
## [1]  {relationship=Not-in-family}                                 => {income=small} 0.2288641 0.8934766  0.2561501 1.1895930  6903
## [2]  {marital-status=Never-married}                               => {income=small} 0.3068762 0.9516759  0.3224587 1.2670808  9256
## [3]  {sex=Female}                                                 => {income=small} 0.2874478 0.8863218  0.3243154 1.1800670  8670
## [4]  {education=HS-grad}                                          => {income=small} 0.2726278 0.8356707  0.3262383 1.1126291  8223
## [5]  {age=Middle-aged}                                            => {income=small} 0.3816723 0.7306887  0.5223460 0.9728539 11512
## [6]  {hours-per-week=Full-time}                                   => {income=small} 0.4690007 0.7990285  0.5869637 1.0638429 14146
## [7]  {sex=Male}                                                   => {income=small} 0.4636297 0.6861629  0.6756846 0.9135714 13984
## [8]  {workclass=Private}                                          => {income=small} 0.5772164 0.7812079  0.7388767 1.0401163 17410
## [9]  {edu_level=Lower}                                            => {income=small} 0.6231682 0.8326393  0.7484252 1.1085931 18796
## [10] {race=White}                                                 => {income=small} 0.6330482 0.7362820  0.8597905 0.9803009 19094
## [11] {native-country=United-States}                               => {income=small} 0.6799615 0.7456734  0.9118759 0.9928048 20509
## [12] {capital-gain=None}                                          => {income=small} 0.7198130 0.7859470  0.9158544 1.0464260 21711
## [13] {capital-loss=None}                                          => {income=small} 0.7282010 0.7643640  0.9526888 1.0176899 21964
## [14] {relationship=Not-in-family, native-country=United-States}   => {income=small} 0.2106293 0.8910238  0.2363902 1.1863274  6353
## [15] {relationship=Not-in-family, capital-gain=None}              => {income=small} 0.2198462 0.9186755  0.2393077 1.2231434  6631
## [16] {relationship=Not-in-family, capital-loss=None}              => {income=small} 0.2205093 0.8973287  0.2457397 1.1947218  6651
## [17] {workclass=Private, marital-status=Never-married}            => {income=small} 0.2554870 0.9602492  0.2660633 1.2784955  7706
## [18] {marital-status=Never-married, edu_level=Lower}              => {income=small} 0.2432199 0.9812734  0.2478615 1.3064875  7336
## [19] {marital-status=Never-married, race=White}                   => {income=small} 0.2527021 0.9484818  0.2664280 1.2628282  7622
## [20] {marital-status=Never-married, native-country=United-States} => {income=small} 0.2796565 0.9503155  0.2942776 1.2652695  8435
## [21] {marital-status=Never-married, capital-gain=None}            => {income=small} 0.2974935 0.9648387  0.3083350 1.2846060  8973
## [22] {marital-status=Never-married, capital-loss=None}            => {income=small} 0.2987534 0.9544540  0.3130097 1.2707796  9011
## [23] {workclass=Private, sex=Female}                              => {income=small} 0.2294609 0.9056530  0.2533652 1.2058049  6921
## [24] {sex=Female, edu_level=Lower}                                => {income=small} 0.2337710 0.9298431  0.2514091 1.2380121  7051
## [25] {race=White, sex=Female}                                     => {income=small} 0.2295604 0.8770108  0.2617532 1.1676701  6924
## [26] {sex=Female, native-country=United-States}                   => {income=small} 0.2621842 0.8853560  0.2961342 1.1787812  7908
## [27] {sex=Female, capital-gain=None}                              => {income=small} 0.2773026 0.9070600  0.3057158 1.2076782  8364
## [28] {sex=Female, capital-loss=None}                              => {income=small} 0.2794576 0.8924299  0.3131424 1.1881994  8429
## [29] {workclass=Private, education=HS-grad}                       => {income=small} 0.2177243 0.8553009  0.2545587 1.1387651  6567
## [30] {education=HS-grad, edu_level=Lower}                         => {income=small} 0.2726278 0.8356707  0.3262383 1.1126291  8223
## [31] {education=HS-grad, race=White}                              => {income=small} 0.2291957 0.8228782  0.2785293 1.0955969  6913
## [32] {education=HS-grad, native-country=United-States}            => {income=small} 0.2542272 0.8326637  0.3053179 1.1086255  7668
## [33] {education=HS-grad, capital-gain=None}                       => {income=small} 0.2611564 0.8562887  0.3049864 1.1400804  7877
## [34] {education=HS-grad, capital-loss=None}                       => {income=small} 0.2645382 0.8434461  0.3136397 1.1229814  7979
## [35] {marital-status=Married-civ-spouse, edu_level=Lower}         => {income=small} 0.2167628 0.6617409  0.3275645 0.8810554  6538
## [36] {age=Middle-aged, hours-per-week=Full-time}                  => {income=small} 0.2469664 0.7965141  0.3100590 1.0604952  7449
## [37] {age=Middle-aged, sex=Male}                                  => {income=small} 0.2464028 0.6771754  0.3638684 0.9016052  7432
## [38] {age=Middle-aged, workclass=Private}                         => {income=small} 0.2903654 0.7468873  0.3887673 0.9944210  8758
## [39] {age=Middle-aged, edu_level=Lower}                           => {income=small} 0.3052848 0.8178346  0.3732843 1.0888818  9208
## [40] {age=Middle-aged, race=White}                                => {income=small} 0.3158942 0.7122673  0.4435051 0.9483273  9528
## [41] {age=Middle-aged, native-country=United-States}              => {income=small} 0.3434454 0.7244563  0.4740733 0.9645559 10359
## [42] {age=Middle-aged, capital-gain=None}                         => {income=small} 0.3652278 0.7639920  0.4780519 1.0171946 11016
## [43] {age=Middle-aged, capital-loss=None}                         => {income=small} 0.3694384 0.7447036  0.4960878 0.9915136 11143
## [44] {sex=Male, hours-per-week=Full-time}                         => {income=small} 0.2792255 0.7431395  0.3757377 0.9894312  8422
## [45] {workclass=Private, hours-per-week=Full-time}                => {income=small} 0.3701015 0.8276859  0.4471520 1.1019980 11163
## [46] {hours-per-week=Full-time, edu_level=Lower}                  => {income=small} 0.3952656 0.8549914  0.4623036 1.1383531 11922
## [47] {race=White, hours-per-week=Full-time}                       => {income=small} 0.3823354 0.7860405  0.4864067 1.0465504 11532
## [48] {hours-per-week=Full-time, native-country=United-States}     => {income=small} 0.4194351 0.7942617  0.5280817 1.0574963 12651
## [49] {capital-gain=None, hours-per-week=Full-time}                => {income=small} 0.4503680 0.8269816  0.5445925 1.1010603 13584
## [50] {capital-loss=None, hours-per-week=Full-time}                => {income=small} 0.4551091 0.8088981  0.5626285 1.0769835 13727
## [51] {workclass=Private, sex=Male}                                => {income=small} 0.3477555 0.7162660  0.4855116 0.9536513 10489
## [52] {sex=Male, edu_level=Lower}                                  => {income=small} 0.3893973 0.7834701  0.4970161 1.0431281 11745
## [53] {race=White, sex=Male}                                       => {income=small} 0.4034878 0.6746868  0.5980373 0.8982918 12170
## [54] {sex=Male, native-country=United-States}                     => {income=small} 0.4177773 0.6784945  0.6157417 0.9033615 12601
## [55] {sex=Male, capital-gain=None}                                => {income=small} 0.4425104 0.7252622  0.6101386 0.9656289 13347
## [56] {sex=Male, capital-loss=None}                                => {income=small} 0.4487435 0.7016589  0.6395464 0.9342030 13535
## [57] {workclass=Private, edu_level=Lower}                         => {income=small} 0.4955905 0.8517864  0.5818248 1.1340859 14948
## [58] {workclass=Private, race=White}                              => {income=small} 0.4863736 0.7665778  0.6344738 1.0206374 14670
## [59] {workclass=Private, native-country=United-States}            => {income=small} 0.5170082 0.7744723  0.6675618 1.0311483 15594
## [60] {workclass=Private, capital-gain=None}                       => {income=small} 0.5545388 0.8126123  0.6824150 1.0819287 16726
## [61] {workclass=Private, capital-loss=None}                       => {income=small} 0.5607718 0.7931164  0.7070486 1.0559714 16914
## [62] {race=White, edu_level=Lower}                                => {income=small} 0.5225449 0.8207572  0.6366620 1.0927729 15761
## [63] {native-country=United-States, edu_level=Lower}              => {income=small} 0.5647835 0.8269417  0.6829786 1.1010072 17035
## [64] {capital-gain=None, edu_level=Lower}                         => {income=small} 0.5980373 0.8548005  0.6996220 1.1380989 18038
## [65] {capital-loss=None, edu_level=Lower}                         => {income=small} 0.6052317 0.8404696  0.7201114 1.1190185 18255
## [66] {race=White, native-country=United-States}                   => {income=small} 0.5861349 0.7299942  0.8029308 0.9719293 17679
## [67] {race=White, capital-gain=None}                              => {income=small} 0.6058948 0.7721396  0.7846960 1.0280425 18275
## [68] {race=White, capital-loss=None}                              => {income=small} 0.6129899 0.7500304  0.8172867 0.9986059 18489
## [69] {capital-gain=None, native-country=United-States}            => {income=small} 0.6514488 0.7814277  0.8336649 1.0404089 19649
## [70] {capital-loss=None, native-country=United-States}            => {income=small} 0.6587428 0.7590831  0.8678138 1.0106588 19869
## [71] {capital-gain=None, capital-loss=None}                       => {income=small} 0.6969365 0.8024201  0.8685432 1.0683586 21021
```

## Mineração de Sequências
Padrões sequenciais estendem itemsets para ordem temporal. SPADE é uma abordagem vertical eficiente.  
Slides: 46–62.

Diferença-chave: em mineração sequencial, a ordem dos eventos influencia o padrão descoberto.


``` r
# Sequence Mining
x <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
as(x, "data.frame")
```

```
##        items sequenceID eventID SIZE
## 1      {C,D}          1      10    2
## 2    {A,B,C}          1      15    3
## 3    {A,B,F}          1      20    3
## 4  {A,C,D,F}          1      25    4
## 5    {A,B,F}          2      15    3
## 6        {E}          2      20    1
## 7    {A,B,F}          3      10    3
## 8    {D,G,H}          4      10    3
## 9      {B,F}          4      20    2
## 10   {A,G,H}          4      25    3
```


``` r
pm_cspade <- pat_cspade(parameter = list(support = 0.4), control = list(verbose = TRUE))
pm_cspade <- fit(pm_cspade, x)
s1 <- discover(pm_cspade, x)
```

```
## 
## parameter specification:
## support : 0.4
## maxsize :  10
## maxlen  :  10
## 
## algorithmic control:
## bfstype  : FALSE
## verbose  :  TRUE
## summary  : FALSE
## tidLists : FALSE
## 
## preprocessing ... 1 partition(s), 0 MB [0.18s]
## mining transactions ... 0 MB [0.9s]
## reading sequences ... [0.19s]
## 
## total elapsed time: 1.27s
```

``` r
as(s1, "data.frame")
```

```
##           sequence support
## 1            <{A}>    1.00
## 2            <{B}>    1.00
## 3            <{D}>    0.50
## 4            <{F}>    1.00
## 5          <{A,F}>    0.75
## 6          <{B,F}>    1.00
## 7        <{D},{F}>    0.50
## 8      <{D},{B,F}>    0.50
## 9        <{A,B,F}>    0.75
## 10         <{A,B}>    0.75
## 11       <{D},{B}>    0.50
## 12       <{B},{A}>    0.50
## 13       <{D},{A}>    0.50
## 14       <{F},{A}>    0.50
## 15   <{D},{F},{A}>    0.50
## 16     <{B,F},{A}>    0.50
## 17 <{D},{B,F},{A}>    0.50
## 18   <{D},{B},{A}>    0.50
```

## Graph Pattern Mining (Exemplo)
Em grafos, padrões frequentes são subgrafos recorrentes. Aqui mostramos contagem de subgrafos como ilustração.  
Slides: 63–64.


``` r
# Mineracao de padroes em grafos (exemplo simples)

g1 <- make_ring(4)
g2 <- make_full_graph(3)
g3 <- make_star(4, mode = "undirected")
tri <- make_full_graph(3)

count_subgraph_isomorphisms(tri, g1)
```

```
## [1] 0
```

``` r
count_subgraph_isomorphisms(tri, g2)
```

```
## [1] 6
```

``` r
count_subgraph_isomorphisms(tri, g3)
```

```
## [1] 0
```

## Referências
- Han, J., Pei, J., & Tong, H. (2022). *Data Mining: Concepts and Techniques* (4th ed.). Morgan Kaufmann.
- Agrawal, R., & Srikant, R. (1994). Fast algorithms for mining association rules. *VLDB*.
- Zaki, M. (2001). SPADE: An efficient algorithm for mining frequent sequences. *Machine Learning*, 42(1), 31–60.
- Geng, L., & Hamilton, H. (2006). Interestingness measures for data mining: A survey. *ACM Computing Surveys*, 38(3).
- McGarry, K. (2005). A survey of interestingness measures for knowledge discovery. *Knowledge Engineering Review*, 20(1).
- Borgelt, C. (2005). An implementation of the FP-growth algorithm. *ACM SIGKDD Explorations*.
- Yan, X., & Han, J. (2002). gSpan: Graph-based substructure pattern mining. *ICDM*.


