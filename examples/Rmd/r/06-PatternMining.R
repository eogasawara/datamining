knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Slides 1–3: contexto
# DAL ToolBox
# version 1.0.77

library(daltoolbox)

library(arules)
library(arulesViz)
library(arulesSequences)
library(igraph)

# Slides 2–4: exemplo transacional
data(AdultUCI)
myAdultUCI <- as.data.frame(AdultUCI)
dim(myAdultUCI)
head(myAdultUCI)

# Limpeza básica e remoção de atributos
na_obj <- na_removal()
myAdultUCI <- transform(na_obj, myAdultUCI)

myAdultUCI$fnlwgt <- NULL
myAdultUCI$"education-num" <- NULL

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

# Convert to transactions
AdultTrans <- as(myAdultUCI, "transactions")

# A Priori
pm_apriori <- pat_apriori(
  parameter = list(supp = 0.5, conf = 0.9, minlen = 2, maxlen = 10, target = "rules"),
  appearance = list(rhs = c("capital-gain=None"), default = "lhs"),
  control = NULL
)
pm_apriori <- fit(pm_apriori, AdultTrans)
rules <- discover(pm_apriori, AdultTrans)
inspect(rules)

rules_a <- as(rules, "data.frame")
head(rules_a)

# ECLAT (itemsets frequentes)
pm_eclat <- pat_eclat(parameter = list(supp = 0.5, maxlen = 3))
pm_eclat <- fit(pm_eclat, AdultTrans)
itemsets_eclat <- discover(pm_eclat, AdultTrans)
inspect(head(sort(itemsets_eclat, by = "support")))

# Padrões fechados e max-padrões
closed_sets <- itemsets_eclat[is.closed(itemsets_eclat)]
max_sets <- itemsets_eclat[is.maximal(itemsets_eclat)]

inspect(head(sort(closed_sets, by = "support")))
inspect(head(sort(max_sets, by = "support")))

# Analise das regras
imrules <- interestMeasure(rules, transactions = AdultTrans)
head(imrules)

imrules2 <- interestMeasure(rules, c("support", "confidence", "lift", "leverage", "conviction"), AdultTrans)
head(imrules2[order(imrules2[, "lift"], decreasing = TRUE), ])

# Removendo regras redundantes
nrules <- rules[!is.redundant(rules)]

arules::inspect(nrules)

# Mostrando as transacoes que suportam as regras
st <- supportingTransactions(nrules[1], AdultTrans)
trans <- unique(st@data@i)
length(trans)
print(c(length(trans)/length(AdultTrans), nrules[1]@quality$support))

# Transacoes de suporte para multiplas regras
st <- supportingTransactions(nrules[1:2], AdultTrans)
trans <- unique(st@data@i)
length(trans)
print(c(length(trans)/length(AdultTrans), nrules[1:2]@quality$support))

# Visualizacao de regras
options(repr.plot.width=10, repr.plot.height=5)
plot(rules)

options(repr.plot.width=10, repr.plot.height=5)
plot(rules, method="paracoord", control=list(reorder=TRUE))

# Padrões raros e negativos
pm_apriori_rare <- pat_apriori(parameter = list(supp = 0.05, conf = 0.6, minlen = 2, maxlen = 3))
pm_apriori_rare <- fit(pm_apriori_rare, AdultTrans)
rare_rules <- discover(pm_apriori_rare, AdultTrans)
neg_rules <- subset(rare_rules, lift < 1)
length(neg_rules)
inspect(head(neg_rules))

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
inspect(rules_md)

# Sequence Mining
x <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
as(x, "data.frame")

pm_cspade <- pat_cspade(parameter = list(support = 0.4), control = list(verbose = TRUE))
pm_cspade <- fit(pm_cspade, x)
s1 <- discover(pm_cspade, x)
as(s1, "data.frame")

# Mineracao de padroes em grafos (exemplo simples)

g1 <- make_ring(4)
g2 <- make_full_graph(3)
g3 <- make_star(4, mode = "undirected")
tri <- make_full_graph(3)

count_subgraph_isomorphisms(tri, g1)
count_subgraph_isomorphisms(tri, g2)
count_subgraph_isomorphisms(tri, g3)
