# DAL ToolBox
# version 1.0.77


library(daltoolbox)

library(arules)
library(arulesViz)
library(arulesSequences)

data(AdultUCI)
myAdultUCI <- as.data.frame(AdultUCI)
dim(myAdultUCI)
head(myAdultUCI)

myAdultUCI$fnlwgt <- NULL
myAdultUCI$"education-num" <- NULL

print(0)

myAdultUCI$age <- ordered(cut(myAdultUCI$age, c(15,25,45,65,100)),
                              labels = c("Young", "Middle-aged", "Senior", "Old"))

print(1)

myAdultUCI$"hours-per-week" <- ordered(cut(myAdultUCI$"hours-per-week",
                                             c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

print(2)

myAdultUCI$"capital-gain" <- ordered(cut(myAdultUCI$"capital-gain",
                                           c(-Inf,0,median(myAdultUCI$"capital-gain"[myAdultUCI$"capital-gain">0]),
                                             Inf)), labels = c("None", "Low", "High"))

print(3)

myAdultUCI$"capital-loss" <- ordered(cut(myAdultUCI$"capital-loss",
                                           c(-Inf,0, median(myAdultUCI$"capital-loss"[myAdultUCI$"capital-loss">0]),
                                             Inf)), labels = c("None", "Low", "High"))

print(4)

head(myAdultUCI)

AdultTrans <- as(myAdultUCI, "transactions")

rules <- apriori(AdultTrans, parameter=list(supp = 0.5, conf = 0.9, minlen=2, maxlen= 10, target = "rules"), 
                 appearance=list(rhs = c("capital-gain=None"), default="lhs"), control=NULL)
inspect(rules)

rules_a <- as(rules, "data.frame")
head(rules_a)

imrules <- interestMeasure(rules, transactions = AdultTrans)
head(imrules)

nrules <- rules[!is.redundant(rules)]

arules::inspect(nrules)

st <- supportingTransactions(nrules[1], AdultTrans)
trans <- unique(st@data@i)
length(trans)
print(c(length(trans)/length(AdultTrans), nrules[1]@quality$support))

st <- supportingTransactions(nrules[1:2], AdultTrans)
trans <- unique(st@data@i)
length(trans)
print(c(length(trans)/length(AdultTrans), nrules[1:2]@quality$support))

options(repr.plot.width=10, repr.plot.height=5)
plot(rules)

options(repr.plot.width=10, repr.plot.height=5)
plot(rules, method="paracoord", control=list(reorder=TRUE))

x <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
as(x, "data.frame")

s1 <- cspade(x, parameter = list(support = 0.4), control = list(verbose = TRUE))
as(s1, "data.frame")
