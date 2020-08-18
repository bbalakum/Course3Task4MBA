#---------------------------------------------------------------
# UT-DA: Course3 Task 4 - Market Basket Analysis
# Bala Balakumar, July 31st, 2020
# --------------------------------------------------------------
library(arules)
library(arulesViz)
#install.packages("lazyeval")
library(arulesViz)
basket <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep=",", rm.duplicates=TRUE)
sink("./Course3Task4ConsoleOutput.txt", append=TRUE)
inspect(basket)
length(basket)
size(basket)
LIST(basket)
itemLabels(basket)
summary(basket)
# plot frequencies of frequent items in the dataset
itemFrequencyPlot(basket, support=0.10, cex.names=0.6, type="absolute")
# Mine association rules using Apriori algorithm implemented in arules.
rules <- apriori(basket, parameter = list(support= 0.01 , confidence= 0.2, minlen=1))
#summary of rules
summary(rules)
# Inspect rules
#inspect(rules)
#inspect top 25 rules by highest lift
inspect(head(sort(rules, by ="lift"),25))
# Visualization of rules
#Plotting rules
plot(rules)
# Interactive plots for rules
sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=FALSE)
rulesminlen2 <- apriori(basket, parameter = list(support= 0.015 , confidence= 0.30, minlen=2))
inspect(rules)
inspect(rulesminlen2)
inspect(head(sort(rules, by ="lift", decreasing = TRUE),30))
inspect(head(sort(rulesminlen2, by ="lift", decreasing = TRUE),20))
summary(rulesminlen2)
is.redundant(rulesminlen2)
sink()            
write.csv(inspect(head(sort(rules, by ="lift", decreasing = TRUE),30)), "Top30AssociationsNoMinLenWithHighLift.csv")
write.csv(inspect(head(sort(rules, by ="confidence", decreasing = TRUE),30)), "Top30AssociationsNoMinLenWithHighConfidence.csv")
write.csv(inspect(head(sort(rulesminlen2, by ="lift", decreasing = TRUE),30)), "Top30AssociationMinLen2WithHighLift.csv")
write.csv(inspect(head(sort(rulesminlen2, by ="confidence", decreasing = TRUE),30)), "Top30AssociationMinLen2WithHighConfidence.csv")
rulesLaptop <- subset(rules, subset=rhs %ain% "iMac"|rhs %ain% "HP Laptop")
inspect(rulesLaptop)
write.csv(inspect(head(sort(rulesLaptop, by ="lift", decreasing = TRUE),30)), "Top30AssociationsRuleLaptopWithHighLift.csv")
rulesMonitor <- subset(rules, subset=rhs %ain% "ViewSonic Monitor"|rhs %ain% "ASUS Monitor"|rhs %ain% "ASUS 2 Monitor")
inspect(rulesMonitor)
write.csv(inspect(head(sort(rulesMonitor, by ="lift", decreasing = TRUE),30)), "Top30AssociationsRuleMonitorWithHighLift.csv")
