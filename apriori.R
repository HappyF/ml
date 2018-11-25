setwd('F:/mtpa/Chapter_4')
library(arules)  # association rules
library(arulesViz)  # data visualization of association rules
library(RColorBrewer)  # color palettes for plots
library(cluster)  # cluster analysis for market segmentation


data(Groceries)  # grocery transcations object from arules package

######### data.frame 2 json ######
data(iris)
iris
library(plyr)
library(jsonlite)
m<-toJSON(unname(alply(iris[1:4,1:4],1,identity)))



#show the dimensions of the transactions object
print(dim(Groceries))
print(dim(Groceries)[1]) #9835个市场篮子
print(dim(Groceries)[2]) #169个杂货物品

# examine frequency for each item with support greater than 0.025
#pdf(file="fig_market_basket_initial_item_support1.pdf", 
 #   width = 8.5, height = 11)
itemFrequencyPlot(Groceries, support = 0.025, cex.names=0.2, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
#dev.off()  
# explore possibilities for combining similar items
print(head(itemInfo(Groceries))) 
print(levels(itemInfo(Groceries)[["level1"]]))  # 10 levels... too few 
print(levels(itemInfo(Groceries)[["level2"]]))  # 55 distinct levels

groceries <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])  


pdf(file="fig_market_basket_final_item_support3.pdf", width = 8.5, height = 11)
itemFrequencyPlot(groceries, support = 0.025, cex.names=1.0, xlim = c(0,0.5),
                  type = "relative", horiz = TRUE, col = "blue", las = 1,
                  xlab = paste("Proportion of Market Baskets Containing Item",
                               "\n(Item Relative Frequency or Support)"))
dev.off()   

# this is done by setting very low criteria for support and confidence
first.rules <- apriori(groceries, 
                       parameter = list(support = 0.001, confidence = 0.05))
print(summary(first.rules))  # yields 69,921 rules... too many

second.rules <- apriori(groceries, 
                        parameter = list(support = 0.025, confidence = 0.05))
print(summary(second.rules))  # yields 344 rules

dev.off()
plot(second.rules, 
     control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")   

plot(second.rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))

vegie.rules <- subset(second.rules, subset = rhs %pin% "vegetables")
inspect(vegie.rules)  # 41 rules
inspect(second.rules)

plot(vegie.rules,method = 'graph')
