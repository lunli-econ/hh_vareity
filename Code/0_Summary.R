## Goal: look at household level variety by income level, product variety, etc

packages <- c("reshape", "lubridate", "data.table", "plyr", "zoo", "bit64",
              "ggplot2", "xtable", "foreign", "dplyr", "foreach","doParallel",
              "readstata13", "stringr","plm", "blsAPI", "rjson")
lapply(packages, require, character.only = TRUE)

## 1. Look at cross-sectional facts by households, using latest data.
panelist <- fread("/project/databases/nielsen/nielsen_extracts/HMS/2016/Annual_Files/panelists_2016.tsv")
purchases <- fread("/project/databases/nielsen/nielsen_extracts/HMS/2016/Annual_Files/purchases_2016.tsv")
trips <- fread("/project/databases/nielsen/nielsen_extracts/HMS/2016/Annual_Files/trips_2016.tsv")

trips[, ym:= as.yearmon(purchase_date, format = "%Y-%m-%d")]
combined <- merge(purchases, trips, by = "trip_code_uc")

## 1.1 Distribution of household
### A. What's the overall distribution of things?
sum_1 <- combined[,.(nupc = length(unique(upc)), 
                     nstore = length(unique(store_code_uc)),
                     ntrips = length(unique(trip_code_uc)),
                     exp = sum(total_price_paid),
                     quant = sum(quantity)), by = .(household_code)]

pdf("/home/lunl/Research/hh_variety/0_Summary_1.pdf", width = 16, height = 8)
ggplot(data = sum_1, aes(nupc)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Number of unique upcs bought")
ggplot(data = sum_1, aes(nstore)) + geom_bar(color = "black", fill = "white") + ggtitle("Number of stores visited")
ggplot(data = sum_1, aes(ntrips)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Number of shopping trips made")
ggplot(data = sum_1, aes(exp)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Total expenditure")
ggplot(data = sum_1, aes(quant)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Total quantity (units) of goods purchased")
ggplot(data = sum_1, aes(nupc/nstore)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Average number of unique upcs bought, per store")
ggplot(data = sum_1, aes(nupc/ntrips)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Average number of unique upcs bought, per visit")
ggplot(data = sum_1, aes(nupc/quant)) + geom_histogram(bins = 100, color = "black", fill = "white") + ggtitle("Ratio of unique upc/total quantity")
dev.off()

### B. What's the distribution by income levels? (Per Household)
# combined_2 <- merge(combined, panelist, by.x = "household_code", by.y = "Household_Cd")
# 

sum_1_merged <- merge(sum_1, panelist, by.x = "household_code", by.y = "Household_Cd")
sum_1B <- sum_1_merged[,.(AHnupc = mean(nupc), 
                   APnupc = mean(nupc/Household_Size),
                   AHnstore = mean(nstore),
                   AHntrips = mean(ntrips),
                   AHexp =  mean(exp),
                   AHquant = mean(quant)), by = .(Household_Income)]  %>% arrange(Household_Income)

pdf("/home/lunl/Research/hh_variety/0_Summary_2.pdf", width = 16, height = 8)
ggplot(data = sum_1B, aes(Household_Income, AHnupc)) + geom_line()
ggplot(data = sum_1B, aes(Household_Income, APnupc)) + geom_line()
ggplot(data = sum_1B, aes(Household_Income, AHnstore)) + geom_line()
ggplot(data = sum_1B, aes(Household_Income, AHntrips)) + geom_line()
ggplot(data = sum_1B, aes(Household_Income, AHexp)) + geom_line()
ggplot(data = sum_1B, aes(Household_Income, AHquant)) + geom_line()
dev.off()



### C. Distribution by education? Employment Status? Where they live? Family Size?
### D. Could there by unexplained variety differences after controlling for everything? 



