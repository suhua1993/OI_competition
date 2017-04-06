# OI = read.csv("./OI.csv", na.strings = "#N/A")
# library(dplyr)
# #subset(OI, Production.Category == "BEER" & Ship.to.Country.Name == "Colombia")
# a = levels(OI$Production.Category)
# b = levels(OI$Ship.to.Country.Name)
# LIST = list(BEER = list(), `Drug & Chemical` = list(), Food = list(), Miscellaneous = list(), NAB = list(), `RTD/FAB` = list(), SPIRITS = list(), WINE = list())
# for(i in 1:length(a)){
#   for(j in 1:length(b)){
#     LIST[[i]][[j]] = droplevels(subset(OI, Production.Category == a[i] & Ship.to.Country.Name == b[j], select = c(Week.ID:Item.ID, Customer.Name:Shipment.Destination, Pieces:Tonnes)))
#   }
# }
# ## Beer-Columbia
# for(i in 1:length(a)){
#   for(j in 1:length(b)){
#     if(prod(dim(LIST[[i]][[j]])) == 0){
#       plot(1:2, main = paste("Product ", a[i], " and Country ", b[j], sep = ""))
#     }
#     else{hist(LIST[[i]][[j]]$Net.Weight, main = paste("Product ", a[i], " and Country ", b[j], sep = ""))}
#   }
# }

## 4/1 - real work started today
OI = read.csv("./OI.csv", na.strings = "#N/A")
library(dplyr)

levels(OI$Ship.to.Country.Name) = c("Colombia", "Comoros", "Congo", 
                                    "Croatia", "Cuba", "Cyprus", "Czech Republic", "DRC", 
                                    "Denmark", "Djibouti", "Dominica", "Dominican Republic", "English English Name")
OI %>% subset(Ship.to.Country.Name %in% c("Comoros", "Congo", "DRC", "Djibouti")) -> Africa
Africa = droplevels(Africa)
table(Africa$Production.Category)
#levels(Africa$Production.Category) = c("ALCOHOLIC", "Drug & Chemical", "Food", "Miscellaneous", "NAB", "Miscellaneous", "ALCOHOLIC", "ALCOHOLIC")
#setdiff(1:105, unique(subset(Africa, Production.Category == "ALCOHOLIC")$Week.ID))
#setdiff(1:105, unique(subset(Africa, Production.Category == "NAB")$Week.ID))
#setdiff(1:105, unique(subset(Africa, Production.Category == "Food")$Week.ID))
#setdiff(1:105, unique(subset(Africa, Production.Category == "Miscellaneous")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "BEER")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "Food")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "Miscellaneous")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "NAB")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "RTD/FAB")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "SPIRITS")$Week.ID))
setdiff(1:105, unique(subset(Africa, Production.Category == "WINE")$Week.ID))

OI %>% subset(Ship.to.Country.Name %in% c("Colombia", "Cuba", "Dominica", "Dominican Republic")) -> Latin.America
Latin.America = droplevels(Latin.America)
table(Latin.America$Production.Category)
setdiff(1:105, unique(subset(Latin.America, Production.Category == "BEER")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "Drug & Chemical")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "Food")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "Miscellaneous")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "NAB")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "RTD/FAB")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "SPIRITS")$Week.ID))
setdiff(1:105, unique(subset(Latin.America, Production.Category == "WINE")$Week.ID))

OI %>% subset(Ship.to.Country.Name %in% c("Croatia", "Denmark", "Cyprus", "Czech Republic")) -> Europe
Europe = droplevels(Europe)
table(Europe$Production.Category)
setdiff(1:105, unique(subset(Europe, Production.Category %in% c("Food", "Miscellaneous"))$Week.ID))
Europe = droplevels(Europe)
levels(Europe$Production.Category) = c("Miscellaneous", "Miscellaneous")

## Africa not full weeks: RTD/FAB (77 missing)
## Europe not full weeks: only have Miscellaneous, but has all weeks
## Latin.America not full weeks: Drug & Chemical (50 missing), Miscellaneous (85 missing), RTD/FAB (53 missing)

## Africa RTD/FAB
unique(subset(Africa, Production.Category == "RTD/FAB")$Week.ID)
dotchart(unique(subset(Africa, Production.Category == "RTD/FAB")$Week.ID), pch = 16, xlim = c(53, 105))
abline(v = seq(60, 105, by = 6) + .5, col = "red")
abline(v = seq(60, 105, by = 5) + .5, col = "red")
abline(v = seq(60, 105, by = 4) + .5, col = "red")
abline(v = seq(60, 105, by = 3) + .5, col = "red")

Africa %>% subset(Production.Category == "RTD/FAB") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_RTD_FAB
plot(total ~ Week.ID, Africa_RTD_FAB, pch = 16)
empty = setdiff(61:105, unique(Africa_RTD_FAB$Week.ID))
zeros = rep(0, length(empty))
Africa_RTD_FAB = rbind(Africa_RTD_FAB, data.frame(Week.ID = empty, total = zeros))
Africa_RTD_FAB = Africa_RTD_FAB[order(Africa_RTD_FAB$Week.ID),]
plot(total ~ Week.ID, Africa_RTD_FAB, pch = 16, type = "o")
acf(ts(Africa_RTD_FAB$total))
pacf(ts(Africa_RTD_FAB$total)) # Africa RTD/FAB not forecastable

## Latin.America Drug & Chemical
dotchart(unique(subset(Latin.America, Production.Category == "Drug & Chemical")$Week.ID), pch = 16, xlim = c(1, 105))
abline(v = seq(0, 105, by = 4) + .5, col = "red") # monthly averaging seems to work: total by weeks, then group the weeks 1:4, 5:8, 8:12, etc. and average the totals that exist

## Latin.America Miscellaneous
dotchart(unique(subset(Latin.America, Production.Category == "Miscellaneous")$Week.ID), pch = 16, xlim = c(1, 105)) # not forecastable

## Latin.America RTD/FAB
dotchart(unique(subset(Latin.America, Production.Category == "RTD/FAB")$Week.ID), pch = 16, xlim = c(1, 105))
abline(v = seq(0, 105, by = 5) + .5, col = "red") # group by weeks 1:5, 6:10, etc.

## Latin.America Drug & Chemical: total every week, group by 4 weeks at a time, average the totals
seq = seq(0, 105, by = 4) + .5
Latin.America %>% subset(Production.Category == "Drug & Chemical") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA.D_C
LA.D_C$Month = findInterval(LA.D_C$Week.ID, seq)
LA.D_C %>% group_by(Month) %>% summarise(average = mean(total)) -> LA.D_C_ready

## Latin.America RTD/FAB: total every week, group by 5 weeks at a time, average the totals
seq = seq(0, 105, by = 5) + .5
Latin.America %>% subset(Production.Category == "RTD/FAB") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA.RTD
LA.RTD$five_week = findInterval(LA.RTD$Week.ID, seq)
LA.RTD %>% group_by(five_week) %>% summarise(average = mean(total)) -> LA.RTD_ready
plot(ts(LA.D_C$total))
plot(ts(LA.RTD$total))

## segmented by 4 weeks, 5 weeks at a time: Latin.America Drug & Chemical, Latin.America RTD/FAB respectively
## not forecastable: Africa RTD/FAB, Latin.America Miscellaneous
## everything else: total by each week and forecast

## 4/2

## Africa (RTD/FAB is not forecastable)
rm(Africa_RTD_FAB)
table(Africa$Production.Category)

Africa %>% subset(Production.Category == "BEER") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_Beer
plot(Africa_Beer$total, pch = 16, type = "o")
acf(Africa_Beer$total, lag.max = length(Africa_Beer$total) - 1)
pacf(Africa_Beer$total, lag.max = length(Africa_Beer$total) - 1)

Africa %>% subset(Production.Category == "Food") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_Food
plot(Africa_Food$total, pch = 16, type = "o")
acf(Africa_Food$total, lag.max = length(Africa_Food$total) - 1)
pacf(Africa_Food$total, lag.max = length(Africa_Food$total) - 1)

Africa %>% subset(Production.Category == "Miscellaneous") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_Misc
plot(Africa_Misc$total, pch = 16, type = "o")
acf(Africa_Misc$total, lag.max = length(Africa_Misc$total) - 1)
pacf(Africa_Misc$total, lag.max = length(Africa_Misc$total) - 1)

Africa %>% subset(Production.Category == "NAB") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_NAB
plot(Africa_NAB$total, pch = 16, type = "o")
acf(Africa_NAB$total, lag.max = length(Africa_NAB$total) - 1)
pacf(Africa_NAB$total, lag.max = length(Africa_NAB$total) - 1)

Africa %>% subset(Production.Category == "SPIRITS") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_Spirits
plot(Africa_Spirits$total, pch = 16, type = "o")
acf(Africa_Spirits$total, lag.max = length(Africa_Spirits$total) - 1)
pacf(Africa_Spirits$total, lag.max = length(Africa_Spirits$total) - 1)

Africa %>% subset(Production.Category == "WINE") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Africa_Wine
plot(Africa_Wine$total, pch = 16, type = "o")
acf(Africa_Wine$total, lag.max = length(Africa_Wine$total) - 1)
pacf(Africa_Wine$total, lag.max = length(Africa_Wine$total) - 1)

## weekly averaging:
# Africa %>% subset(Production.Category == "BEER") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> Africa_Beer
# plot(Africa_Beer$average, pch = 16, type = "o")
# acf(Africa_Beer$average, lag.max = length(Africa_Beer$average) - 1)
# pacf(Africa_Beer$average, lag.max = length(Africa_Beer$average) - 1)
# 
# Africa %>% subset(Production.Category == "Food") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> Africa_Food
# plot(Africa_Food$average, pch = 16, type = "o")
# acf(Africa_Food$average, lag.max = length(Africa_Food$average) - 1)
# pacf(Africa_Food$average, lag.max = length(Africa_Food$average) - 1)
# 
# Africa %>% subset(Production.Category == "Miscellaneous") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> Africa_Misc
# plot(Africa_Misc$average, pch = 16, type = "o")
# acf(Africa_Misc$average, lag.max = length(Africa_Misc$average) - 1)
# pacf(Africa_Misc$average, lag.max = length(Africa_Misc$average) - 1)
# 
# Africa %>% subset(Production.Category == "NAB") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> Africa_NAB
# plot(Africa_NAB$average, pch = 16, type = "o")
# acf(Africa_NAB$average, lag.max = length(Africa_NAB$average) - 1)
# pacf(Africa_NAB$average, lag.max = length(Africa_NAB$average) - 1)
# 
# Africa %>% subset(Production.Category == "SPIRITS") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> Africa_Spirits
# plot(Africa_Spirits$average, pch = 16, type = "o")
# acf(Africa_Spirits$average, lag.max = length(Africa_Spirits$average) - 1)
# pacf(Africa_Spirits$average, lag.max = length(Africa_Spirits$average) - 1)
# 
# Africa %>% subset(Production.Category == "WINE") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> Africa_Wine
# plot(Africa_Wine$average, pch = 16, type = "o")
# acf(Africa_Wine$average, lag.max = length(Africa_Wine$average) - 1)
# pacf(Africa_Wine$average, lag.max = length(Africa_Wine$average) - 1)

## Europe
dotchart(unique(subset(Europe, Production.Category %in% c("Food", "Miscellaneous"))$Week.ID), pch = 16) # NOT forecastable, actually

## Latin.America
table(Latin.America$Production.Category) # Drug & Chemical, RTD you already took care of
rm(LA.D_C, LA.RTD)

Latin.America %>% subset(Production.Category == "BEER") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA_Beer
plot(LA_Beer$total, pch = 16, type = "o")
acf(LA_Beer$total, lag.max = length(LA_Beer$total) - 1)
pacf(LA_Beer$total, lag.max = length(LA_Beer$total) - 1)

Latin.America %>% subset(Production.Category == "Food") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA_Food
plot(LA_Food$total, pch = 16, type = "o")
acf(LA_Food$total, lag.max = length(LA_Food$total) - 1)
pacf(LA_Food$total, lag.max = length(LA_Food$total) - 1)

Latin.America %>% subset(Production.Category == "NAB") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA_NAB
plot(LA_NAB$total, pch = 16, type = "o")
acf(LA_NAB$total, lag.max = length(LA_NAB$total) - 1)
pacf(LA_NAB$total, lag.max = length(LA_NAB$total) - 1)

Latin.America %>% subset(Production.Category == "SPIRITS") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA_Spirits
plot(LA_Spirits$total, pch = 16, type = "o")
acf(LA_Spirits$total, lag.max = length(LA_Spirits$total) - 1)
pacf(LA_Spirits$total, lag.max = length(LA_Spirits$total) - 1)

Latin.America %>% subset(Production.Category == "WINE") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> LA_Wine
plot(LA_Wine$total, pch = 16, type = "o")
acf(LA_Wine$total, lag.max = length(LA_Wine$total) - 1)
pacf(LA_Wine$total, lag.max = length(LA_Wine$total) - 1)

## weekly averaging:
# Latin.America %>% subset(Production.Category == "BEER") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> LA_Beer
# plot(LA_Beer$average, pch = 16, type = "o")
# acf(LA_Beer$average, lag.max = length(LA_Beer$average) - 1)
# pacf(LA_Beer$average, lag.max = length(LA_Beer$average) - 1)
# 
# Latin.America %>% subset(Production.Category == "Food") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> LA_Food
# plot(LA_Food$average, pch = 16, type = "o")
# acf(LA_Food$average, lag.max = length(LA_Food$average) - 1)
# pacf(LA_Food$average, lag.max = length(LA_Food$average) - 1)
# 
# Latin.America %>% subset(Production.Category == "NAB") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> LA_NAB
# plot(LA_NAB$average, pch = 16, type = "o")
# acf(LA_NAB$average, lag.max = length(LA_NAB$average) - 1)
# pacf(LA_NAB$average, lag.max = length(LA_NAB$average) - 1)
# 
# Latin.America %>% subset(Production.Category == "SPIRITS") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> LA_Spirits
# plot(LA_Spirits$average, pch = 16, type = "o")
# acf(LA_Spirits$average, lag.max = length(LA_Spirits$average) - 1)
# pacf(LA_Spirits$average, lag.max = length(LA_Spirits$average) - 1)
# 
# Latin.America %>% subset(Production.Category == "WINE") %>% group_by(Week.ID) %>% summarise(average = mean(Pieces)) -> LA_Wine
# plot(LA_Wine$average, pch = 16, type = "o")
# acf(LA_Wine$average, lag.max = length(LA_Wine$average) - 1)
# pacf(LA_Wine$average, lag.max = length(LA_Wine$average) - 1)

## Europe Misc not forecastable; LA Misc not forecastable; Africa RTD/FAB note forecastable
## everything other subset has not been made

## plots for the two "grouped" subsets:
plot(LA.D_C_ready$average, pch = 16, type = "o")
acf(LA.D_C_ready$average, lag.max = length(LA.D_C_ready$average) - 1)
pacf(LA.D_C_ready$average, lag.max = length(LA.D_C_ready$average) - 1)

plot(LA.RTD_ready$average, pch = 16, type = "o")
acf(LA.RTD_ready$average, lag.max = length(LA.RTD_ready$average) - 1)
pacf(LA.RTD_ready$average, lag.max = length(LA.RTD_ready$average) - 1)

## we have 13 datasets: 6 for Africa, 7 for Latin.America

LA_Drug_Chemical = LA.D_C_ready; rm(LA.D_C_ready)
LA_RTD_FAB = LA.RTD_ready; rm(LA.RTD_ready) 
save.image("./13_subsets.RData") # uploaded to GitHub, 3:15 PM, 4/2

## I'll take Africa_Beer, LA_Food, LA_NAB, Africa_Misc

## Africa_Beer
plot(Africa_Beer$total, pch = 16, type = "o")
acf(Africa_Beer$total, lag.max = length(Africa_Beer$total) - 1)
pacf(Africa_Beer$total, lag.max = length(Africa_Beer$total) - 1)

## LA_Food
plot(LA_Food$total, pch = 16, type = "o")
acf(LA_Food$total, lag.max = length(LA_Food$total) - 1)
pacf(LA_Food$total, lag.max = length(LA_Food$total) - 1)

## LA_NAB
plot(LA_NAB$total, pch = 16, type = "o")
acf(LA_NAB$total, lag.max = length(LA_NAB$total) - 1)
pacf(LA_NAB$total, lag.max = length(LA_NAB$total) - 1)

## Africa_Misc
plot(Africa_Misc$total, pch = 16, type = "o")
acf(Africa_Misc$total, lag.max = length(Africa_Misc$total) - 1)
pacf(Africa_Misc$total, lag.max = length(Africa_Misc$total) - 1)

## helper functions
RMSE = function(actual, pred){
  sqrt(mean((actual - pred)^2))
}
MAD = function(actual, pred){
  mean(abs(actual - pred))
}
MAPE = function(actual, pred){
  mean(abs((actual - pred)/actual))
}

library(forecast)

## Africa_Beer
## arima
AB = auto.arima(Africa_Beer$total[1:97])
summary(AB)
AB_F = forecast(AB, 8)
summary(AB_F)
plot(AB_F)
cat("African Beer auto arima")
RMSE(Africa_Beer$total[98:105], AB_F$mean)
MAD(Africa_Beer$total[98:105], AB_F$mean)
MAPE(Africa_Beer$total[98:105], AB_F$mean)
## neural network
ABnn = nnetar(Africa_Beer$total[1:97])
ABnn_F = forecast(ABnn, 8)
summary(ABnn_F)
plot(ABnn_F)
cat("African Beer neural network")
RMSE(Africa_Beer$total[98:105], ABnn_F$mean)
MAD(Africa_Beer$total[98:105], ABnn_F$mean)
MAPE(Africa_Beer$total[98:105], ABnn_F$mean)

## LA_Food
LAF = auto.arima(LA_Food$total[1:97])
summary(LAF)
plot(forecast(LAF, 8))

## LA_NAB
## arima
LANAB = auto.arima(LA_NAB$total[1:97])
summary(LANAB)
LANAB_F = forecast(LANAB, 8)
summary(LANAB_F)
plot(LANAB_F)
RMSE(LA_NAB$total[98:105], LANAB_F$mean)
MAD(LA_NAB$total[98:105], LANAB_F$mean)
MAPE(LA_NAB$total[98:105], LANAB_F$mean)
## neural network
LANABnn = nnetar(LA_NAB$total[1:97])
LANABnn_F = forecast(LANABnn, 8)
summary(LANABnn_F)
plot(LANABnn_F)
RMSE(LA_NAB$total[98:105], LANABnn_F$mean)
MAD(LA_NAB$total[98:105], LANABnn_F$mean)
MAPE(LA_NAB$total[98:105], LANABnn_F$mean)

## Africa_Misc
## arima
AM = auto.arima(Africa_Misc$total[1:97])
summary(AM)
AM_F = forecast(AM, 8)
summary(AM_F)
plot(AM_F)
RMSE(Africa_Misc$total[98:105], AM_F$mean)
MAD(Africa_Misc$total[98:105], AM_F$mean)
MAPE(Africa_Misc$total[98:105], AM_F$mean)
## neural network
AMnn = nnetar(Africa_Misc$total[1:97])
AMnn_F = forecast(AMnn, 8)
summary(AMnn_F)
plot(AMnn_F)
RMSE(Africa_Misc$total[98:105], AMnn_F$mean)
MAD(Africa_Misc$total[98:105], AMnn_F$mean)
MAPE(Africa_Misc$total[98:105], AMnn_F$mean)

## need a new approach to the arima
## Africa_Beer
plot(Africa_Beer$total, pch = 16, type = "o")
acf(Africa_Beer$total, lag.max = length(Africa_Beer$total) - 1) # might need a first difference
pacf(Africa_Beer$total, lag.max = length(Africa_Beer$total) - 1)

Africa_Beer$diff = c(NA, diff(Africa_Beer$total))
plot(Africa_Beer$diff, pch = 16, type = "o")
acf(Africa_Beer$diff, lag.max = length(Africa_Beer$diff) - 1, na.action = na.omit)
pacf(Africa_Beer$diff, lag.max = length(Africa_Beer$diff) - 1, na.action = na.omit)

AB_arima = arima(Africa_Beer$total[1:97], order = c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 52))
summary(AB_arima)
AB_arima_F = forecast(AB_arima, 8)
summary(AB_arima_F)
plot(AB_arima_F)
points(x = 98:105, y = Africa_Beer$total[98:105], pch = 16, type = "o")
cat("African Beer manual arima")
RMSE(Africa_Beer$total[98:105], AB_arima_F$mean)
MAD(Africa_Beer$total[98:105], AB_arima_F$mean)
MAPE(Africa_Beer$total[98:105], AB_arima_F$mean)

## LA_Food
plot(LA_Food$total, pch = 16, type = "o")
acf(LA_Food$total, lag.max = length(LA_Food$total) - 1)
pacf(LA_Food$total, lag.max = length(LA_Food$total) - 1)

LAF_arima = arima(LA_Food$total[1:97], order = c(1, 0, 0), seasonal = list(order = c(1, 0, 1), period = 52))

## LA_NAB
plot(LA_NAB$total, pch = 16, type = "o")
acf(LA_NAB$total, lag.max = length(LA_NAB$total) - 1)
pacf(LA_NAB$total, lag.max = length(LA_NAB$total) - 1)

LANAB_arima = arima(LA_NAB$total[1:97], order = c(1, 0, 0), seasonal = list(order = c(1, 0, 1), period = 52))

## Africa_Misc
plot(Africa_Misc$total, pch = 16, type = "o")
acf(Africa_Misc$total, lag.max = length(Africa_Misc$total) - 1)
pacf(Africa_Misc$total, lag.max = length(Africa_Misc$total) - 1)

AM_arima = arima(Africa_Misc$total[1:97], order = c(1, 0, 0), seasonal = list(order = c(1, 0, 0), period = 52))

###################

## new idea: product/country combinations with a significant amount of data
OI %>% subset(Production.Category == "WINE" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Wine
plot(Colombia.Wine$total, pch = 16, type = "o")
acf(Colombia.Wine$total, lag.max = 104)
pacf(Colombia.Wine$total, lag.max = 104)

OI %>% subset(Production.Category == "BEER" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Beer
plot(Colombia.Beer$total, pch = 16, type = "o")
acf(Colombia.Beer$total, lag.max = 104)
pacf(Colombia.Beer$total, lag.max = 104)

OI %>% subset(Production.Category == "Food" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Food
plot(Colombia.Food$total, pch = 16, type = "o")
acf(Colombia.Food$total, lag.max = 104)
pacf(Colombia.Food$total, lag.max = 104)

OI %>% subset(Production.Category == "SPIRITS" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Spirits
plot(Colombia.Spirits$total, pch = 16, type = "o")
acf(Colombia.Spirits$total, lag.max = 104)
pacf(Colombia.Spirits$total, lag.max = 104)

OI %>% subset(Production.Category == "NAB" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.NAB
plot(Colombia.NAB$total, pch = 16, type = "o")
acf(Colombia.NAB$total, lag.max = 104)
pacf(Colombia.NAB$total, lag.max = 104)

# OI %>% subset(Production.Category == "Food" & Ship.to.Country.Name == "Comoros") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Comoros.Food
# plot(Comoros.Food$total, pch = 16, type = "o")
# acf(Comoros.Food$total, lag.max = 104)
# pacf(Comoros.Food$total, lag.max = 104)

# OI %>% subset(Production.Category == "WINE" & Ship.to.Country.Name == "Comoros") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Comoros.Wine
# plot(Comoros.Wine$total, pch = 16, type = "o")
# acf(Comoros.Wine$total, lag.max = 104)
# pacf(Comoros.Wine$total, lag.max = 104)

OI %>% subset(Production.Category == "Miscellaneous" & Ship.to.Country.Name == "Congo") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Congo.Misc

dotchart(unique(Congo.Misc$Week.ID), pch = 16, xlim = c(1, 105))

plot(Congo.Misc$total, pch = 16, type = "o")
acf(Congo.Misc$total, lag.max = 104)
pacf(Congo.Misc$total, lag.max = 104) # come back to

# OI %>% subset(Production.Category == "Miscellaneous" & Ship.to.Country.Name == "Cuba") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Cuba.Misc
# plot(Cuba.Misc$total, pch = 16, type = "o")
# acf(Cuba.Misc$total, lag.max = 104)
# pacf(Cuba.Misc$total, lag.max = 104)

OI %>% subset(Production.Category == "BEER" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Beer
plot(DRC.Beer$total, pch = 16, type = "o")
acf(DRC.Beer$total, lag.max = 104)
pacf(DRC.Beer$total, lag.max = 104)

OI %>% subset(Production.Category == "Food" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Food
plot(DRC.Food$total, pch = 16, type = "o")
acf(DRC.Food$total, lag.max = 104)
pacf(DRC.Food$total, lag.max = 104)

OI %>% subset(Production.Category == "Miscellaneous" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Misc
plot(DRC.Misc$total, pch = 16, type = "o")
acf(DRC.Misc$total, lag.max = 104)
pacf(DRC.Misc$total, lag.max = 104)

OI %>% subset(Production.Category == "NAB" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.NAB
plot(DRC.NAB$total, pch = 16, type = "o")
acf(DRC.NAB$total, lag.max = 104)
pacf(DRC.NAB$total, lag.max = 104)

OI %>% subset(Production.Category == "SPIRITS" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Spirits
plot(DRC.Spirits$total, pch = 16, type = "o")
acf(DRC.Spirits$total, lag.max = 104)
pacf(DRC.Spirits$total, lag.max = 104)

OI %>% subset(Production.Category == "WINE" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Wine
plot(DRC.Wine$total, pch = 16, type = "o")
acf(DRC.Wine$total, lag.max = 104)
pacf(DRC.Wine$total, lag.max = 104)

save.image("./Some_country_subsets.RData")
## we have many Colombia subsets now, many DRC (Democratic Republic of the Congo) subsets now, and a Congo Miscellaneous subset that would need some weeks combined (maybe two weeks at a time?)

## 4/4
load("./Some_country_subsets.RData")
library(LearnEDA); library(aplpack)

## Colombia.Beer
plot(Colombia.Beer$total, pch = 16, type = "o", main = "Colombia.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_Colombia.Beer = smooth(Colombia.Beer$total, kind = "3RSS", twiceit = TRUE)
smooth_Colombia.Beer = han(smooth_Colombia.Beer)
rough_Colombia.Beer = Colombia.Beer$total - smooth_Colombia.Beer
plot(smooth_Colombia.Beer, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_Colombia.Beer), pch = 16, main = "Smooth.Colombia.Beer.Residuals")
abline(h = 0, col = "blue")

## Colombia.Food
plot(Colombia.Food$total, pch = 16, type = "o", main = "Colombia.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_Colombia.Food = smooth(Colombia.Food$total, kind = "3RSS", twiceit = TRUE)
smooth_Colombia.Food = han(smooth_Colombia.Food)
rough_Colombia.Food = Colombia.Food$total - smooth_Colombia.Food
plot(smooth_Colombia.Food, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_Colombia.Food), pch = 16, main = "Smooth.Colombia.Food.Residuals")
abline(h = 0, col = "blue")

## Colombia.NAB
plot(Colombia.NAB$total, pch = 16, type = "o", main = "Colombia.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_Colombia.NAB = smooth(Colombia.NAB$total, kind = "3RSS", twiceit = TRUE)
smooth_Colombia.NAB = han(smooth_Colombia.NAB)
rough_Colombia.NAB = Colombia.NAB$total - smooth_Colombia.NAB
plot(smooth_Colombia.NAB, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_Colombia.NAB), pch = 16, main = "Smooth.Colombia.NAB.Residuals")
abline(h = 0, col = "blue")

## Colombia.Spirits
plot(Colombia.Spirits$total, pch = 16, type = "o", main = "Colombia.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_Colombia.Spirits = smooth(Colombia.Spirits$total, kind = "3RSS", twiceit = TRUE)
smooth_Colombia.Spirits = han(smooth_Colombia.Spirits)
rough_Colombia.Spirits = Colombia.Spirits$total - smooth_Colombia.Spirits
plot(smooth_Colombia.Spirits, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_Colombia.Spirits), pch = 16, main = "Smooth.Colombia.Spirits.Residuals")
abline(h = 0, col = "blue")

## Colombia.Wine
plot(Colombia.Wine$total, pch = 16, type = "o", main = "Colombia.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_Colombia.Wine = smooth(Colombia.Wine$total, kind = "3RSS", twiceit = TRUE)
smooth_Colombia.Wine = han(smooth_Colombia.Wine)
rough_Colombia.Wine = Colombia.Wine$total - smooth_Colombia.Wine
plot(smooth_Colombia.Wine, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_Colombia.Wine), pch = 16, main = "Smooth.Colombia.Wine.Residuals")
abline(h = 0, col = "blue")

## Congo.Misc (needs grouped a bit, maybe?)
dotchart(unique(Congo.Misc$Week.ID), pch = 16, xlim = c(1, 105))
abline(v = seq(0.5, 104.5, by = 4), col = "red")

## DRC.Beer
plot(DRC.Beer$total, pch = 16, type = "o", main = "DRC.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_DRC.Beer = smooth(DRC.Beer$total, kind = "3RSS", twiceit = TRUE)
smooth_DRC.Beer = han(smooth_DRC.Beer)
rough_DRC.Beer = DRC.Beer$total - smooth_DRC.Beer
plot(smooth_DRC.Beer, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_DRC.Beer), pch = 16, main = "Smooth.DRC.Beer.Residuals")
abline(h = 0, col = "blue")

## DRC.Food
plot(DRC.Food$total, pch = 16, type = "o", main = "DRC.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_DRC.Food = smooth(DRC.Food$total, kind = "3RSS", twiceit = TRUE)
smooth_DRC.Food = han(smooth_DRC.Food)
rough_DRC.Food = DRC.Food$total - smooth_DRC.Food
plot(smooth_DRC.Food, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_DRC.Food), pch = 16, main = "Smooth.DRC.Food.Residuals")
abline(h = 0, col = "blue")

## DRC.Misc
plot(DRC.Misc$total, pch = 16, type = "o", main = "DRC.Misc")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_DRC.Misc = smooth(DRC.Misc$total, kind = "3RSS", twiceit = TRUE)
smooth_DRC.Misc = han(smooth_DRC.Misc)
rough_DRC.Misc = DRC.Misc$total - smooth_DRC.Misc
plot(smooth_DRC.Misc, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Misc")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_DRC.Misc), pch = 16, main = "Smooth.DRC.Misc.Residuals")
abline(h = 0, col = "blue")

## DRC.NAB
plot(DRC.NAB$total, pch = 16, type = "o", main = "DRC.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_DRC.NAB = smooth(DRC.NAB$total, kind = "3RSS", twiceit = TRUE)
smooth_DRC.NAB = han(smooth_DRC.NAB)
rough_DRC.NAB = DRC.NAB$total - smooth_DRC.NAB
plot(smooth_DRC.NAB, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_DRC.NAB), pch = 16, main = "Smooth.DRC.NAB.Residuals")
abline(h = 0, col = "blue")

## DRC.Spirits
plot(DRC.Spirits$total, pch = 16, type = "o", main = "DRC.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_DRC.Spirits = smooth(DRC.Spirits$total, kind = "3RSS", twiceit = TRUE)
smooth_DRC.Spirits = han(smooth_DRC.Spirits)
rough_DRC.Spirits = DRC.Spirits$total - smooth_DRC.Spirits
plot(smooth_DRC.Spirits, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_DRC.Spirits), pch = 16, main = "Smooth.DRC.Spirits.Residuals")
abline(h = 0, col = "blue")

## DRC.Wine
plot(DRC.Wine$total, pch = 16, type = "o", main = "DRC.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
smooth_DRC.Wine = smooth(DRC.Wine$total, kind = "3RSS", twiceit = TRUE)
smooth_DRC.Wine = han(smooth_DRC.Wine)
rough_DRC.Wine = DRC.Wine$total - smooth_DRC.Wine
plot(smooth_DRC.Wine, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(as.vector(rough_DRC.Wine), pch = 16, main = "Smooth.DRC.Wine.Residuals")
abline(h = 0, col = "blue")

## Dusky Gull
library(dplyr)
OI %>% subset(Customer.Name == "Dusky gull") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Dusky.Gull
plot(Dusky.Gull$total, pch = 16, type = "o")

## have 10 plots
par(mfrow = c(4, 3))
plot(smooth_Colombia.Beer, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_Colombia.Food, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_Colombia.NAB, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_Colombia.Spirits, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_Colombia.Wine, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_DRC.Beer, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_DRC.Food, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_DRC.Misc, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Misc")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_DRC.NAB, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_DRC.Spirits, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
plot(smooth_DRC.Wine, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")

## overlays?
par(mfrow = c(3, 2))
plot(smooth_Colombia.Beer, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = Colombia.Beer$total, pch = 16)
plot(smooth_Colombia.Food, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = Colombia.Food$total, pch = 16)
plot(smooth_Colombia.NAB, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = Colombia.NAB$total, pch = 16)
plot(smooth_Colombia.Spirits, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = Colombia.Spirits$total, pch = 16)
plot(smooth_Colombia.Wine, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.Colombia.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = Colombia.Wine$total, pch = 16)
par(mfrow = c(3, 2))
plot(smooth_DRC.Beer, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Beer")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = DRC.Beer$total, pch = 16)
plot(smooth_DRC.Food, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Food")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = DRC.Food$total, pch = 16)
plot(smooth_DRC.Misc, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Misc")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = DRC.Misc$total, pch = 16)
plot(smooth_DRC.NAB, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.NAB")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = DRC.NAB$total, pch = 16)
plot(smooth_DRC.Spirits, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Spirits")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = DRC.Spirits$total, pch = 16)
plot(smooth_DRC.Wine, pch = 16, type = "l", col = "red", lwd = 2, main = "Smooth.DRC.Wine")
abline(v = seq(13, 104, by = 13), col = "blue")
points(x = 1:105, y = DRC.Wine$total, pch = 16)

## coefficients of variation
cv_Colombia.Beer = sd(Colombia.Beer$total)/mean(Colombia.Beer$total)
cv_Colombia.Food = sd(Colombia.Food$total)/mean(Colombia.Food$total)
cv_Colombia.NAB = sd(Colombia.NAB$total)/mean(Colombia.NAB$total)
cv_Colombia.Spirits = sd(Colombia.Spirits$total)/mean(Colombia.Spirits$total)
cv_Colombia.Wine = sd(Colombia.Wine$total)/mean(Colombia.Wine$total)
Colombia.cvs = c(cv_Colombia.Beer, cv_Colombia.Food, cv_Colombia.NAB, cv_Colombia.Spirits, cv_Colombia.Wine)
names(Colombia.cvs) = c("Beer", "Food", "NAB", "Spirits", "Wine")

cv_DRC.Beer = sd(DRC.Beer$total)/mean(DRC.Beer$total)
cv_DRC.Food = sd(DRC.Food$total)/mean(DRC.Food$total)
cv_DRC.Misc = sd(DRC.Misc$total)/mean(DRC.Misc$total)
cv_DRC.NAB = sd(DRC.NAB$total)/mean(DRC.NAB$total)
cv_DRC.Spirits = sd(DRC.Spirits$total)/mean(DRC.Spirits$total)
cv_DRC.Wine = sd(DRC.Wine$total)/mean(DRC.Wine$total)
DRC.cvs = c(cv_DRC.Beer, cv_DRC.Food, cv_DRC.Misc, cv_DRC.NAB, cv_DRC.Spirits, cv_DRC.Wine)
names(DRC.cvs) = c("Beer", "Food", "Misc", "NAB", "Spirits", "Wine")

OI %>% group_by(Week.ID, Production.Category, Ship.to.Country.Name) %>% summarise(total = sum(Pieces)) %>% group_by(Production.Category, Ship.to.Country.Name) %>% summarise(mean = mean(total)) -> means

OI %>% group_by(Week.ID, Production.Category, Ship.to.Country.Name) %>% summarise(total = sum(Pieces)) %>% group_by(Production.Category, Ship.to.Country.Name) %>% summarise(sd = sd(total)) -> sds

Colombia.cvs; DRC.cvs

#write.csv(OI, "./OI_data_fixed_country_names.csv", row.names = FALSE)

## Pallav: remove all customers who only had one order

OI %>% group_by(Customer.Name) %>% summarise(order_count = n()) -> order_count
OI %>% group_by(Customer.Name) %>% count(Customer.Name) -> order_count2
sum(order_count$order_count == 1)/nrow(order_count) # 4.10%
sum(order_count$order_count < 30)/nrow(order_count) # 66.52%
sum(order_count$order_count < 20)/nrow(order_count) # 57.26%
sum(order_count$order_count < 15)/nrow(order_count) # 49.44%
sum(order_count$order_count < 10)/nrow(order_count) # 38.69%
sum(order_count$order_count < 5)/nrow(order_count) # 19.27%

## top frequency customers:
order_count %>% arrange(desc(order_count))

OI %>% subset(Customer.Name == "Dusky gull") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Dusky.Gull # 93 obs
setdiff(1:105, unique(Dusky.Gull$Week.ID))
plot(Dusky.Gull$total, pch = 16, type = "o")
OI %>% subset(Customer.Name == "Boa, emerald green tree") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Boa # 102 obs
setdiff(1:105, unique(Boa$Week.ID))
plot(Boa$total, pch = 16, type = "o")
OI %>% subset(Customer.Name == "Cat, european wild") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Cat # 104 obs (the only missing one if the first, so the series is still continuous)
setdiff(1:105, unique(Cat$Week.ID))
plot(Cat$total, pch = 16, type = "o")
OI %>% subset(Customer.Name == "Alpaca") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Alpaca # 105 obs
setdiff(1:105, unique(Alpaca$Week.ID))
plot(Alpaca$total, pch = 16, type = "o")
OI %>% subset(Customer.Name == "Agile wallaby") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Agile.Wallaby # 102 obs
plot(Agile.Wallaby$total, pch = 16, type = "o")
setdiff(1:105, unique(Agile.Wallaby$Week.ID))

## let's filter out customers who ordered less than 20 times:
OI %>% left_join(order_count, "Customer.Name") %>% filter(order_count >= 20) %>% droplevels() -> OI_gt_20

OI_gt_20 %>% subset(Production.Category == "BEER" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Beer.20
OI_gt_20 %>% subset(Production.Category == "Food" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Food.20
OI_gt_20 %>% subset(Production.Category == "Miscellaneous" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Misc.20
OI_gt_20 %>% subset(Production.Category == "NAB" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.NAB.20
OI_gt_20 %>% subset(Production.Category == "SPIRITS" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Spirits.20
OI_gt_20 %>% subset(Production.Category == "WINE" & Ship.to.Country.Name == "DRC") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> DRC.Wine.20

OI_gt_20 %>% subset(Production.Category == "BEER" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Beer.20
OI_gt_20 %>% subset(Production.Category == "Food" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Food.20
OI_gt_20 %>% subset(Production.Category == "NAB" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.NAB.20
OI_gt_20 %>% subset(Production.Category == "SPIRITS" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Spirits.20
OI_gt_20 %>% subset(Production.Category == "WINE" & Ship.to.Country.Name == "Colombia") %>% group_by(Week.ID) %>% summarise(total = sum(Pieces)) -> Colombia.Wine.20

save.image("./4.5.10.35.PM.RData")
