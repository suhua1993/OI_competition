OI = read.csv("./OI.csv", na.strings = "#N/A")
library(dplyr)
#subset(OI, Production.Category == "BEER" & Ship.to.Country.Name == "Colombia")
a = levels(OI$Production.Category)
b = levels(OI$Ship.to.Country.Name)
LIST = list(BEER = list(), `Drug & Chemical` = list(), Food = list(), Miscellaneous = list(), NAB = list(), `RTD/FAB` = list(), SPIRITS = list(), WINE = list())
for(i in 1:length(a)){
  for(j in 1:length(b)){
    LIST[[i]][[j]] = droplevels(subset(OI, Production.Category == a[i] & Ship.to.Country.Name == b[j], select = c(Week.ID:Item.ID, Customer.Name:Shipment.Destination, Pieces:Tonnes)))
  }
}
## Beer-Columbia
for(i in 1:length(a)){
  for(j in 1:length(b)){
    if(prod(dim(LIST[[i]][[j]])) == 0){
      plot(1:2, main = paste("Product ", a[i], " and Country ", b[j], sep = ""))
    }
    else{hist(LIST[[i]][[j]]$Net.Weight, main = paste("Product ", a[i], " and Country ", b[j], sep = ""))}
  }
}

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
setdiff(1:105, unique(subset(Latin.America, Production.Category %in% c("Food", "Miscellaneous"))$Week.ID))
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