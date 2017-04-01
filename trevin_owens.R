library(ggplot2)
library(plyr)

owens_data <- read.csv("new_data.csv")

# drop the first column of ordered integers
owens_data <- subset(owens_data, select = -c(X))

# rename columns
owens_data <- rename(owens_data, c("Calendar.Year.Week"="Date", "Item.ID" = "ID", 
                     "Production.Category" = "Category", "Customer.Name" = "Customer",
                     "Shipment.Destination" = "Destination", "Ship.to.Country.name" = "Country",
                     "Net.Weight" = "Weight"))

# rename country
owens_data$Country <- revalue(owens_data$Country, c(
                      "Democratic Republic of the Congo\xa0(Kinshasa)"="Kinshasa", 
                      "Congo, Republic of\xa0(Brazzaville)"="Brazzaville",
                      "English\xa0English Name"="English"))

# delete observations with less than 5 observations
owens_data <- subset(owens_data, Country!="English" & Country!="#N/A")
owens_data <- droplevels(owens_data)

# group countries by region
SA <- c("Colombia", "Dominica", "Dominican Republic", "Cuba")
EU <- c("Czech Republic", "Cyprus", "Croatia", "Denmark")
AF <- c("Comoros", "Brazzaville", "Kinshasa", "Djibouti")

sa.df <- owens_data[owens_data$Country %in% SA,]
eu.df <- owens_data[owens_data$Country %in% EU,]
af.df <- owens_data[owens_data$Country %in% AF,]

# combine into new dataframe
df <- rbind(af.df, eu.df, sa.df)


