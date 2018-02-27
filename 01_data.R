#01_Data

#working census key: 73562ee0a098c703e3b06e5341c284a6654d0c1f

# Area for ID: 142645 --> West

# load packages
#install.packages("acs", clean=T)
pacman::p_load(tidyverse, sf, sp, acs, mapview, tigris, data.table)

library(raster)
library(sp)
library(tigris)
library(abind)
library(data.table)
library(vegan)
library(RColorBrewer)
library(classInt)
library(rgeos)
library(bibtex)
library(ggfortify)
library(ggbiplot)
library(stats)


# load data
data(state)

# make a df
state <- data.frame(name = state.name, area = state.area, abb = state.abb,
                    division = state.division, region = state.region, x = state.center$x,
                    y = state.center$y)
# filter west
state.west<- state %>%
  filter(region == "West")


#census data
acs::api.key.install('73562ee0a098c703e3b06e5341c284a6654d0c1f')


area <- geo.make(state = c(state.west$abb), county = "*")
# Age, Sex, Income (Households), Employment (Class of Worker), Education, Family,

#ENDYEAR was orignally set to 2016, but due to incomplete datasets this was not
#possible (i.e. health insurance, was not provided with a 2016 data set).
#additionally only table numbers startin with B.* are working with acs.fetch()


#Sex by Age
B21001 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B21001", col.names = "pretty")
#income x
B19301 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B19301", col.names = "pretty")
#poverty x
B17021 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B17021", col.names = "pretty")
#health insurance x
B27001<- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B27001", col.names = "pretty")
#earnings x
B23001 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                   table.number = "B23001", col.names = "pretty")
#education x
B06009 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B06009", col.names = "pretty")
#household size x
B11016 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B11016", col.names = "pretty")
#family income x
B19101 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B19101", col.names = "pretty")
# vehicles available x
B08015 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B08015", col.names = "pretty")
# value of housing x
B25075 <- acs.fetch(endyear = 2015, span = 5, geography = area,
                    table.number = "B25075", col.names = "pretty")
#total pop x
total <- acs.fetch(endyear = 2015,span= 5, geography = area,
                          table.number = "B01003", col.names = "pretty")

# spatial data, downloaded if needed
find_d <- function(dname){
  for (i in (length(ls() == dname))){
    if (i == TRUE){
      return(TRUE)
      break
    }
  }
}
if (find_d("counties") && find_d("states")==F){
  counties <- counties(state.west$abb)
  states <- states()
  if (class(counties) == "sp"){
  counties <- st_as_sf(counties)
  states <- st_as_sf(states)
  }
}

#thematic data -cleaning
county_name <- rownames(estimate(B24091))
Total_pop <- as.data.table(estimate(total))# total Population

for (i in grep("B.*[0-9]$", ls(),value = T)){
  print(i)
  assign(paste0(i,"e"),as.data.table(estimate(get(i))))
} # cleaned vers. ending with e

# Index
index <- function(df, dftarget){
  df <- as.data.frame(df)
  dftarget <- df %>%
    dplyr::select(dftarget)
  Total <- df %>%
    dplyr::select(Total)
  avg <-(sum(dftarget))/(sum(Total))
  return(((dftarget/Total)/avg))
}

all_index <- list()
#**********************************************************
# Variable 1: Number of workers######################
#**********************************************************

employed <- B23001e[, .SD, .SDcols =
                       B23001e[, grep("*Employed$", colnames(B23001e))]] #total employed

tot_employ <- cbind(county_name, B23001e[,1], Employed) #total per county

colnames(tot_employ)<- c("county","Total",paste0("col", 1:26))
tot_employ$Worker <- apply(tot_employ[, c(3:28)],sum, MARGIN = 1)


#index
tot_employ$index <- index(tot_employ, "Worker")

all_index[[1]] <- tot_employ

#**********************************************************
# Variable 2: Health Ensurance############################
#**********************************************************

noEns <- B27001e[, .SD, .SDcols =
                       B27001e[, grep("*No health insurance coverage$", colnames(B27001e))]]
HE <- cbind(county_name,B27001e[,1],noEns)

colnames(HE)<- c("county","Total",paste0("col", 1:18))
HE$colsum <- apply(HE[, c(3:20)],sum, MARGIN = 1)
HE$ensurance <- HE$Total - HE$colsum

# Index for HealthEnsurance
HE$indexHE <- index(HE, "ensurance")

all_index[[2]] <- HE

#**********************************************************
# 3 Variable 3: Number of cars
#**********************************************************
tot_cars <- cbind(county_name, B08015e[,1], Total_pop)
colnames(tot_cars)<- c("county","cars","Total")

# Index
tot_cars$index <- index(tot_cars, "cars")

all_index[[3]] <- tot_cars

#**********************************************************
# Variable 4: Poverty status #############################
#**********************************************************
richkids <- cbind(county_name,B17021e[,c(22,1)]) # above poverty
colnames(richkids) <- c("county", "rich", "Total")

# Index
richkids$index <- index(richkids, "rich")

all_index[[4]] <- richkids
#**********************************************************
# Variable 5: Income ######################################
#**********************************************************
income <- cbind(county_name, B19301e,Total_pop)
colnames(income) <- c("county", "money","Total")

#index Income
income$index <- index(income, "money")

all_index[[5]] <- income

#**********************************************************
# Variable 6: Family Income ###############################
#**********************************************************
high_income <- data.frame(county = county_name, high = apply(B19101e[, c(9:17)],sum, MARGIN = 1),
                     Total = apply(B19101e[,1:17], sum,MARGIN = 1)) #income over 40.000$
#index family in
high_income$index <- index(high_income, "high")

all_index[[6]] <- high_income

#**********************************************************
# Variable 7: House value #################################
#**********************************************************
house_value <- data.frame(county = county_name,  value =apply(B25075e[, c(15:27)],sum, MARGIN = 1),
                          Total = B25075e[,1]) #value over 100k$
colnames(house_value)[3] <- "Total"
#index value
house_value$index <- index(house_value, "value")

all_index[[7]] <- house_value
#**********************************************************
# Variable 8: Nr. of family households with 3 or more persons
#**********************************************************
family <- data.frame(county = county_name,  family = B11016e[,2],
                          Total = B11016e[,1])
colnames(family)[3] <- "Total"
colnames(family)[2] <- "family"
#index Income
family$index <- index(family, "family")

all_index[[8]] <- family

#**********************************************************
# Variable 9: Education, high school degree ##############
#**********************************************************
high_school <- data.frame(county = county_name, high_school = B06009e[,3],
                     Total = B06009e[,1])
colnames(high_school)[3] <- "Total"
colnames(high_school)[2] <- "high-school"
#index Income
high_school$index <- index(high_school, "high-school")

all_index[[9]] <- high_school

#**********************************************************
# Variable 10 & 11: Sex ##############
#**********************************************************
male <- data.frame(county = county_name, male = B21001e[, 4],
                          Total = Total_pop)  # male over 18
colnames(male)[2] <- "male"
colnames(male)[3] <- "Total"
#index sex
male$index <- index(male, "male")

all_index[[10]] <- male
######
female <- data.frame(county = county_name, female = B21001e[, 22],
                     Total = Total_pop)  # female over 18

colnames(female)[2] <- "female"
colnames(female)[3] <- "Total"
#index sex
female$index <- index(female, "female")

all_index[[11]] <- female

#**********************************************************
# Variable 12: Age ##############
#**********************************************************
#males and females between the age of 18-65
age <- data.frame(county = county_name, age = apply(B21001e[, c(7,10,13,25,28,31)],sum, MARGIN = 1),
                   Total = Total_pop)  #
colnames(age)[3] <- "Total"
age$index <- index(age, "age")

all_index[[12]] <- age

#********************************************************
# combine all data
#********************************************************

for (i in all_index){
  print(i)


}

