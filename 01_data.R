####################################################
# Applied GIScience
# Assingment 2
# GEO 404 FSU Jena
# Eric Krueger
# Date: Winter 2017/18
# Script 1 - Data preperation
####################################################
# Purchasing power in the US(West)
####################################################

#***********************************************
#01_Data--------------------------------------
#***********************************************

#working census key: 73562ee0a098c703e3b06e5341c284a6654d0c1f

# Area for ID: 142645 --> West

# load packages
#install.packages("acs", clean=T)

#library(devtools)
#install_github("vqv/ggbiplot")
pacman::p_load(tidyverse, sf, sp, acs, mapview, tigris, data.table, ggbiplot)

# load data
data(state)
# make a df
state <- data.frame(name = state.name, area = state.area, abb = state.abb,
                    division = state.division, region = state.region, x = state.center$x,
                    y = state.center$y)
# filter west
state.west<- state %>%
  filter(region == "West")

#*******************************
#census data download###########
#*******************************
acs::api.key.install('73562ee0a098c703e3b06e5341c284a6654d0c1f')

# for fetch geography
area <- geo.make(state = c(as.character(state.west$abb)), county = "*",tract = "*")

# Age, Sex, Income (Households), Employment (Class of Worker), Education, Family,

#ENDYEAR was orignally set to 2016, but due to incomplete datasets this was not
#possible (i.e. health insurance, was not provided with a 2016 data set).
#additionally only table numbers startin with B.* are working with acs.fetch()

#Sex by Age, income, poverty, health insurance, earnings, education, household size,
#family income, vehicles, value of housing, total pop

# census_tbl based on factfinder

census_tbl <- c( "B23001", "B06009", "B21001","B19301","B17021","B17021","B27001",
                 "B11016", "B19101", "B08015", "B25075","B01003")

for (i in census_tbl){
  print(i)
  assign(i,acs.fetch(endyear = 2015, span = 5, geography = area,
            table.number = i, col.names = "pretty"))
}

# spatial data, downloaded if needed
find_d <- function(dname){
  for (i in (length(ls() == dname))){
    if (i == TRUE){
      return(TRUE)
      break
    }
  }
}
if (find_d("tracts") && find_d("counties")==F){
  counties <- counties(as.character(state.west$abb))
  n = 1
  tract_list <- list()
  for (j in as.character(state.west$abb)){
    tract_list[[n]] <- tracts(state = j, cb = TRUE)
    n = n +1
  }
  tracts <- do.call(rbind_tigris, tract_list)
  counties <- st_as_sf(counties)
  tracts <- st_as_sf(tracts)
}
#save(tracts,counties,"spatial.Rdata")
#load("spatial.Rdata")

#****************************
#thematic data -cleaning#####
#****************************

load("census.Rdata")

tract_name <- rownames(estimate(B21001))
tract_name2 <- B21001@geography$tract
c_name <- gsub(pattern = "[A-z].*?,[[:space:]]","",B21001@geography$NAME)
state_id <- as.character(B21001@geography$state)
Total_pop <- as.data.table(estimate(B01003))

for (i in grep("B.*[0-9]$", ls(),value = T)){
  print(i)
  assign(paste0(i,"e"),as.data.table(estimate(get(i))))
} # cleaned vers. ending with e

# Index formula
index <- function(df, dftarget){
  df <- as.data.frame(df)
  dftarget <- df %>%
    dplyr::select(dftarget)
  Total <- df %>%
    dplyr::select(Total)
  avg <-(sum(dftarget, na.rm = T))/(sum(Total, na.rm = T))
  return(((dftarget/Total)/avg*100))
}

all_index <- list()
#**********************************************************
# Variable 1: Number of workers######################
#**********************************************************
employed <- B23001e[, .SD, .SDcols =
                       B23001e[, grep("*Employed$", colnames(B23001e))]]
#tot_employ <- data.frame(tract_name, B23001e[,1], employed)
tot_employ <- data.frame(tract_name,B23001e[,1], employed)
tot_employ$Worker <- apply(tot_employ[, c(3:28)],sum, MARGIN = 1)#all workers per tract
colnames(tot_employ)[1:2]<- c("tract","Total")
#index
tot_employ$index <- index(tot_employ, "Worker")

all_index[[1]] <- tot_employ
#**********************************************************
# Variable 2: Health Ensurance############################
#**********************************************************
noEns <- B27001e[, .SD, .SDcols =
                       B27001e[, grep("*No health insurance coverage$", colnames(B27001e))]]

HE <- data.frame(tract_name,B27001e[,1],noEns)
#B27001e[,1]
colnames(HE)<- c("tract","Total",paste0("col", 1:18))
HE$colsum <- apply(HE[, c(3:20)],sum, MARGIN = 1)
HE$ensurance <- HE$Total - HE$colsum

# Index for HealthEnsurance
HE$indexHE <- index(HE, "ensurance")

all_index[[2]] <- HE
#**********************************************************
# 3 Variable 3: Number of cars
#**********************************************************
tot_cars <- data.frame(tract_name, B08015e[,1], Total_pop)
colnames(tot_cars)<- c("tract","cars","Total")

# Index
tot_cars$index <- index(tot_cars, "cars")

all_index[[3]] <- tot_cars
#**********************************************************
# Variable 4: Poverty status #############################
#**********************************************************
richkids <- data.frame(tract_name,B17021e[,c(22,1)], Total_pop) # above poverty
colnames(richkids) <- c("tract", "non_poverty", "Total")
# Index
richkids$index <- index(richkids, "non_poverty")

all_index[[4]] <- richkids
#**********************************************************
# Variable 5: Income ######################################
#**********************************************************
income <- data.frame(tract_name, B19301e, Total_pop)
colnames(income) <- c("tract", "income","Total")

#index Income
avg <- (sum(income$income, na.rm = T)/16071)
income$index <- (income$income/avg)

all_index[[5]] <- income
#**********************************************************
# Variable 6: Family Income ###############################
#**********************************************************
high_income <- data.frame(county = tract_name, family_income = apply(B19101e[, c(13:17)],sum, MARGIN = 1),
                     Total = apply(B19101e[,1:17], sum,MARGIN = 1)) #income over 75.000$
#index family in
high_income$index <- index(high_income, "family_income")

all_index[[6]] <- high_income
#**********************************************************
# Variable 7: House value #################################
#**********************************************************
house_value <- data.frame(county = tract_name,  house_value =apply(B25075e[, c(15:27)],sum, MARGIN = 1),
                          Total = B25075e[,1]) #value over 100k$
colnames(house_value)[3] <- "Total"
#index value
house_value$index <- index(house_value, "house_value")
all_index[[7]] <- house_value
#**********************************************************
# Variable 8: Nr. of family households with 3 or more persons
#**********************************************************
family <- data.frame(tract = tract_name,  family = B11016e[,2],
                     Total = B11016e[,1])
colnames(family)[3] <- "Total"
colnames(family)[2] <- "family"
#index Income
family$index <- index(family, "family")

all_index[[8]] <- family
#**********************************************************
# Variable 9: Education,  degree ##############
#**********************************************************
bsc <- B06009e[, .SD, .SDcols =
                 B06009e[, grep("Bachelor", colnames(B06009e))]]
high_school <- data.frame(tract = tract_name, bsc,
                     Total = B06009e[,1])

high_school$bsc <- apply(high_school[, c(2:6)],sum, MARGIN = 1)

colnames(high_school)[7] <- "Total"
colnames(high_school)[8] <- "bsc"
#index Income
high_school$index <- index(high_school, "bsc")

all_index[[9]] <- high_school
#**********************************************************
# Variable 10 & 11: Sex ##############
#**********************************************************
male <- data.frame(tract = tract_name, male = B21001e[,4],
                          Total = Total_pop)  # male over 18
colnames(male)[2] <- "male"
colnames(male)[3] <- "Total"
#index sex
male$index <- index(male, "male")

all_index[[10]] <- male
######

female <- data.frame(tract = tract_name, female = B21001e[,22],
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
age <- data.frame(tract = tract_name, age = apply(B21001e[, c(7,10,13,25,28,31)],sum, MARGIN = 1),
                   Total = Total_pop)  #
colnames(age)[3] <- "Total"
age$index <- index(age, "age")

all_index[[12]] <- age
#********************************************************
# combine all data######################################
#********************************************************
index_list <- list()
j = 1
for (i in all_index){
  #print(i)
  index_list[[j]] <- i[,length(i)]
  j = j +1
  if (j > length(all_index)){
    index_list[[j]] <- tract_name
    index_list[[j+1]] <- tract_name2
    index_list[[j+2]] <- c_name
    index_list[[j+3]] <- state_id
    pca_data <- do.call(cbind, index_list)
    colnames(pca_data)[j] <- "tract_name"
    colnames(pca_data)[j+1] <- "tract_name2"
    colnames(pca_data)[j+2] <- "c_name"
    colnames(pca_data)[j+3] <- "state_id"
    colnames(pca_data)[5] <- "income"

    pca_data <- pca_data[complete.cases(pca_data),]
  }
}
#************************************
#Saving#############################
#************************************
saveRDS(pca_data,file = "./pca_data.Rds")
#save.image("data.Rdata")