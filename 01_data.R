#01_Data

#working census key: 73562ee0a098c703e3b06e5341c284a6654d0c1f

# Area for ID: 142645 --> West

# load packages
#install.packages("acs", clean=T)

library(devtools)
install_github("vqv/ggbiplot")
pacman::p_load(tidyverse, sf, sp, acs, mapview, tigris, data.table, ggbiplot)


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



area <- geo.make(state = c(state.west$abb), county = "*",tract = "*")

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

library(rgdal)
if (find_d("counties") && find_d("states")==F){
  counties <- counties(state.west$abb)
  n = 1
  tract_list <- list()
  for (j in state.west$abb){
    tract_list[[n]] <- tracts(state = j, cb = TRUE)
    n = n +1
  }
  tracts <- do.call(rbind_tigris, tract_list)
save.image("data.Rdata")


#thematic data -cleaning
tract_name <- B21001@geography$tract

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
  avg <-(sum(dftarget,na.rm = T))/(sum(Total))
  return(((dftarget/Total)/avg))
}

all_index <- list()
#**********************************************************
# Variable 1: Number of workers######################
#**********************************************************

employed <- B23001e[, .SD, .SDcols =
                       B23001e[, grep("*Employed$", colnames(B23001e))]] #total employed


tot_employ <- data.frame(tract_name, B23001e[,1], employed) #total per county

colnames(tot_employ)<- c("tract","Total",paste0("col", 1:26))

tot_employ$Worker <- apply(tot_employ[, c(3:28)],sum, MARGIN = 1)

#index
tot_employ$index <- index(tot_employ, "Worker")

all_index[[1]] <- tot_employ

#**********************************************************
# Variable 2: Health Ensurance############################
#**********************************************************

noEns <- B27001e[, .SD, .SDcols =
                       B27001e[, grep("*No health insurance coverage$", colnames(B27001e))]]

HE <- data.frame(tract_name,B27001e[,1],noEns)

colnames(HE)<- c("county","Total",paste0("col", 1:18))
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

richkids <- data.frame(tract_name,B17021e[,c(22,1)]) # above poverty
colnames(richkids) <- c("county", "non_poverty", "Total")

# Index
richkids$index <- index(richkids, "non_poverty")

all_index[[4]] <- richkids
#**********************************************************
# Variable 5: Income ######################################
#**********************************************************

income <- data.frame(tract_name, B19301e, as.numeric(Total_pop$`Total Population: Total`))
colnames(income) <- c("county", "income","Total")

#index Income
avg <- sum(income$income,na.rm = T)/21513
income$index <- income$income/avg

all_index[[5]] <- income

#**********************************************************
# Variable 6: Family Income ###############################
#**********************************************************

high_income <- data.frame(county = tract_name, family_income = apply(B19101e[, c(9:17)],sum, MARGIN = 1),
                     Total = apply(B19101e[,1:17], sum,MARGIN = 1)) #income over 40.000$
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

index_list <- list()
j = 1
for (i in all_index){
  #print(i)
  index_list[[j]] <- i[,length(i)]
  j = j +1
  if (j > length(all_index)){
    index_list[[j]] <- tract_name
    pca_data <- do.call(cbind, index_list)
    colnames(pca_data)[j] <- "tract_name"
    colnames(pca_data)[5] <- "income"
    pca_data <- pca_data[complete.cases(pca_data),]
  }
}

saveRDS(pca_data,file = "./pca_data.Rds")
pca_data <- readRDS("pca_data.Rds")
#********************************************************
# PCA
#********************************************************

pca <- princomp(pca_data[,1:8], cor = TRUE)
#length(pca_data)-1
# importance of components
summary(pca)


View(pca$scores)


# Varianz
gsa.var <- pca$sdev^2
histogram(gsa.var/sum(gsa.var))

# plot ggbiplot
ggbiplot_custom <- function (pcobj, size ,choices = 1:2, scale = 1, pc.biplot = TRUE,
          obs.scale = 1 - scale, var.scale = scale, groups = NULL,
          ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3,
          alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69,
          varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE,
          ...)
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  library(ggthemes)
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  }
  else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  }
  else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
    v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord),
                                                  1]), FUN = "/")
  }
  else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  }
  else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale,
                              FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  if (obs.scale == 0) {
    u.axis.labs <- paste("standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)",
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) +
    ylab(u.axis.labs[2]) + coord_equal()
  if (var.axes) {
    if (circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi,
                                                length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r *
                             sin(theta))
      g <- g + geom_path(data = circle, color = muted("white"),
                         size = 1/2, alpha = 1/3)
    }
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0,
                                           xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2,
                                                                                                  "picas")), color = muted("cyan"), size = 3)
  }
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups),
                         size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha, size = 0.1, colour = "black")
    }
    else {
      g <- g + geom_point(alpha = alpha, size = 0.1, colour = "black")
    }
  }
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2,
                       mu, FUN = "+"), groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  if (var.axes) {
    g <- g + geom_text(data = df.v, aes(label = varname,
                                        x = xvar, y = yvar, angle = angle, hjust = hjust),
                       color = "cyan", size = varname.size)
  }
  g <- g + theme_solarized(light =  F)
  return(g)
}

ggbiplot_custom(pca, obs.scale = 1, var.scale = 1,scale = 0.4, size = 0.1,
         ellipse = TRUE, circle = TRUE, lable.size = 5) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  xlim(-4,4) + ylim(-4,4)

# loadings
loadings <- pca$loadings

# scores

scores <- pca$scores
scores <- pca$scores[,1]+ 100
scores <- data.frame(scores, TRACTCE = pca_data$tract_name)

#merge with spatial data
tract_scores <- left_join(tracts, scores, by = "TRACTCE")

county290 <- tract_scores %>%
  filter(COUNTYFP == "290")

mapview(county290, zcol = "scores", legend = T)



data_t = data.table(tract_scores)
data_t <- na.omit(data_t)
county_scores <- data_t[,mean(scores), by ="COUNTYFP"]

county_scores <- left_join(counties, county_scores)

mapview(county_scores, zcol="V1", legend = T)
