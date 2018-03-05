#**************************************
#02 PCA, Smoothing and Visual---------
#**************************************

#load packages
pacman::p_load(tidyverse, sf, sp, acs, mapview, tigris, data.table, ggbiplot)

#load data
pca_data <- readRDS("pca_data.Rds")
colnames(pca_data)[1] <- "employed"
colnames(pca_data)[9] <- "University"
load("data.Rdata")
#********************************************************
# PCA
#********************************************************
set.seed(123)
pca <- princomp(pca_data[,c(1:7,9)], cor = TRUE, scores = T) #
pca2 <- princomp(pca_data[,c(1:12)], cor = TRUE, scores = T) # final vers
#change sign for both loading and score to get positive purchase power
# optional:
pca$loadings <- (pca$loadings*-1)
pca$scores <- (pca$scores*-1)

pca2$loadings <- (pca2$loadings*-1)
pca2$scores <- (pca2$scores*-1)

summary(pca)

# plot ggbiplot - needed adjustment of the default function
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


biplot1 <- ggbiplot_custom(pca, obs.scale = 1, var.scale = 1,scale = 0.4, size = 0.1,
                ellipse = TRUE, circle = TRUE, lable.size = 5) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  xlim(-8,8) + ylim(-8,8)

biplot2 <- ggbiplot_custom(pca2, obs.scale = 1, var.scale = 1,scale = 0.4, size = 0.1,
                ellipse = TRUE, circle = TRUE, lable.size = 5) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  xlim(-8,8) + ylim(-8,8)
save(biplot1,biplot2, file = "plots.Rdata")
gridExtra::grid.arrange(biplot1,biplot2,ncol = 2)
# scores of purchasing power
scores <- abs(pca$scores)
scores <- pca$scores[,1] + 100
scores <- data.frame(scores, TRACTCE = as.character(pca_data$tract_name2),
                     name = as.factor(pca_data$c_name),state_id = as.factor(pca_data$state_id))
#*************************************
#aggregation #########################
#*************************************
tracts$state_id <- as.factor(gsub("^0","",tracts$STATEFP))
tract_scores <- left_join(tracts,scores, by = c("TRACTCE", "state_id"))

data_t <- data.table(tract_scores)
county_scores <- data_t[,mean(scores,na.rm = T), by =c("STATEFP","COUNTYFP")] # aggregation with data table
county_scores <- left_join(counties, county_scores)

#***************************************
#Visual-Raw#############################
#***************************************

#pre visual
mapviewOptions(vector.palette = viridisLite::viridis(256,option = "D"))
colnames(county_scores)[18] <- "pp_raw"

#colors and breaks
pal <- rev(RColorBrewer::brewer.pal(n = 10, name="RdYlBu"))
breaks.raw <-classInt::classIntervals(county_scores$pp_raw, n = 6,
                                      style = "pretty",intervalClosure = "left")

#mapping

# one tract
tract_lvl<- tract_scores %>%
  filter(STATEFP == "06") %>%
  filter(COUNTYFP == "041")
save(tract_lvl,breaks.raw,pal, file = "tract.Rdata")

mapview(tract_lvl, zcol = "scores", legend = T,
        at = breaks.raw$brks, col.regions = pal, lwd = 0.2,
        layer.name = "purchase power")
# State California with all Counties

county_scores %>%
  filter(STATEFP == "06") %>%
  mapview(zcol ="pp_raw", legend = T,map.types = c("CartoDB.Positron"),
          layer.name = "purchase power")

# whole region
west <- county_scores %>%
  filter(STATEFP != "15") %>% #exclude hawii and alska for visualization
  filter(STATEFP != "02")

west.raw <- west %>%
  mapview(zcol = "pp_raw", legend = T,map.types = c("CartoDB.Positron"), zoom = 5,
           at = breaks.raw$brks, col.regions = pal, lwd = 0.2,
          layer.name = "purchase power")

all.raw <- all.west %>%
  mapview(zcol = "pp_raw", legend = T,map.types = c("CartoDB.Positron"), zoom = 5,
          at = breaks.raw$brks, col.regions = pal, lwd = 0.2,
          layer.name = "purchase power")

#static
raw <- spplot(as(west, "Spatial"),  zcol="V1", at = , col.regions = pal)

#***********************************************
#Smoothing######################################
#***********************************************

for(k in 1:nrow(county_scores)){
  print(k)
  temp <- st_intersects(county_scores[k,], county_scores)
  val <- sum(county_scores[temp[[1]],]$pp_raw)/length(temp[[1]])
  county_scores$V2[k] <- val
}

county_scores$pp_smooth <- round(county_scores$V2,digits = 1)

#*************************************
# Visual-Smooth#######################
#*************************************

pal <- rev(RColorBrewer::brewer.pal(n = 9, name="RdYlBu"))
breaks.smooth <-classInt::classIntervals(county_scores$pp_smooth, n = 6,
                                         style = "pretty",intervalClosure = "left")
# brks.smooth <- quantile(county_scores$V2)
# brks <- c(98.40938,  98.77851, 98.97680,  99.26162,100, 100.5, 101.24529 )
# pal <- c("#2171b5","#6baed6","#bdd7e7","#ffffb2","#ffeda0","#fe9929","#cc4c02")
# one state

county_scores %>%
  filter(STATEFP == "06") %>%
  mapview(zcol = "pp_smooth", legend = T, map.types = c("CartoDB.Positron"))

# the west-states
west <- county_scores %>%
  filter(STATEFP != "15") %>% #exclude hawii and alska for visualization
  filter(STATEFP != "02")
#simplify layers for visual.
west <- rmapshaper::ms_simplify(west, keep = 0.01,
                        keep_shapes = TRUE)
all.west <- rmapshaper::ms_simplify(county_scores, keep = 0.01,
                                    keep_shapes = TRUE)
#
west.smooth <- west %>%
  mapview(zcol = "pp_smooth", legend = T,map.types = c("CartoDB.Positron"), zoom = 5,
          at = breaks.smooth$brks, col.regions = pal, lwd = 0.2, layercontrol = F,
          layer.name = "purchase power")

all.smooth <- all.west %>%
  mapview(zcol = "pp_smooth", legend = T,map.types = c("CartoDB.Positron"), zoom = 5,
          at = breaks.smooth$brks, col.regions = pal, lwd = 0.2, layercontrol = F,
          layer.name = "purchase power", zoom = 4)


#Static
smooth <- spplot(as(west, "Spatial"),  zcol="V3", at = breaks.smooth$brks , col.regions = pal)

save(west.raw,west.smooth, file = "maps.Rdata")

#Saving to shape
# raster::shapefile(as(tracts,"Spatial"),"tract.shp")
# raster::shapefile(as(counties,"Spatial"),"count.shp")
# raster::shapefile(as(county_scores, "Spatial), "scores.shp")
