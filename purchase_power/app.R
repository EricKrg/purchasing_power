#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(tidyverse)
require(sf)
require(sp)
require(data.table)
require(leaflet)
require(shiny)
require(shinydashboard)
#
#
# load data
# is_local <- Sys.getenv('SHINY_PORT') == ""
# if(is_local){
#   pca_data <- readRDS("./purchase_power/data/pca_data.Rds")
#   smp_counties <- readRDS("./purchase_power/data/counties.Rds")
#   smp_tracts <- readRDS("./purchase_power/data/tracts.Rds")
#   county_scores <- readRDS("./purchase_power/data/scores.Rds")
#   state.west <- readRDS("./purchase_power/data/west.Rds")
# } else{
#   pca_data <- readRDS("./data/pca_data.Rds")
#   smp_counties <- readRDS("./data/counties.Rds")
#   smp_tracts <- readRDS("./data/tracts.Rds")
#   county_scores <- readRDS("./data/scores.Rds")
#   state.west <- readRDS("./data/west.Rds")}

pca_data <- readRDS("./data/pca_data.Rds")
smp_counties <- readRDS("./data/counties.Rds")
smp_tracts <- readRDS("./data/tracts.Rds")
county_scores <- readRDS("./data/scores.Rds")
state.west <- readRDS("./data/west.Rds")
gc()
# Define UI for application that draws a histogram
ui <- navbarPage("Purchasing Power USA", id = "nav",
  tabPanel("Map",
  div(class = "outer",
  tags$head(
    # Include our custom CSS
    includeCSS("./data/style.css")),
  leafletOutput("map", width ="100%", height = "100%"),
  absolutePanel(id = "better_panel",
                      class = "panel panel-default",
                      width = "auto",
                      height = 60,
                      right = "auto",
                      left =  "auto",
                      top = 5,
                      bottom = "auto",
                      fixed = TRUE),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 60, left = "auto" , right = 15, bottom = "auto",
                width = 440, height = "100%",
                h3("Control Panel"),
                selectInput('STATEFP', 'region',c(paste0(unique(county_scores$STATEFP),
                                                         " ",unique(state.west$name)), "WEST")),
                tags$div(align = "left",
                         class = "multicol",
                         checkboxGroupInput("variable", "PCA Variables",
                                   c(colnames(pca_data[,1:12])),width = 400)),
  plotOutput("biplot"),
  box(title = "Information",status = "warning", solidHeader = TRUE,
      includeMarkdown("./data/desc.md")
                )))),
  tabPanel("Data",
           plotOutput("plot1")
           )
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # data required: pca_data, tracts, counties
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
      print(plots[[1]])

    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  ggbiplot_custom <- function (pcobj, size ,choices = 1:2, scale = 1, pc.biplot = TRUE,
                               obs.scale = 1 - scale, var.scale = scale, groups = NULL,
                               ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 10,
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
                                                                                                    "picas")), color = muted("red"), size = 3)
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
                         color = "red", size = varname.size)
    }
    return(g)
  }
  pca_values <- function(pca_data){
    pca_data <- as.data.frame(pca_data)
    if(length(input$variable) < 2){
      pca2 <- princomp(pca_data[,c("Worker","income")], cor = TRUE, scores = T)
    } else {
      pca2 <- princomp(pca_data[,input$variable], cor = TRUE, scores = T)
      pca2$loadings <- (pca2$loadings*-1)
      pca2$scores <- (pca2$scores*-1)
    }
    return(pca2)
  }
  scores_fun <- function(in_scores){
    scores.sy <- as.data.table(in_scores["scores"])[,1] + 100
    scores.sy <- data.table(scores.sy, TRACTCE = as.character(pca_data$tract_name2),
                         name = as.factor(pca_data$c_name),state_id = as.factor(pca_data$state_id))
    colnames(scores.sy)[1] <- "scores.sy"
    tracts.dt <- as.data.table(smp_tracts)
    tracts.dt$state_id <- as.factor(gsub("^0","",smp_tracts$STATEFP))
    result <- tracts.dt[scores.sy, on = c("TRACTCE", "state_id"),
                        allow.cartesian=TRUE]
    shiny_scores <- result[,mean(scores.sy,na.rm = T), by =c("STATEFP","COUNTYFP")]
    counties.dt <- as.data.table(smp_counties)
    shiny_scores <- counties.dt[shiny_scores, on = c("STATEFP","COUNTYFP"),
                                allow.cartesian=TRUE]
    shiny_scores <- st_as_sf(shiny_scores)
    gc()
    return(shiny_scores)
  }
  zoom <- function(region){
    if(region %in% county_scores$STATEFP){
      return(6)
  } else {
    return(4)
  }
  }

  #reactives
  values <- reactiveValues(df = NULL)


  pca <- reactive({
    pca_values(pca_data)
  })
  filtered <- reactive({
    #filtered <- scores_fun(pca_values(pca_data))
    if(substring(input$STATEFP,1,2) %in% county_scores$STATEFP){
      values$df <- scores_fun(pca_values(pca_data)) %>%
        filter(STATEFP == substring(input$STATEFP,1,2))
    }
    else {
        if(input$STATEFP == "WEST"){
          values$df <- scores_fun(pca_values(pca_data))
        }
    }
    return(values$df)

  })
  output$biplot <- renderPlot({
     ggbiplot_custom(pca(), obs.scale = 1, var.scale = 1,scale = 0.4, size = 0.1,
                              ellipse = TRUE, circle = TRUE, lable.size = 5) +
      scale_color_discrete(name = '') +
      theme(legend.direction = 'horizontal', legend.position = 'top') +
      xlim(-8,8) + ylim(-8,8)
  })
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addProviderTiles(providers$Hydda.Base) %>%
      setView(lng = -100 , lat = 40, zoom = 4)

  })

  observe({
    breaks.shiny <-classInt::classIntervals(filtered()$V1, n = 6,
                                          style = "pretty",intervalClosure = "left")
    pal <- colorNumeric("YlOrRd", domain = breaks.shiny$brks )
    content <- paste0(
      "<b>","Purchasing Power: ","</b>", format(filtered()$V1,digits = 4))

    leafletProxy("map", data = filtered()) %>%
      clearShapes() %>% addPolygons(highlightOptions = highlightOptions(color = "white", weight = 3,
                                                                        bringToFront = TRUE),
                                    fillColor = ~pal(V1),
                                    color = "white",
                                    opacity = 0.5,
                                    fillOpacity = 0.8,
                                    weight = 0.5,
                                    popup = content) %>%
      setView(lng = mean(sf::st_coordinates(filtered())[,1]) ,
              lat =mean(sf::st_coordinates(filtered())[,2]) ,
              zoom = zoom(substring(input$STATEFP,1,2)))
  })
  output$plot1 <- renderPlot({
    if (length(input$variable)<2){
      var <- c("Worker", "income")
      plots <- list()
      j <- 1
      for (i in var){
      tmp<- as.data.frame(pca_data) %>%
        filter(state_id == as.numeric(substring(input$STATEFP,1,2))) %>%
        select(i)
      plots[[j]] <- ggplot(tmp, aes_string(x = i)) +geom_histogram() + xlab(i)
      j = j + 1
      }
      print(multiplot(plotlist = plots, cols = 2))
      plots <- NULL
      tmp <- NULL
      } else {
        if (substring(input$STATEFP,1,2) %in% county_scores$STATEFP){
        var <- input$variable
        plots <- list()
        j <- 1
        for (i in var){
          tmp<- as.data.frame(pca_data) %>%
            filter(state_id == as.numeric(substring(input$STATEFP,1,2))) %>%
            select(i)
          plots[[j]] <- ggplot(tmp, aes_string(x = i)) +geom_histogram() + xlab(i)
          j = j + 1
        }
        print(multiplot(plotlist = plots, cols = 2))
        plots <- NULL
        tmp <- NULL
        gc() }
        else if (input$STATEFP == "WEST"){
          var <- input$variable
          plots <- list()
          j <- 1
          for (i in var){
            tmp<- as.data.frame(pca_data) %>%
              select(i)
            plots[[j]] <- ggplot(tmp, aes_string(x = i)) +geom_histogram() + xlab(i)
            j = j + 1
          }
          print(multiplot(plotlist = plots, cols = 2))
          plots <- NULL
          tmp <- NULL
          gc() 
       }
      }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

