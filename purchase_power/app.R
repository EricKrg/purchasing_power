#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(tidyverse, sf, sp, acs, mapview, tigris, data.table, ggbiplot, leaflet)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Purchase power"),
   
   # Sidebar with a slider input for number of bins 

      sidebarPanel(
        sidebarPanel(
          selectInput('STATEFP', 'Statefp',unique(county_scores$STATEFP))),
         sliderInput("var",
                     "Number of var:",
                     min = 1,
                     max = 12,
                     value = 30)),
     
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("biplot"),
         tableOutput('table') 
      ),
   leafletOutput
   ("map")
   )
   


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #pca_data <- readRDS("./pca_data.Rds")
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
  pca_values <- function(pca_data){
    pca2 <- princomp(pca_data[,c(1:input$var)], cor = TRUE, scores = T)
    pca2$loadings <- (pca2$loadings*-1)
    pca2$scores <- (pca2$scores*-1)
    return(pca2)
  }
  scores_fun <- function(in_scores){
    scores <- as.data.frame(in_scores["scores"])[,1] + 100
    scores <- data.frame(scores, TRACTCE = as.character(pca_data$tract_name2),
                         name = as.factor(pca_data$c_name),state_id = as.factor(pca_data$state_id))
    
    tracts$state_id <- as.factor(gsub("^0","",tracts$STATEFP))
    print("test1")
    tract_scores <- left_join(tracts,scores, by = c("TRACTCE", "state_id"))
    print("test2")
    data_t <- data.table(tract_scores)
    shiny_scores <- data_t[,mean(scores,na.rm = T), by =c("STATEFP","COUNTYFP")] # aggregation with data table
    shiny_scores <- left_join(counties, shiny_scores)
    print("joined")
    return(shiny_scores)
  }

  #reactives
  values <- reactiveValues(df = NULL)
  
  
  pca <- reactive({
    pca_values(pca_data)
  })
  filtered <- reactive({
    #filtered <- scores_fun(pca_values(pca_data))
    values$df <- scores_fun(pca_values(pca_data)) %>%
      filter(STATEFP == input$STATEFP)
  })
  output$biplot <- renderPlot({
     ggbiplot_custom(pca(), obs.scale = 1, var.scale = 1,scale = 0.4, size = 0.1,
                              ellipse = TRUE, circle = TRUE, lable.size = 5) +
      scale_color_discrete(name = '') +
      theme(legend.direction = 'horizontal', legend.position = 'top') +
      xlim(-8,8) + ylim(-8,8)
  })
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addProviderTiles(providers$Hydda.Base) %>%
      setView(lng = -100 , lat = 40, zoom = 4) 
  })

  observe({
    breaks.shiny <-classInt::classIntervals(filtered()$V1, n = 6,
                                          style = "pretty",intervalClosure = "left")
    pal <- colorNumeric("YlOrRd", domain = breaks.shiny$brks )
    content <- paste(
      "<b>","Titel",": ","</b>", filtered()$V1)
    
    leafletProxy("map", data = filtered()) %>%
      clearShapes() %>% addPolygons(highlightOptions = highlightOptions(color = "red", weight = 2,
                                                                        bringToFront = TRUE),
                                    fillColor = ~pal(V1),
                                    color = "white",
                                    weight = 0.5,
                                    popup = content)
  })
  output$table <- renderTable(values$df)
  # output$viewData <- renderTable({
  #  filtered()
  # })
  
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

