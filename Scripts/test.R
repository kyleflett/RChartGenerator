# @author Kyle Flett
# Kaiser Research Online
# https://secure.kaiserresearch.com



generatemap <- function(df, title, legendTitle, fileName, displayData, colorChoice, revScheme, myZAuto = TRUE, myMax = 0, myMin = 0){
  
  # libraries - some are not used but can be useful for color palettes
  # kyle is a big dingus XD
  
  library(grDevices)
  library(colorRamps)
  library(RColorBrewer)
  library(htmlwidgets)
  library(plotly)
  library(xlsx)
  
  # global variables
  myColors <- colorChoice
  
  # black boundaries
  l <- list(color = toRGB("black"), width = 1)
  
  # specify map projection/options
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type =  "tranverse mercator" ),
    showocean = TRUE,
    oceancolor = 'lightblue',
    bgcolor = 'white'
  )
  
  
  # plot function
  p <- plot_geo(df) %>%
    add_trace(
      z = displayData, color = displayData, colors = myColors, text = ~LocationName,
      locations = ~LocationCode, marker = list(line = l),
      zmax = myMax, reversescale = revScheme, zmin = myMin, zauto = myZAuto
    ) %>%
    colorbar(title = legendTitle, tickprefix = '$') %>%
    layout(
      title = title,
      geo = g
    ) %>%
    add_annotations(
      x=0,
      y =0,
      xref = "paper",
      yref = "paper",
      text = "Source: ",
      showarrow = F
    )
  
  
  # export graph as HTML file
  htmlwidgets::saveWidget(as_widget(p), fileName)
  # Create a shareable link
  #chart_link = api_create(p, filename="World GDP 2017")
  #chart_link
  
}

# test graph for test purposes
generatemap(df, '1995 PPP', 'PPP (LCU per $)', "graph.html", ~Field001, "Blues", FALSE, myZAuto = TRUE, myMin = 0, myMax = 1000000000)

