# @author Kyle Flett
# Copyright 2018 Kaiser Research Online
# https://secure.kaiserresearch.com

#plot
Sys.setenv("plotly_username" = "arcadia")
Sys.setenv("plotly_api_key" = "hkLjpovajXXHVEaHRPuF")
# generatemap()
# generates a map using given parameters and exports to the Charts folder
generatemap <- function(df, 
                        title, 
                        legendTitle, 
                        fileName, 
                        displayData, 
                        colorChoice, 
                        revScheme, 
                        myZAuto = TRUE, 
                        myMax = 0, 
                        myMin = 0, 
                        isInteractive = TRUE, 
                        tickPref = "",
                        tickSuf = "",
                        source, 
                        copyright, 
                        date, 
                        titleFontSize = 40, 
                        fontSize = 20, 
                        xdim = 1920, 
                        ydim = 1080,
                        dateloc,
                        sourceloc,
                        titleloc,
                        copyloc,
                        colorbarloc = c(1.02,.5),
                        topmarg,
                        bottommarg,
                        leftmarg,
                        rightmarg
){
  
  # libraries - some are not used but can be useful for color palettes
  library(stringr)
  library(orca)
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
    colorbar(
      title = legendTitle, 
      tickprefix = tickPref,
      ticksuffix = tickSuf,
      tickfont = list(size = fontSize, color = "black"), 
      len = .9, 
      xpad = 10, 
      ypad = 30, 
      outlinewidth = 2, 
      outlinecolor = "black", 
      ticklen = 10, 
      tickcolor = "black", 
      x = 1.02, 
      xanchor = 'left',
      y = .5,
      yanchor = 'middle'
    ) %>%
    layout(
      geo = g,
      margin = list(t = topmarg, r = rightmarg, l = leftmarg, b = bottommarg, pad = 0),
      font = list(family = 'helvetica', size = fontSize, color = "black")
    ) %>%
    add_annotations(
      x = sourceloc[1],
      y = sourceloc[2],
      xref = "paper",
      yref = "paper",
      text = paste("Source: ", source, sep = ""),
      showarrow = F
    ) %>%
    add_annotations(
      x = dateloc[1],
      y = dateloc[2],
      xref = "paper",
      yref = "paper",
      text = paste("Date: ", date, sep = ""),
      showarrow = F
    ) %>% 
    add_annotations(
      x = copyloc[1],
      y = copyloc[2],
      xanchor = 'left',
      xref = "paper",
      yref = "paper",
      text = copyright,
      showarrow = F
    ) %>% 
    add_annotations(
      font = list(size = titleFontSize),
      x = titleloc[1],
      y = titleloc[2],
      xref = "paper",
      yref = "paper",
      text = paste("<b>", title, "</b>", sep = ""),
      showarrow = F
    )
  
  if(isInteractive){
    # export graph as HTML file
    htmlwidgets::saveWidget(partial_bundle(as_widget(p)), fileName)
  } else {
    plotly_IMAGE(p, width = xdim, height = ydim, format = "png", out_file = fileName)
  }
  # Create a shareable link
  #chart_link = api_create(p, filename="World GDP 2017")
  #chart_link
  
}

generateradar <- function(myDf, myColors, mySave = TRUE){
  library(plotly)
  library(htmlwidgets)
  
  df <- testdata
  p <- plot_ly(df,
               type = 'scatterpolar',
               fill = 'toself',
               mode = 'lines'
  )
  i = 1
  while (i <= nrow(testdata)){
    p <- add_trace(p,
                   r = unlist(df[i,], use.names = FALSE),
                   theta = colnames(3:ncol(df)),
                   name = df[[2]]
    )
    i = i + 1
  }
  p <- layout(p,
              polar = list(
                radialaxis = list(
                  visible = T
                )
              ),
              showlegend = T
  )
  #plotly_IMAGE(p, format = "png", out_file = "output.png")
  htmlwidgets::saveWidget(as_widget(p), 'testpolarplot.html')
  # <- api_create(p, filename = "polar-charts-basic")
  #chart_link
}
# test graph for test purposes
#generatemap(gdp, '1995 PPP', 'PPP (LCU per $)', "graph.html", ~X1995, "Blues", TRUE, myZAuto = FALSE, myMin = 0, myMax = 20)

tabgui <- function(df){
  library(gWidgets2)
  library(gWidgets2tcltk)
  library(grDevices)
  library(colorRamps)
  library(RColorBrewer)
  library(htmlwidgets)
  library(plotly)
  library(xlsx)
  
  #default datafile import
  #df <- gdp
  
  win <- gwindow("KRO Graph Creator")
  nb <- gnotebook(container = win)
  group <- ggroup(label = "Heat Map Creator", container = nb)
  
  left <- ggroup(container = group, horizontal = FALSE)
  right <- ggroup(container = group, horizontal = FALSE)
  
  
  # not yet functional
  newfile <- gfilebrowse(text = "Select a file", container = left)
  # not yet functional
  btn_parse <- gbutton(
    text = "Use selected data file",
    container = right,
    handler = function(h, ...) {
      print(svalue(newfile))
      #gdp <- read.xlsx(svalue(newfile), sheetIndex = 1)
      #df <- gdp
      visible(win) <- FALSE
      source('C:/Kyle/R/RMapGenerator/RMapGen/R/tabgui.R')
      
    }
  )
  datacolumn <- gcombobox(colnames(df[1:ncol(df)]), container = left)
  title <- gedit("", initial.msg = "Enter a title here:", container = left)
  scaleval <- gedit("", initial.msg = "Colorbar scale text:", container = left)
  
  fileName <- gedit("graph.html", initial.msg = "File name:", container = left)
  dropdown <- gcombobox(c("Reds", "Blues", "Greens", "Purples", "Oranges", 'PuBuGn'), container = right)
  reversecols <- gcheckbox(text = "Reverse Colorscheme", checked = TRUE, container = right)
  autoscale <- gcheckbox(text = "Automatic scale values", checked = TRUE, container = left)
  maxVal <- gedit("", initial.msg = "Colorbar maximum value", container = left)
  minVal <- gedit("", initial.msg = "Colorbar minimum value", container = left)
  
  btn_create <- gbutton(
    text = "Create graph",
    container = right,
    handler = function(h, ...) {
      finalTitle <- (svalue(title))
      year <- svalue(datacolumn)
      generatemap(myData = df, finalTitle,svalue(scaleval), svalue(fileName), df[,c(year)], svalue(dropdown), svalue(reversecols), myZAuto = svalue(autoscale), myMin = svalue(minVal), myMax = svalue(maxVal))
    }
  )
  
  
  #svalue(nb) <- 1
  group2 <- ggroup(label = 'Mass Graph Creation', container = nb, horizontal = FALSE)
  
  presetcolumn <- gcombobox(c("Radar","Pie"), container = group2)
  newmassfile <- gfilebrowse(text = "Select a file", container = group2)
  creategraphs <- gbutton(
    "Create new graph",
    container = group2,
    handler = function(h, ...) {
      #TODO
    }
  )
  
  
  radarchart <- ggroup(label = 'Radar Chart Creation', container = nb, horizontal = FALSE)
  
  piechart <- ggroup(label = 'Pie Chart Creation', container = nb, horizontal = FALSE)
  
}

#takes 2-4 files and merges them into one usable file
ingest <- function(chartStyle, dataFile, fieldMap, code){
  library(stringr)
  # Dealing with merging df and field map
  cs <-  chartStyle
  df <- dataFile
  fm <- fieldMap
  # num <- ncol(fm)
  # cols <- c("LocationName", "LocationCode", fm[1,3:num])
  # colnames(df) <- cols
  #print(df)
  # Ingest of CS
  if(cs$ChartStyleType == "Map"){
    title <- cs$LDOutputTitle
    legend <-  cs$LDOutputLegendTitleX
    colors <-  as.character(cs$ChartStyleColorScheme)
    reverse  <-  binary2boolean(cs$ReverseColorScheme)
    ZAuto  <-  binary2boolean(cs$AutoScale)
    date <-  cs$LDOutputDate
    source <- cs$LDOutputSource
    copyright <- cs$LDOutPutCopyright
    crloc <-  str2coords(cs$CopyrightLocXY)
    srcloc <- str2coords(cs$SourceLocXY)
    dateloc <- str2coords(cs$DateLocXY)
    titleloc <- str2coords(cs$TitleLocXY)
    cbloc <- str2coords(cs$LegendLocation)
    xdim <- cs$DimensionX
    ydim <- cs$DimensionY
    t <- cs$MarginTop
    r <- cs$MarginRight
    l <- cs$MarginLeft
    b <- cs$MarginBottom
    max <- cs$LDOutputMaxScale
    min <- cs$LDOutPutMinScale
    titlesize <- cs$TitleSize
    fontsize <- cs$LegendSize
    tickpref <- ""
    ticksuf <- ""
    if(is.na(cs$LDOutputDisplayUnit) | cs$LDOutputDisplayUnit == "Numerical"){
      tickpref <- ""
    }else if(cs$LDOutputDisplayUnit == "Percentage"){
      ticksuf <- '%'
    }else if(cs$LDOutputDisplayUnit == "Currency"){
      tickpref <- "$"
    }
    interact <- binary2boolean(cs$IsLDOutputInteractive)
    
    # Generate map
    i <- 1
    fmlen <- (ncol(fm) - 2)
    
    while(i <= fmlen){
      title <- paste(cs$LDOutputTitle, fm[i+2])
      if(interact){
        output  <-  paste("C:/KROData/Charts/", code, "_", threedigitconvert(i), ".html", sep = "")
      } else {
        output  <-  paste("C:/KROData/Charts/", code, "_", threedigitconvert(i), ".png", sep = "")
      }
      generatemap(df, 
                  title, 
                  legend, 
                  output, 
                  df[,(i+2)], 
                  colors, 
                  revScheme = reverse, 
                  myZAuto = ZAuto, 
                  myMin = min, 
                  myMax = max, 
                  isInteractive = interact, 
                  tickPref = tickpref,
                  tickSuf = ticksuf,
                  source = source, 
                  copyright = copyright, 
                  date = date, 
                  dateloc = dateloc, 
                  sourceloc = srcloc, 
                  copyloc = crloc, 
                  titleloc = titleloc, 
                  colorbarloc = cbloc,
                  topmarg = t,
                  bottommarg = b,
                  leftmarg = l,
                  rightmarg = r,
                  xdim = xdim,
                  ydim = ydim,
                  titleFontSize = titlesize,
                  fontSize = fontsize)
      i <- i + 1
    }
  }
}

gui <- function(code = ""){
  library(gWidgets2)
  library(gWidgets2tcltk)
  library(grDevices)
  library(colorRamps)
  library(RColorBrewer)
  library(htmlwidgets)
  library(plotly)
  library(xlsx)
  library(stringr)
  
  win <- gwindow("KRO Graph Creator")
  group <- ggroup(container = win, horizontal = FALSE)
  
  stylegroup <- ggroup(container = group)
  datagroup <- ggroup(container = group)
  fmgroup <- ggroup(container = group)
  
  stylelabel <- glabel(text = "Chart Style:", container = stylegroup)
  importstyle <- gfilebrowse(text = "select a file", container = stylegroup, initial.dir = 'C:\\KROData\\Export', initial.filename = paste(code, "_000ChartStyle.csv", sep = ""))
  svalue(importstyle) <- paste("C:/KROData/Export/",code, "_000ChartStyle.csv", sep = "")
  
  datalabel <- glabel(text = "Data:           ", container = datagroup)
  importdata <- gfilebrowse(text = "select a file", container = datagroup, initial.dir = 'C:\\KROData\\Export', initial.filename = paste(code, "_000Data.csv", sep = ""))
  svalue(importdata) <- paste("C:/KROData/Export/",code, "_000Data.csv", sep = "")
  
  fmlabel <- glabel(text = "Field Map:   ", container = fmgroup)
  importfm <- gfilebrowse(text = "select a file", container = fmgroup, initial.dir = 'C:\\KROData\\Export', initial.filename = paste(code, "_000FieldMap.csv", sep = ""))
  svalue(importfm) <- paste("C:/KROData/Export/",code, "_000FieldMap.csv", sep = "")
  
  importbutton <- gbutton(
    text = "Import and Graph",
    container = group,
    handler = function(h, ...){
      code <- getLDCode(svalue(importdata))
      
      cs <- read.csv(svalue(importstyle))
      df <- read.csv(svalue(importdata))
      fm <- read.csv(svalue(importfm))
      ingest(cs, df, fm, code)
      gmessage(paste("Success! Graphs created at ", code, sep = ""), title = 'Import Successful.')
    }
  )
  #slider <- gslider(container = group)
}
#returns string of LD Code
getLDCode <- function(strName){
  index <- unlist(str_locate(strName, "LD"), use.names = F)
  str_sub(strName, index[1], (index[1] + 13))
}

binary2boolean <- function(x){
  if(x == 0){
    result <- FALSE
  } else if(x == 1){
    result <- TRUE
  } else {
    result <- NULL
  }
  return(result)
}

threedigitconvert <- function(x){
  if(str_length(x) < 1){
    result <- NULL
  } else if(str_length(x) < 2){
    result <- paste("00", x, sep = "")
  } else if(str_length(x) < 3){
    result <- paste("0", x, sep = "")
  } else {
    result <- x
  }
  return(result)
}

generatebar <- function(){
  library(plotly)
  
  month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
             'August', 'September', 'October', 'November', 'December')
  high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
  low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
  data <- data.frame(month, high_2014, low_2014)
  data$average_2014 <- rowMeans(data[,c("high_2014", "low_2014")])
  
  #The default order will be alphabetized unless specified as below:
  data$month <- factor(data$month, levels = data[["month"]])
  
  p <- plot_ly(data, x = ~month, y = ~high_2014, type = 'scatter', mode = 'lines',
               line = list(color = 'transparent'),
               showlegend = FALSE, name = 'High 2014') %>%
    add_trace(y = ~low_2014, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,1)', line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Low 2014') %>%
    layout(title = "Average, High and Low Temperatures in New York",
           paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "Months",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE),
           yaxis = list(title = "Temperature (degrees F)",
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        zeroline = FALSE)
    )
  p
}

#Takes input of a string in the form of "1,1" and outputs as a list of coordinates
str2coords <- function(x){
  library(stringr)
  commapos <- unlist(str_locate(x, ","), use.names = FALSE)
  xcoord <- str_sub(x, start = 0, end = commapos[1]- 1)
  ycoord <- str_sub(x, start = commapos[2]+ 1, end = -1L)
  output <- c(as.numeric(xcoord), as.numeric(ycoord))
  return(output)
}
