
library(shinydashboard)
library(shiny)
library(googleVis)
library(dplyr)
library(lubridate)
library(DT)
library(readr)
library(ggplot2)


##### Month Graph-March2016####

server <- function(input, output){
  
    output$monthly_sales_graph <- renderGvis({
    msales<- select(data,day,sales)
    msalechart<-gvisLineChart(msales,xvar="day",yvar="sales")
    return(msalechart)
    
  })
  
  ##### Year Graph 2016 ###
  output$Yearly_sales_graph <- renderGvis({
    ysales<- select(y2015,Month,Sales)
    ysalechart<-gvisColumnChart(ysales,xvar="Month",yvar="Sales",options=list(colors="['#008000']"))
    
    return(ysalechart)
    
  })
  output$rvcgraph <- renderGvis({
    crvalue<- select(Rbycatval,category_id,Revenue)
    crchart<-gvisLineChart(crvalue,xvar="category_id",yvar="Revenue",options=list(colors="['#008000']"))
    
    return(crchart)
    
  })
  output$sales_Location_graph <- renderGvis({
    # locsales<- select(salelocval,Location,Revenue)
    # locsalechart<-gvisPieChart(locsales)
    # 
    # return(locsalechart)
    mlocsales <- salelocval 
    msaleloc <- na.omit(mlocsales) 
    mgeostate <- gvisGeoChart(msaleloc,"Location","Revenue",options=list(region="US",displayMode="regions",resolution="provinces"))
    # plot(mgeostate)
    return(mgeostate)
    
  })
  output$sales_Location_graph_Year <- renderGvis({
    # ylocsales<- select(salelocvaly,Location,Revenue)
    # ylocsalechart<-gvisPieChart(ylocsales)
    # 
    # return(ylocsalechart)
    ylocsales <- salelocvaly 
    ysaleloc <- na.omit(ylocsales) 
    ygeostate <- gvisGeoChart(ysaleloc,"Location","Revenue",options=list(region="US",displayMode="regions",resolution="provinces"))
    return(ygeostate)
    
  })
  # output$sales_Location_Total_Year <- renderGvis({
  #   # yallocsales<- select(salelocvalyall,Location,Revenue)
  #   # yallocsalechart<-gvisPieChart(yallocsales)
  #   # // saved in to new dataframe just for my work purpose
  #   saleloc <- salelocvalyall 
  #   saleloc <- na.omit(saleloc) 
  #   geostate <- gvisGeoChart(saleloc,"Location","Revenue",options=list(region="US",displayMode="regions",resolution="provinces"))
  #   plot(geostate)
  #   return(geostate)
  #   
  # })
  output$Month_sales_graph_everyYear <- renderGvis({
    marchrevenue<- select(RevenueMarchVal,Revenue,Year)
    marchchart<-gvisColumnChart(marchrevenue,xvar = "Year",yvar = "Revenue",options=list(colors="['#008000']"))
    
    return(marchchart)
    
  })
  output$year_wise_revenue<- renderGvis({
    yRsales<- select(RevenueByYear,Year,Revenue)
    yRsalechart<-gvisColumnChart(yRsales,xvar = "Year",yvar = "Revenue",options=list(colors="['#008000']"))
    return(yRsalechart)
    
  })
  output$Qty_Sold_loc<- renderGvis({
    # ilsales<- select(InventsalesbyRegion,Revenue,Location)
    InventorysalesbyRegion <- InventsalesbyRegion
    ilsales <- na.omit(InventorysalesbyRegion)
    ilgeostate <- gvisGeoChart(ilsales,"Location","Revenue",options=list(region="US",displayMode="regions",resolution="provinces"))
    return(ilgeostate)
    
  })
  output$salespricing<- renderGvis({
    unitssoldandship<- select(unitssold,Month,UnitsOrderd,UnitsShipped)
    Month<-unitssold$Month
    UnitsSold<-unitssold$UnitsOrderd
    UnitsShipped<-unitssold$UnitsShipped
    df<-data.frame(Month,UnitsSold,UnitsShipped)
    # unitssell<-gvisComboChart(unitssoldandship,xvar="UnitsOrderd",yvar="UnitsShipped")
    unitssell<-gvisColumnChart(df)
    
    return(unitssell)
    
  })
  
  output$revenueBox <- renderValueBox({
    valueBox(
      paste(round(Revenueval/1000000,2),"M" ), "Revenue", icon = icon("glyphicon glyphicon-usd",lib="glyphicon"),
      color = "orange"
    )
  })
  
  output$average_order_valueBox <- renderValueBox({
    valueBox(
      paste(round(Avg_Value,2),"$"), "AverageOrderValue", icon = icon("glyphicon glyphicon-usd",lib="glyphicon"),
      color = "fuchsia"
    )
    
  })
  ##No Of Visits Per Day Value Box####
  output$dVisitsBox <- renderValueBox({
    valueBox(
      paste(dVisitsperday), "Visits/Day",icon = icon("glyphicon glyphicon-usd",lib="glyphicon"),
      color = "fuchsia"
      )
  })
  
  output$returning_customersBox <- renderValueBox({
    valueBox(
      paste(RepeatVal), "returning customers", icon = icon("glyphicon glyphicon-scale",lib="glyphicon"),
      color = "teal"
    )
    
  })
  
  output$Eratio <- renderInfoBox({
    infoBox(
      "Ecommerce Conversion Ratio",paste(round(ERatio,4),"%"),
      color = "purple",fill = TRUE,icon = icon("credit-card")
    )
    
  })
  
  output$InventoryBox <- renderInfoBox({
    infoBox(
      " Averge Inventory", paste( Avg_Value,"K"),icon=icon("tree"),
      color = "blue",fill = TRUE
    )
  })
  
  #### Top Selling Product of Month #####
  output$TopSellingProduct<- renderInfoBox({
    infoBox(
      "CVC Crew 6210 - Black L has",paste(maxQty/1000,"K sales"),icon=icon("glyphicon glyphicon-apple",lib="glyphicon"),
      color = "red",fill = TRUE
    )
  }) ## Close 
  
  
  output$inventurn<- renderInfoBox({
    infoBox(
      "Inventory Turnover",paste(round(inventurnover/254480.8,2)),icon = icon("glyphicon glyphicon-usd",lib="glyphicon"),
      color = "green",fill = TRUE
    )
  })
  
### Total units sold for Month ###
  output$units <- renderValueBox({
  infoBox(
    "Units/Transaction",paste(unitspertransaction),icon = icon("thumbs-up", lib = "glyphicon"),
    color = "orange",fill=TRUE
  )
}) ### Close..
}