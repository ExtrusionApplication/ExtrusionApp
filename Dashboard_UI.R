require(shiny)
require(bootstrap)
require(jpeg)
require(ggplot2)
require(DT)
require(stringr)
require(gsubfn)
require(proto)
require(sqldf)
require(shinyjs)
require(shinyBS)
library(shinydashboard)


#_s:name of the checkbox
#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

ui<-dashboardPage(
  dashboardHeader("SIBYL - Extrusion Application"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Part Catalog", tabName = "partcatalog", icon = icon("list")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    ) #end sidebarMenu
  ), #end dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem()#end tabItem
    )#end tabItems
  )#end dashboardbody
  
) #end UI