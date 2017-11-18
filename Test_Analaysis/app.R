require(shiny)
require(bootstrap)
require(jpeg)
require(ggplot2)
require(ggExtra)
require(ggdendro)
require(DT)
require(stringr)
require(gsubfn)
require(proto)
require(sqldf)
require(shinyjs)
require(shinyBS)
require(shinydashboard)
require(plotly)
require(googleVis)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "SIBYL - Extrusion Application",
                 titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(id = "tabs",
                menuItem("Test Analysis", tabName = "testanalysis", icon = icon("home"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "testanalysis", 
              ###this is the test analysis tab for analyzing the MES Data
              
              fluidRow(
                column(
                  width = 4,
                  wellPanel(#a well panel to store the box for choosing the data
                    id = "datawellpanel", style = "overflow-y:scroll; max-height: 500px",
                    box(title = "Choose Data and Graph", solidHeader = TRUE, 
                        status = "primary", collapsible = TRUE, width = 12,
                        #radio button to choose the data set
                        radioButtons(inputId = "dataset",
                                     #'this controls the data set a user chooses
                                     label = "Choose a Data Set to Analyze",
                                     choiceNames = c("Single", "Multi","Tapered"),
                                     choiceValues = c(1,2,3)
                        ),
                        conditionalPanel(
                          #only appears once a data set has been chosen
                          condition = "input.dataset",
                          radioButtons(inputId = "graphpackage",
                                       #'this controls what type of graphs are available
                                       label = "Select a Chart/Graph Type",
                                       choiceNames = c("Google Plot (zoom works on Internet Explorer)",
                                                       "GGplot2",
                                                       "Plotly"),
                                       choiceValues = c(1,2,3)
                          )
                        ),#end conditionPanel of PD Custom
                        #'the selectinputs for the graph type are all the same, which package
                        #'is used to render the graph is determined by the choice value for the graph
                        #'it follows this pattern:
                        #'first integer -> graph group (1 = google, 2 = ggplot2, 3 = plotly)
                        #'second integeger -> special or non special graph (0 = not-special,
                        #'2 = dendrogram, 3 = parallel coordinates, 4 = google motion chart)
                        #'third integer -> degrees of freedom
                        #'fourth integer -> is color a degree of freedom (0 = no, 1 = yes)
                        #'fifth integer -> is size a degree of freedom (0 = no, 1 = yes)
                        #'sixth integer -> does the graph allow multiple traces (0 = no, 1 = yes)
                        #'7 and 8 integer -> graph ID (currently starts at 06).
                        
                        conditionalPanel(
                          condition = "input.graphpackage == '1'",
                          #this only appears of Google Plot is selected
                          selectInput(inputId = "graphtype",
                                      #'the regular 2D charts will be less than 10 for value
                                      #'the combo charts for two data sets per category will be
                                      #'between 10 - 20
                                      #'individual charts will be greater than 100
                                      #'3 
                                      label = "Select a Chart/Graph Type",
                                      choices = list("Scatter Chart" = 06,
                                                     "Line Chart" = 07,
                                                     "Line Chart with 2 Y-Axes" = 08,
                                                     "Bar Chart" = 09,
                                                     "Column Chart" = 10,
                                                     "Area Chart" = 11,
                                                     "Stepped Area Chart" = 12,
                                                     "Combo Chart" = 13,
                                                     "Bubble Chart" = 14,
                                                     "Pie Chart" = 15,
                                                     "Histogram" = 16,
                                                     "Motion Chart" = 17,
                                                     "Annotated Time Line Chart" = 18)
                          )
                        ),#end conditionPanel of Google Plot
                        conditionalPanel(
                          condition = "input.graphpackage == '2'",
                          #this only appears of GGplot2 is selected
                          selectInput(inputId = "graphtype",
                                      #'for a 2D graph on certain options are available
                                      #'if the value is greater than 10, it has 3 degress of freedom
                                      #'Marginal plots are in the 20s and have distributions along
                                      #'the axis
                                      #'
                                      label = "Select a Chart/Graph Type",
                                      choices = list("Scatter Plot" = 10200119,
                                                     "Counts Plot" = 20,
                                                     "Area Chart" = 21,
                                                     "Ordered Bar Chart" = 22,
                                                     "Histogram" = 23,
                                                     "Density Plot" = 24,
                                                     "Box Plot" = 25,
                                                     "Pie Chart" = 26,
                                                     "Bubble Plot" = 27,
                                                     "Tree Map" = 28,
                                                     "Marginal Histogram" = 29,
                                                     "Marginal Boxplot" = 30,
                                                     "Dendrogram" = 31,
                                                     "Cluster for PCA" = 32)
                          )
                        ),#end conditionPanel of GGplot2
                        conditionalPanel(
                          condition = "input.graphpackage == '3'",
                          #this only appears of PD custom is available
                          selectInput(inputId = "graphtype",
                                      #'for a 2D graph on certain options are available
                                      #'if the value is greater than 10, it has 3 axis,
                                      #'things such as a heat map and bubble plot
                                      label = "Select a Chart/Graph Type",
                                      choices = list("Scatter Plot" = 33,
                                                     "Line Plot" = 34,
                                                     "Filled Area Plot" = 35,
                                                     "Box Plot" = 36,
                                                     "Histogram" = 37,
                                                     "2D Histogram" = 38,
                                                     "Bubble Chart" = 39,
                                                     "Heat Map" = 40,
                                                     "Stacked Area Plot" = 41,
                                                     "3D Scatter Plot" = 42,
                                                     "3D Line Plot" = 43,
                                                     "3D Mesh Plot" = 44,
                                                     "3D Mesh Plot" = 45,
                                                     "Parallel Coordinates Plot" = 46)
                          )
                        )#end conditionPanel of Plotly
                        
                    )#end box for choosing data set and graph
                  )#end wellPanel
                  
                )#end column
              )
              
              
      ) #end tabItem for testanalysis
      
    )#end tabItems
  )#end dasboardBody
   
  
) #end UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$graphtype,{
    
    graphtypeid <- input$graphtype #gets the graph id that lets the program know what type of graph
    #it is
    
    is_special <- (substring(graphtypeid, 2, 2) != 0) #if it is not zero, it is special
    
    if (is_special){
      #if it is a special type of graph we will determine what type of graph
    }
    else{
      #it is not a special type of graph
      dof <- substring(graphtypeid, 3, 3) #gets the number of degrees of freedom
      has_color <- (substring(graphtypeid, 4, 4) == 1) #graph represents a degree of freedom
      #with colors
      has_size <- (substring(graphtypeid, 5, 5) == 1) #graph represents a degree of freedom
      #with size
      allows_multiple_traces <- (substring(graphtypeid, 6, 6) == 1) #graph supports multiple traces
      
      if (dof == 1){
        if (has_color){
          if (has_size){
            if(allows_multiple_traces){
              
            }
            else{
              
            }
          }
          else{
            if(allows_multiple_traces){
              
            }
            else{
              
            }
          }
        }
        else{
          if (has_size){
            if(allows_multiple_traces){
              
            }
            else{
              
            }
          }
          else{
            if(allows_multiple_traces){
              
            }
            else{
              
            }
          }
        }
      } #end else for dof == 1
      else if (dof == 2){
        
      }#end else for dof == 2
      else if (dof == 3){
        
      }#end else for dof == 3
      else if (dof == 4){
        
      }#end else for dof == 4
      else if (dof == 5){
        
      }#end else for dof == 5
      
    }
    
    
  })#end observeEvent(input$graphtype)
   
   
}#end server

# Run the application 
shinyApp(ui = ui, server = server)

