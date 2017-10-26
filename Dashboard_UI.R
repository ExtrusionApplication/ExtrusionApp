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
  dashboardHeader(title = "SIBYL - Extrusion Application",
                  titleWidth = 350
                  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Part Catalog - PPS Data", tabName = "partcatalog", icon = icon("list"),
               menuSubItem("Single Layer Extrusion", tabName = "singleppstab"),
               menuSubItem("Multi Layer Extrusion", tabName = "multippstab"),
               menuSubItem("Tapered Extrusion", tabName = "taperedppstab")
        ), #end menuItem
      menuItem("Sampling and Test Method Information", icon = icon("wrench"),
               menuSubItem("Single Layer Extrusion", tabName = "singlesamplingtab"),
               menuSubItem("Multi Layer Extrusion", tabName = "multisamplingtab"),
               menuSubItem("Tapered Extrusion", tabName = "taperedsamplingtab"),
               menuSubItem("Extra Extrusion", tabName = "extrasamplingtab"),
               menuSubItem("Total Extrusion", tabName = "totalsamplingtab")
        ), #end menuItem
      menuItem("Single Layer Extrusion - MES Batch Data", icon = icon("cogs"),
               menuSubItem("MES Parameters and Yield", tabName = "singlemesparametersandyieldtab"),
               menuSubItem("MES Parameters", tabName = "singlemesparameterstab"),
               menuSubItem("MES Timestamps", tabName = "singlemestimetab"),
               menuSubItem("MES Submitters", tabName = "singlemessubmitterstab"),
               menuSubItem("MES Total", tabName = "singlemestotaltab")
      ), #end menuItem
      menuItem("Multi Layer Extrusion - MES Batch Data", icon = icon("cogs"),
               menuSubItem("MES Parameters and Yield", tabName = "multimesparametersandyieldtab"),
               menuSubItem("MES Parameters", tabName = "multimesparameterstab"),
               menuSubItem("MES Timestamps", tabName = "multimestimetab"),
               menuSubItem("MES Submitters", tabName = "multimessubmitterstab"),
               menuSubItem("MES Total", tabName = "multimestotaltab")
      ), #end menuItem
      menuItem("Tapered Extrusion - MES Batch Data", icon = icon("cogs"),
               menuSubItem("MES Parameters and Yield", tabName = "taperedmesparametersandyieldtab"),
               menuSubItem("MES Parameters", tabName = "taperedmesparameterstab"),
               menuSubItem("MES Timestamps", tabName = "taperedmestimetab"),
               menuSubItem("MES Submitters", tabName = "taperedmessubmitterstab"),
               menuSubItem("MES Total", tabName = "taperedmestotaltab")
      ), #end menuItem
      menuItem("Scrap Rates and Codes", icon = icon("chain-broken"),
               menuSubItem("Single Layer Extrusion", tabName = "singlescrapcodestab"),
               menuSubItem("Multi Layer Extrusion", tabName = "multiscrapcodestab"),
               menuSubItem("Tapered Extrusion", tabName = "taperedscrapcodestab")
      ),
      menuItem("Extra Information", icon = icon("book"),
               menuSubItem("Resin Information", tabName = "resininfotab"),
               menuSubItem("Screw Print Information", tabName = "screwinfotab")
      ),
      menuItem("Shopping Cart PPS Data", icon = icon("clone"),
               menuSubItem("Single Layer Extrusion", tabName = "singleshoppingcarttab"),
               menuSubItem("Multi Layer Extrusion", tabName = "multishoppingcarttab"),
               menuSubItem("Tapered Extrusion", tabName = "taperedshoppingcarttab"),
               menuSubItem("Total Extrusion", tabName = "totalshoppingcarttab")
      ), #end menuItem
      menuItem("Analysis Tool",icon=icon("bar-chart"),
               menuSubItem("MES Data Analysis",tabName = "MESDataAnalysis"),
               menuSubItem("Scrap Analysis",tabName = "ScrapAnalysis"),
               menuSubItem("Financial Data",tabName = "FinancialDataAnalysis")
      )#end menuItem
    ) #end sidebarMenu
    
  ), #end dashboardSidebar
  
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "singleppstab",
              #Part Resin
              fluidRow(
                box(title = "Part, Resin, and PPS - Numbers and Descriptions", 
                    solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                    #Part Number
                    column(2, align = "center", style='padding-left: 20px; padding-right:20px;',
                           
                           fluidRow(checkboxInput("PCSPN_d", "Part Number", value = TRUE)),  #Show the checkbox for Part number. it will return a True/False value
                           fluidRow(
                             conditionalPanel(
                               condition="input.PCSPN_d",   #If it were Ture, then there will have a search box for Part Number under checkbox
                               selectInput("PCSPN",label = "Select One",
                                           c("All",unique(as.character(single_pps_data$`Part Number`))))
                               
                             ))),
                    # Part Description
                    column(2, align = "center", style='padding-left: 20px; padding-right:20px;',
                           fluidRow(checkboxInput("PCSPD_d", "Part Description", value = TRUE)),
                           fluidRow(
                             conditionalPanel(
                               condition = "input.PCSPD_d",
                               selectInput("PCSPD",label = "Select One",
                                           c("All",unique(as.character(single_pps_data$`Part Description`))))
                             ))), 
                    # Resin Number
                    column(2, align = "center", style='padding-left: 20px; padding-right:20px;',
                           fluidRow(checkboxInput("PCSRN_d","Resin Number",value=TRUE)),
                           fluidRow(
                             conditionalPanel(
                               condition = "input.PCSRN_d",
                               selectInput("PCSRN",label = "Select One",
                                           c("All",unique(as.character(single_pps_data$`Resin Number`))))
                             ))),
                    #Resin Description
                    column(2, align = "center", style='padding-left: 20px; padding-right:20px;',
                           fluidRow(checkboxInput("PCSRD_d","Resin Description",value=TRUE)),
                           fluidRow(
                             conditionalPanel(
                               condition = "input.PCSRD_d",
                               selectInput("PCSRD",label = "Select One",
                                           c("All",unique(as.character(single_pps_data$`Resin Description`))))
                             ))),
                    #PPS Number
                    column(2, align = "center", style='padding-left: 20px; padding-right:20px;',
                           fluidRow(checkboxInput("PCSPPSN_d","PPS Number",value=F)),
                           fluidRow(
                             conditionalPanel(
                               condition = "input.PCSPPSN_d",
                               selectInput("PCSPPSN",label = "Select One",
                                           c("All",unique(as.character(single_pps_data$`PPS Number`))))
                             )))
                    )
              ),
              #Resin Information
              fluidRow(
                box(title = "Resin Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Resin Families
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRF_d","Resin Families",value=F))
                      ), 
                      #Is Resin Blended with Anything?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRBQ_d","Is Resin Blended with Anything?",value=F))
                      ), 
                      #Is Resin a Polymer Blend?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRPBQ_d","Is Resin a Polymer Blend?",value=F))
                      ),
                      #Is Resin Filled?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRFQ_d","Is Resin Filled?",value=F))
                      ),
                      #Resin Fillers
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRFi_d","Resin Fillers",value=F))
                      ),
                      #Is Resin Colored?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRCQ_d","Is Resin Colored?",value=F))
                      ),
                      #Resin Color
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRC_d","Resin Color",value=F))
                      ),
                      #Is Resin Radiopaque?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRRQ_d","Is Resin Radiopaque?",value=F))
                      ),
                      #Resin Durometer
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRDu_d","Resin Durometer (D)",value=F))
                      ),
                      #Average Resin Durometer
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSRADu_d","Average Durometer (D)",value=F))
                      )
                    ),#end Resin Information
                fluidRow(
                  #fluid row for showing or deleting buttons
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("checksingleresininfo", "Show All Resin Information")
                  ),
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("unchecksingleresininfo", "Hide All Resin Information")
                  )
                )
                )
              ),
              
              
              #Tooling
              fluidRow(
                box(title = "Tooling Parameters", 
                    solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Die Size
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSDS_d","Die Size (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSDS_d",
                                 column(6,numericInput("PCSDS_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSDSmin, ")"),
                                                       value=PCSDSmin,step=0.001)),
                                 column(6,numericInput("PCSDS_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSDSmax, ")"),
                                                       value=PCSDSmax,step=0.001))
                               )
                             )
                      ), #end column
                      #Die Land Length
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSDLL_d","Die Land Length (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSDLL_d",
                                 selectInput("PCSDLL",label = "Select One",c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
                               ))),
                      #Tip Size
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSTS_d","Tip Size (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSTS_d",
                                 column(6,numericInput("PCSTS_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSTSmin, ")"),
                                                       value=PCSTSmin,step=0.001)),
                                 column(6,numericInput("PCSTS_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSTSmax, ")"),
                                                       value=PCSTSmax,step=0.001))
                               )
                             )
                      ), #end column
                      #Tip Land Length
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSTLL_d","Tip Land Length (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSTLL_d",
                                 selectInput("PCSTLL",label = "Select One",c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
                               ))),
                      #Screw Print
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSSP_d","Screw Print",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSSP_d",
                                 selectInput("PCSSP",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`Screw Print`))))
                               )))
                    ),#end Tooling
                fluidRow(
                  #fluid row for showing or deleting buttons
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("checksingletooling", "Show All Tooling")
                  ),
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("unchecksingletooling", "Hide All Tooling")
                  )
                )
                )
                ),
              
              #Processing Attributes
              fluidRow(
                box(title = "Processing Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Feedthroat
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSFT_d","Feedthroat Temperature F",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSFT_d",
                                 column(6,numericInput("PCSFT_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSFTmin, ")"),
                                                       value = PCSFTmin,step=1)),
                                 column(6,numericInput("PCSFT_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSFTmax, ")"),
                                                       value = PCSFTmax,step=1))
                               )
                             )
                      ),
                      #Barrel Zone 1
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSBZT1_d","Barrel Zone 1 Temperature F",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSBZT1_d",
                                 column(6,numericInput("PCSBZT1_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSBZT1min, ")"),
                                                       value=PCSBZT1min,step=5)),
                                 column(6,numericInput("PCSBZT1_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSBZT1max, ")"),
                                                       value=PCSBZT1max,step=5))
                               )
                             )
                      ),
                      #Barrel ZOne2
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSBZT2_d","Barrel Zone 2 Temperature F",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSBZT2_d",
                                 column(6,numericInput("PCSBZT2_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSBZT2min, ")"),
                                                       value=PCSBZT2min,step=5)),
                                 column(6,numericInput("PCSBZT2_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSBZT2max, ")"),
                                                       value=PCSBZT2max,step=5))
                               )
                             )
                      ),
                      #Barrel Zone3
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSBZT3_d","Barrel Zone 3 Temperature F",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSBZT3_d",
                                 column(6,numericInput("PCSBZT3_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSBZT3min, ")"),
                                                       value=PCSBZT3min,step=5)),
                                 column(6,numericInput("PCSBZT3_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSBZT3max, ")"),
                                                       value=PCSBZT3max,step=5))
                               )
                             )
                      )
                  ),#end Processing Attribute 1
                fluidRow(
                  #Clamp Temperature F
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSCT_d","Clamp Temperature F",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSCT_d",
                             column(6,numericInput("PCSCT_min",
                                                   label = paste0("Insert Minimum Value ", "(Minimum is ", PCSCTmin, ")"),
                                                   value=PCSCTmin,step=5)),
                             column(6,numericInput("PCSCT_max",
                                                   label = paste0("Insert Maximum Value ", "(Maximum is ", PCSCTmax, ")"),
                                                   value=PCSCTmax,step=5))
                           )
                         )
                  ),
                  #Adapter Temperature F
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSAT_d","Adapter Temperature F",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSAT_d",
                             column(6,numericInput("PCSAT_min",
                                                   label = paste0("Insert Minimum Value ", "(Minimum is ", PCSATmin, ")"),
                                                   value=PCSATmin,step=5)),
                             column(6,numericInput("PCSAT_max",
                                                   label = paste0("Insert Maximum Value ", "(Maximum is ", PCSATmax, ")"),
                                                   value=PCSATmax,step=5))
                           )
                         )
                  ),
                  #Die 1 Temperature F
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSDT1_d","Die 1 Temperature F",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSDT1_d",
                             column(6,numericInput("PCSDT1_min",
                                                   label = paste0("Insert Minimum Value ", "(Minimum is ", PCSDT1min, ")"),
                                                   value=PCSDT1min,step=5)),
                             column(6,numericInput("PCSDT1_max",
                                                   label = paste0("Insert Maximum Value ", "(Maximum is ", PCSDT1max, ")"),
                                                   value=PCSDT1max,step=5))
                           )
                         )
                  ),
                  #Die 2 Temperature F
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSDT2_d","Die 2 Temperature F",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSDT2_d",
                             column(6,numericInput("PCSDT2_min",
                                                   label = paste0("Insert Minimum Value ", "(Minimum is ", PCSDT2min, ")"),
                                                   value=PCSDT2min,step=5)),
                             column(6,numericInput("PCSDT2_max",
                                                   label = paste0("Insert Maximum Value ", "(Maximum is ", PCSDT2max, ")"),
                                                   value=PCSDT2max,step=5))
                           )
                         )
                  )
                ), #end Processing Attribute 
                fluidRow(
                  #fluid row for showing or deleting buttons
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("checksingleparameters", "Show All Processing Parameters")
                  ),
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("unchecksingleparameters", "Hide All Processing Parameters")
                  )
                )
                )
                ),
              
              #Dimentional Attribute
              fluidRow(
                box(title = "Dimensional Attributes", 
                    solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Inner Diameter
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSIDI_d","Inner Diameter (in)",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSIDI_d",
                                 column(6,numericInput("PCSIDI_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSIDImin, ")"),
                                                       value=PCSIDImin,step=0.001)),
                                 column(6,numericInput("PCSIDI_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSIDImax, ")"),
                                                       value=PCSIDImax,step=0.001))
                               )
                             )
                      ),
                      #Outer Diameter
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSODI_d","Outer Diameter (in)",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSODI_d",
                                 column(6,numericInput("PCSODI_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSODImin, ")"),
                                                       value=PCSODImin,step=0.001)),
                                 column(6,numericInput("PCSODI_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSODImax, ")"),
                                                       value=PCSODImax,step=0.001))
                               )
                             )
                      ),
                      #Wall Thickness
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSWT_d","Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSWT_d",
                                 column(6,numericInput("PCSWT_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSODImin, ")"),
                                                       value=PCSODImin,step=0.001)),
                                 column(6,numericInput("PCSWT_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSODImax, ")"),
                                                       value=PCSODImax,step=0.001))
                               )
                             )
                      ),
                      #Out of Roundness (in)
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSOR_d","Out of Roundness (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSOR_d",
                                 column(6,numericInput("PCSOR_min",
                                                       label = paste0("Insert Minimum Value ", "(Minimum is ", PCSODImin, ")"),
                                                       value=PCSODImin,step=0.001)),
                                 column(6,numericInput("PCSOR_max",
                                                       label = paste0("Insert Maximum Value ", "(Maximum is ", PCSODImax, ")"),
                                                       value=PCSODImax,step=0.001))
                               )
                             )
                      )
                    ),
                fluidRow(
                  #Concentricity
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSCCT_d","Concentricity",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSCCT_d",
                             column(6,numericInput("PCSCCT_min",
                                                   label = paste0("Insert Minimum Value ", "(Minimum is ", PCSCCTmin, ")"),
                                                   value=PCSCCTmin,step=0.0001)),
                             column(6,numericInput("PCSCCT_max",
                                                   label = paste0("Insert Maximum Value ", "(Maximum is ", PCSCCTmax, ")"),
                                                   value=PCSCCTmax,step=0.0001))
                           )
                         )
                  ),
                  #Length
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSLength_d","Length (in)",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSLength_d",
                             column(6,numericInput("PCSLength_min",
                                                   label = paste0("Insert Minimum Value ", "(Minimum is ", PCSLengthmin, ")"),
                                                   value=PCSLengthmin,step=1)),
                             column(6,numericInput("PCSLength_max",
                                                   label = paste0("Insert Maximum Value ", "(Maximum is ", PCSLengthmax, ")"),
                                                   value=PCSLengthmax,step=1))
                           )
                         )
                  ),
                  #Perpendicularity
                  column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                         fluidRow(checkboxInput("PCSPPD_d","Perpendicularity (in)",value=F)),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.PCSPPD_d",
                             selectInput("PCSPPD",label = "Select One",
                                         c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
                           )))), #End Dimentional Attribute
                fluidRow(
                  #fluid row for showing or deleting buttons
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("checksingledimensions", "Show All Dimensional Attributes")
                  ),
                  column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                         actionButton("unchecksingledimensions", "Hide All Dimensional Attributes")
                  )
                )
                )
              ),
              #Special Operation
              fluidRow(
                box(title = "Special Operations", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSNEXIV_d","NEXIV",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSNEXIV_d",
                                 selectInput("PCSNEXIV",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSAnnealed_d","Annealed",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSAnnealed_d",
                                 selectInput("PCSAnnealed",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSCaliper_d","Caliper",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSCaliper_d",
                                 selectInput("PCSCaliper",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSOS_d","OD Sort",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSOS_d",
                                 selectInput("PCSOS",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSMP_d","Melt Pump",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSMP_d",
                                 selectInput("PCSMP",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSHT_d","Hypo Tip",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSHT_d",
                                 selectInput("PCSHT",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSSPD_d","Sparker Die",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSSPD_d",
                                 selectInput("PCSSPD",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSSLD_d","Slicking Die",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSSLD_d",
                                 selectInput("PCSSLD",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSDLN_d","Delamination",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSDLN_d",
                                 selectInput("PCSDLN",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSULT_d","Ultrasonic",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSULT_d",
                                 selectInput("PCSULT",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSVC_d","Vacuum Calibration",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSVC_d",
                                 selectInput("PCSVC",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCSIRD_d","Irradiated",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCSIRD_d",
                                 selectInput("PCSIRD",label = "Select One",choices=c("All","yes","NA"))
                               )))
                    ), #end Special Operation
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checksinglespecial", "Show All Special Operations")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("unchecksinglespecial", "Hide All Special Operations")
                      )
                    )
                )
              ),
              
              # Show Table
              fluidRow(
                box(title = "Part Listings", 
                    solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                    DT::dataTableOutput("mytable1")
                )
              ),
              fluidRow(
                box(title = "Special Operations", 
                    solidHeader = TRUE, status = "warning", collapsible = TRUE, width = 12,
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             downloadButton('singledownloadSPPSData','Download Single PPS Data')
                      ),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             downloadButton('singledownloadSPPSDataAll','Download Single PPS Data with All Parameters')
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton('resetsingleinputs', "Reset Single Input Parameters")
                      )
                    )
                )
              )
              
      ),#end tabItem
      tabItem(tabName = "multippstab",
              #Part Resin
              fluidRow(
                box(title = "Part, Resin, and PPS - Numbers and Descriptions", 
                    solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Part Number
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMPN_d","Part Number",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition="input.PCMPN_d",
                                 
                                 selectInput("PCMPN",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`Part Number`))))
                                 
                               ))),
                      # Part Description
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMPD_d","Part Description",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMPD_d",
                                 selectInput("PCMPD",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`Part Description`))))
                               ))), 
                      # Resin Number
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRN_d","Resin Number",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMRN_d",
                                 selectInput("PCMRN",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`Resin Number`))))
                               ))),
                      #Resin Description
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRD_d","Resin Description",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMRD_d",
                                 selectInput("PCMRD",label = "Select One",
                                             
                                             c("All",unique(as.character(multi_pps_data$`Resin Description`))))
                                 
                               ))),
                      #PPS Number
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMPPSN_d","PPS Number",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMPPSN_d",
                                 selectInput("PCMPPSN",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`PPS Number`))))
                                 
                               )))
                      
                    )
                )
              ),
              
              #Resin Information
              fluidRow(
                box(title = "Resin Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      tags$h1(strong("Resin Information"),style="font-size:25px;",align="left"),
                      #Resin Families
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRF_d","Resin Families",value=F))
                      ), 
                      #Is Resin Blended with Anything?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRBQ_d","Is Resin Blended with Anything?",value=F))
                      ), 
                      #Is Resin a Polymer Blend?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRPBQ_d","Is Resin a Polymer Blend?",value=F))
                      ),
                      #Is Resin Filled?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRFQ_d","Is Resin Filled?",value=F))
                      ),
                      #Resin Fillers
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRFi_d","Resin Fillers",value=F))
                      ),
                      #Is Resin Colored?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRCQ_d","Is Resin Colored?",value=F))
                      ),
                      #Resin Color
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRC_d","Resin Color",value=F))
                      ),
                      #Is Resin Radiopaque?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRRQ_d","Is Resin Radiopaque?",value=F))
                      ),
                      #Resin Durometer
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRDu_d","Resin Durometer (D)",value=F))
                      ),
                      #Average Resin Durometer
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMRADu_d","Average Durometer (D)",value=F))
                      )
                    ),#end Resin Information
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checkmultiresininfo", "Show All Resin Information")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("uncheckmultiresininfo", "Hide All Resin Information")
                      )
                    )
                )
              ),
              #Tooling
              fluidRow(
                box(title = "Tooling Parameters", 
                    solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Extrusion Type
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMET_d","Extrusion Type",value=T))
                      ),
                      #Barrel
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMB_d","Barrel",value=F))
                      ),
                      #Die Size
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMDS_d","Die Size (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMDS_d",
                                        numericInput("PCMDS_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMDSmin, ")"),
                                                     value=PCMDSmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMDS_d",
                                        numericInput("PCMDS_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMDSmax, ")"),
                                                     value=PCMDSmax,step=0.001)
                                      )))),
                      #Die Land Length
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMDLL_d","Die Land Length (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMDLL_d",
                                 selectInput("PCMDLL",label = "Select One",c("All",unique(as.character(multi_pps_data$`Die Land Length (in)`))))
                               ))),
                      #Tip Size
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMTS_d","Tip Size (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMTS_d",
                                        numericInput("PCMTS_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMTSmin, ")"),
                                                     value=PCMTSmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMTS_d",
                                        numericInput("PCMTS_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMTSmax, ")"),
                                                     value=PCMTSmax,step=0.001)
                                      )
                               ))),
                      #Tip Land Length
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMTLL_d","Tip Land Length (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMTLL_d",
                                 selectInput("PCMTLL",label = "Select One",c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
                               ))),
                      #Screw Print
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMSP_d","Screw Print",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMSP_d",
                                 selectInput("PCMSP",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`Screw Print`))))
                               )))
                    ),
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checkmultitooling", "Show All Tooling")   
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("uncheckmultitooling", "Hide All Tooling")
                      )
                    )
                    
                )
                
              ),#end Tooling
              
              #Processing Attributes
              fluidRow(
                box(title = "Processing Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Feedthroat
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMFT_d","Feedthroat Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMFT_d",
                                        numericInput("PCMFT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMFTmin, ")"),
                                                     value = PCMFTmin,step=1)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMFT_d",
                                        numericInput("PCMFT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMFTmax, ")"),
                                                     value=PCMFTmax,step=1)
                                      )
                               ))),
                      #Barrel Zone 1
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMBZT1_d","Barrel Zone 1 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMBZT1_d",
                                        numericInput("PCMBZT1_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMBZT1min, ")"),
                                                     value=PCMBZT1min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMBZT1_d",
                                        numericInput("PCMBZT1_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMBZT1max, ")"),
                                                     value=PCMBZT1max,step=5)
                                      )
                               ))),
                      #Barrel ZOne2
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMBZT2_d","Barrel Zone 2 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMBZT2_d",
                                        numericInput("PCMBZT2_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMBZT2min, ")"),
                                                     value=PCMBZT2min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMBZT2_d",
                                        numericInput("PCMBZT2_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMBZT2max, ")"),
                                                     value=PCMBZT2max,step=5)
                                      )
                               ))),
                      #Barrel Zone3
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMBZT3_d","Barrel Zone 3 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMBZT3_d",
                                        numericInput("PCMBZT3_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMBZT3min, ")"),
                                                     value=PCMBZT3min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMBZT3_d",
                                        numericInput("PCMBZT3_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMBZT3max, ")"),
                                                     value=PCMBZT3max,step=5)
                                      )
                               )))
                    ),#end Processing Attribute 1
                    fluidRow(
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMCT_d","Clamp Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMCT_d",
                                        numericInput("PCMCT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMCTmin, ")"),
                                                     value=PCMCTmin,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMCT_d",
                                        numericInput("PCMCT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMCTmax, ")"),
                                                     value=PCMCTmax,step=5)
                                      )
                               ))),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMAT_d","Adapter Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMAT_d",
                                        numericInput("PCMAT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMATmin, ")"),
                                                     value=PCMATmin,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMAT_d",
                                        numericInput("PCMAT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMATmax, ")"),
                                                     value=PCMATmax,step=5)
                                      )
                               ))),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMDT1_d","Die 1 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMDT1_d",
                                        numericInput("PCMDT1_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMDT1min, ")"),
                                                     value=PCMDT1min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMDT1_d",
                                        numericInput("PCMDT1_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMDT1max, ")"),
                                                     value=PCMDT1max,step=5)
                                      )
                               ))),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMDT2_d","Die 2 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMDT2_d",
                                        numericInput("PCMDT2_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMDT2min, ")"),
                                                     value=PCMDT2min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMDT2_d",
                                        numericInput("PCMDT2_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMDT2max, ")"),
                                                     value=PCMDT2max,step=5)
                                      )
                               )))), 
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checkmultiparameters", "Show All Processing Parameters")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("uncheckmultiparameters", "Hide All Processing Parameters")
                      )
                    )
                )
              ),#end Processing Attribute 2
              
              
              #Dimentional Attribute
              fluidRow(
                box(title = "Dimensional Attributes", 
                    solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             
                             fluidRow(checkboxInput("PCMTE_d","Tapered End",value=FALSE))
                      ),
                      
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             
                             fluidRow(checkboxInput("PCMIDI_d","Inner Diameter (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMIDI_d",
                                        numericInput("PCMIDI_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMIDImin, ")"),
                                                     value=PCMIDImin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMIDI_d",
                                        numericInput("PCMIDI_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMIDImax, ")"),
                                                     value=PCMIDImax,step=0.001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMODI_d","Outer Diameter (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMODI_d",
                                        numericInput("PCMODI_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMODImin, ")"),
                                                     value=PCMODImin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMODI_d",
                                        numericInput("PCMODI_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMODImax, ")"),
                                                     value=PCMODImax,step=0.001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMIWT_d","Inner Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMIWT_d",
                                        numericInput("PCMIWT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMIWTmin, ")"),
                                                     value=PCMIWTmin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMIWT_d",
                                        numericInput("PCMIWT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMIWTmax, ")"),
                                                     value=PCMIWTmax,step=0.001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMMWT_d","Middle Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMMWT_d",
                                        numericInput("PCMMWT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMMWTmin, ")"),
                                                     value=PCMMWTmin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMMWT_d",
                                        numericInput("PCMMWT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMMWTmax, ")"),
                                                     value=PCMMWTmax,step=0.001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMOWT_d","Outer Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMOWT_d",
                                        numericInput("PCMOWT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMOWTmin, ")"),
                                                     value=PCMOWTmin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMOWT_d",
                                        numericInput("PCMOWT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMOWTmax, ")"),
                                                     value=PCMOWTmax,step=0.001)
                                      ))))
                      ),
                    
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMTWT_d","Total Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMTWT_d",
                                        numericInput("PCMTWT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMTWTmin, ")"),
                                                     value=PCMTWTmin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMTWT_d",
                                        numericInput("PCMTWT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMTWTmax, ")"),
                                                     value=PCMTWTmax,step=0.001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMOR_d","Out of Roundness (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMOR_d",
                                        numericInput("PCMOR_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMORmin, ")"),
                                                     value=PCMORmin,step=0.001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMOR_d",
                                        numericInput("PCMOR_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMORmax, ")"),
                                                     value=PCMORmax,step=0.001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMCCT_d","Concentricity",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMCCT_d",
                                        numericInput("PCMCCT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMCCTmin, ")"),
                                                     value=PCMCCTmin,step=0.0001)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMCCT_d",
                                        numericInput("PCMCCT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMCCTmax, ")"),
                                                     value=PCMCCTmax,step=0.0001)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMLength_d","Length (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMLength_d",
                                        numericInput("PCMLength_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMLengthmin, ")"),
                                                     value=PCMLengthmin,step=1)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMLength_d",
                                        numericInput("PCMLength_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMLengthmax, ")"),
                                                     value=PCMLengthmax,step=1)
                                      )))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMToLength_d","Total Length (in)",value=T)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMToLength_d",
                                        numericInput("PCMToLength_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCMToLengthmin, ")"),
                                                     value=PCMToLengthmin,step=1)
                                      )),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCMToLength_d",
                                        numericInput("PCMToLength_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCMToLengthmax, ")"),
                                                     value=PCMToLengthmax,step=1)
                                      )))),
                      
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMPPD_d","Perpendicularity (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMPPD_d",
                                 selectInput("PCMPPD",label = "Select One",
                                             c("All",unique(as.character(multi_pps_data$`Perpendicularity (in)`))))
                               )))), 
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checkmultidimensions", "Show All Dimensional Attributes")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("uncheckmultidimensions", "Hide All Dimensional Attributes")
                      )
                    )
                )
              ), #End Dimentional Attribute
              #Special Operation
              fluidRow(
                box(title = "Special Operations", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMNEXIV_d","NEXIV",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMNEXIV_d",
                                 selectInput("PCMNEXIV",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMAnnealed_d","Annealed",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMAnnealed_d",
                                 selectInput("PCMAnnealed",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMCaliper_d","Caliper",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMCaliper_d",
                                 selectInput("PCMCaliper",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMOS_d","OD Sort",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMOS_d",
                                 selectInput("PCMOS",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMMP_d","Melt Pump",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMMP_d",
                                 selectInput("PCMMP",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMHT_d","Hypo Tip",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMHT_d",
                                 selectInput("PCMHT",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMSPD_d","Sparker Die",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMSPD_d",
                                 selectInput("PCMSPD",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMSLD_d","Slicking Die",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMSLD_d",
                                 selectInput("PCMSLD",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMDLN_d","Delamination",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMDLN_d",
                                 selectInput("PCMDLN",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMULT_d","Ultrasonic",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMULT_d",
                                 selectInput("PCMULT",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMVC_d","Vacuum Calibration",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMVC_d",
                                 selectInput("PCMVC",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCMIRD_d","Irradiated",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCMIRD_d",
                                 selectInput("PCMIRD",label = "Select One",choices=c("All","yes","NA"))
                               )))
                    ), 
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checkmultispecial", "Show All Special Operations")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("uncheckmultispecial", "Hide All Special Operations")
                      )
                    )
                )
              ), #end Special Operation
              
              
              
              fluidRow(
                box(title = "Part Listings", 
                    solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                    DT::dataTableOutput("mytable2")
                )
                
              ),
              fluidRow(
                box(title = "Download and Reset Buttons", 
                    solidHeader = TRUE, status = "warning", collapsible = TRUE, width = 12,
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             downloadButton('multidownloadSPPSData','Download Multi-Layer PPS Data')
                      ),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             downloadButton('multidownloadSPPSDataAll','Download Multi-Layer PPS Data with All Parameters')
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton('resetmultiinputs', "Reset Multi-Layer Input Parameters")
                      )
                    )
                )
              )
              
              
      ),
      tabItem(tabName = "taperedppstab",
              
              fluidRow(
                box(title = "Part, Resin, and PPS - Number and Description", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Part Number
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPN_d","Part Number",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition="input.PCTPN_d",
                                 
                                 selectInput("PCTPN",label = "Select One",
                                             c("All",unique(as.character(tapered_pps_data$`Part Number`))))
                                 
                               ))),
                      # Part Description
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPD_d","Part Description",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTPD_d",
                                 selectInput("PCTPD",label = "Select One",
                                             c("All",unique(as.character(single_pps_data$`Part Description`))))
                               ))), 
                      # Resin Number
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRN_d","Resin Number",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTRN_d",
                                 selectInput("PCTRN",label = "Select One",
                                             c("All",unique(as.character(single_pps_data$`Resin Number`))))
                               ))),
                      #Resin Description
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRD_d","Resin Description",value=TRUE)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTRD_d",
                                 selectInput("PCTRD",label = "Select One",
                                             c("All",unique(as.character(single_pps_data$`Resin Description`))))
                               ))),
                      #PPS Number
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPPSN_d","PPS Number",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTPPSN_d",
                                 selectInput("PCTPPSN",label = "Select One",
                                             c("All",unique(as.character(single_pps_data$`PPS Number`))))
                               )))
                    )
                )
              ),
              
              #Resin Information
              fluidRow(
                box(title = "Resin Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Resin Families
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRF_d","Resin Families",value=F))
                      ), 
                      #Is Resin Blended with Anything?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRBQ_d","Is Resin Blended with Anything?",value=F))
                      ), 
                      #Is Resin a Polymer Blend?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRPBQ_d","Is Resin a Polymer Blend?",value=F))
                      ),
                      #Is Resin Filled?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRFQ_d","Is Resin Filled?",value=F))
                      ),
                      #Resin Fillers
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRFi_d","Resin Fillers",value=F))
                      ),
                      #Is Resin Colored?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRCQ_d","Is Resin Colored?",value=F))
                      ),
                      #Resin Color
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRC_d","Resin Color",value=F))
                      ),
                      #Is Resin Radiopaque?
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRRQ_d","Is Resin Radiopaque?",value=F))
                      ),
                      #Resin Durometer
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRDu_d","Resin Durometer (D)",value=F))
                      ),
                      #Average Resin Durometer
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTRADu_d","Average Durometer (D)",value=F))
                      )
                    ),#end Resin Information
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checktaperedresininfo", "Show All Resin Information")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("unchecktaperedresininfo", "Hide All Resin Information")
                      )
                    )
                    
                )
              ),#end Resin Info
              
              #Tooling
              fluidRow(
                box(title = "Tooling Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      #Die Size
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDS_d","Die Size (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDS_d",
                                        numericInput("PCTDS_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDSmin, ")"),
                                                     value=PCTDSmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDS_d",
                                        numericInput("PCTDS_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDSmax, ")"),
                                                     value=PCTDSmax,step=0.001)
                                      )))),
                      #Die Land Length
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDLL_d","Die Land Length (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTDLL_d",
                                 selectInput("PCTDLL",label = "Select One",c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
                               ))),
                      #Tip Size
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTTS_d","Tip Size (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTTS_d",
                                        numericInput("PCTTS_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTTSmin, ")"),
                                                     value=PCTTSmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTTS_d",
                                        numericInput("PCTTS_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTTSmax, ")"),
                                                     value=PCTTSmax,step=0.001)
                                      )
                               ))),
                      #Tip Land Length
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTTLL_d","Tip Land Length (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTTLL_d",
                                 selectInput("PCTTLL",label = "Select One",c("All",unique(as.character(single_pps_data$`Tip Land Length (in)`))))
                               ))),
                      #Screw Print
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTSP_d","Screw Print",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTSP_d",
                                 selectInput("PCTSP",label = "Select One",
                                             c("All",unique(as.character(single_pps_data$`Screw Print`))))
                               )))
                    ),
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checktaperedtooling", "Show All Tooling")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("unchecktaperedtooling", "Hide All Tooling")
                      )
                    )
                )
              ), #end Tooling
              
              
              #Processing Attributes
              fluidRow(
                box(title = "Processing Parameters", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      
                      #Feedthroat
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTFT_d","Feedthroat Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTFT_d",
                                        numericInput("PCTFT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTFTmin, ")"),
                                                     value = PCTFTmin,step=1)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTFT_d",
                                        numericInput("PCTFT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTFTmax, ")"),
                                                     value=PCTFTmax,step=1)
                                      )
                               ))),
                      #Barrel Zone 1
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTBZT1_d","Barrel Zone 1 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTBZT1_d",
                                        numericInput("PCTBZT1_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTBZT1min, ")"),
                                                     value=PCTBZT1min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTBZT1_d",
                                        numericInput("PCTBZT1_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTBZT1max, ")"),
                                                     value=PCTBZT1max,step=5)
                                      )
                               ))),
                      #Barrel ZOne2
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTBZT2_d","Barrel Zone 2 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTBZT2_d",
                                        numericInput("PCTBZT2_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTBZT2min, ")"),
                                                     value=PCTBZT2min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTBZT2_d",
                                        numericInput("PCTBZT2_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTBZT2max, ")"),
                                                     value=PCTBZT2max,step=5)
                                      )
                               ))),
                      #Barrel Zone3
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTBZT3_d","Barrel Zone 3 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTBZT3_d",
                                        numericInput("PCTBZT3_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTBZT3min, ")"),
                                                     value=PCTBZT3min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTBZT3_d",
                                        numericInput("PCTBZT3_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTBZT3max, ")"),
                                                     value=PCTBZT3max,step=5)
                                      )
                               )))
                    ),#end Processing Attribute 1
                    fluidRow(
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTCT_d","Clamp Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTCT_d",
                                        numericInput("PCTCT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTCTmin, ")"),
                                                     value=PCTCTmin,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTCT_d",
                                        numericInput("PCTCT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTCTmax, ")"),
                                                     value=PCTCTmax,step=5)
                                      )
                               ))),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTAT_d","Adapter Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTAT_d",
                                        numericInput("PCTAT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTATmin, ")"),
                                                     value=PCTATmin,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTAT_d",
                                        numericInput("PCTAT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTATmax, ")"),
                                                     value=PCTATmax,step=5)
                                      )
                               ))),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDT1_d","Die 1 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDT1_d",
                                        numericInput("PCTDT1_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDT1min, ")"),
                                                     value=PCTDT1min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDT1_d",
                                        numericInput("PCTDT1_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDT1max, ")"),
                                                     value=PCTDT1max,step=5)
                                      )
                               ))),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDT2_d","Die 2 Temperature F",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDT2_d",
                                        numericInput("PCTDT2_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDT2min, ")"),
                                                     value=PCTDT2min,step=5)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDT2_d",
                                        numericInput("PCTDT2_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDT2max, ")"),
                                                     value=PCTDT2max,step=5)
                                      )
                               )))), #end Processing Attribute 2
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checktaperedparameters", "Show All Tooling")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("unchecktaperedparameters", "Hide All Tooling")
                      ) 
                    )
                )
              ), #end Processing Attribute
              
              #Dimentional Attribute
              fluidRow(
                box(title = "Dimensional Attributes", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPIDI_d","Proximal Inner Diameter (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPIDI_d",
                                        numericInput("PCTPIDI_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTPIDImin, ")"),
                                                     value=PCTPIDImin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPIDI_d",
                                        numericInput("PCTPIDI_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTPIDImax, ")"),
                                                     value=PCTPIDImax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPODI_d","Proximal Outer Diameter (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPODI_d",
                                        numericInput("PCTPODI_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTPODImin, ")"),
                                                     value=PCTPODImin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPODI_d",
                                        numericInput("PCTPODI_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTPODImax, ")"),
                                                     value=PCTPODImax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPWT_d","Proximal Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPWT_d",
                                        numericInput("PCTPWT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTPWTmin, ")"),
                                                     value=PCTPWTmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPWT_d",
                                        numericInput("PCTPWT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTPWTmax, ")"),
                                                     value=PCTPWTmax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPOR_d","Proximal Out of Roundness (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPOR_d",
                                        numericInput("PCTPOR_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTPORmin, ")"),
                                                     value=PCTPORmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPOR_d",
                                        numericInput("PCTPOR_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTPORmax, ")"),
                                                     value=PCTPORmax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPCCT_d","Proximal Concentricity (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPCCT_d",
                                        numericInput("PCTPCCT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTPCCTmin, ")"),
                                                     value=PCTPCCTmin,step=0.0001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPCCT_d",
                                        numericInput("PCTPCCT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTPCCTmax, ")"),
                                                     value=PCTPCCTmax,step=0.0001)
                                      )
                               )))
                      
                    ), #end Dimentional Attribute 1
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDIDI_d","Distal Inner Diameter (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDIDI_d",
                                        numericInput("PCTDIDI_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDIDImin, ")"),
                                                     value=PCTDIDImin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDIDI_d",
                                        numericInput("PCTDIDI_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDIDImax, ")"),
                                                     value=PCTDIDImax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDODI_d","Distal Outer Diameter (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDODI_d",
                                        numericInput("PCTDODI_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDODImin, ")"),
                                                     value=PCTDODImin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDODI_d",
                                        numericInput("PCTDODI_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDODImax, ")"),
                                                     value=PCTDODImax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDWT_d","Distal Wall Thickness (in)",value=TRUE)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDWT_d",
                                        numericInput("PCTDWT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDWTmin, ")"),
                                                     value=PCTDWTmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDWT_d",
                                        numericInput("PCTDWT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDWTmax, ")"),
                                                     value=PCTDWTmax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDOR_d","Distal Out of Roundness (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDOR_d",
                                        numericInput("PCTDOR_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDORmin, ")"),
                                                     value=PCTDORmin,step=0.001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDOR_d",
                                        numericInput("PCTDOR_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDORmax, ")"),
                                                     value=PCTDORmax,step=0.001)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDCCT_d","Distal Concentricity (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDCCT_d",
                                        numericInput("PCTDCCT_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDCCTmin, ")"),
                                                     value=PCTDCCTmin,step=0.0001)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDCCT_d",
                                        numericInput("PCTDCCT_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDCCTmax, ")"),
                                                     value=PCTDCCTmax,step=0.0001)
                                      )
                               )))
                      
                    ), #end Dimentional Attribute 2
                    fluidRow(
                      
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPLength_d","Proximal Length (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPLength_d",
                                        numericInput("PCTPLength_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTPLengthmin, ")"),
                                                     value=PCTPLengthmin,step=1)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTPLength_d",
                                        numericInput("PCTPLength_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTPLengthmax, ")"),
                                                     value=PCTPLengthmax,step=1)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTTLength_d","Transition Length (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTTLength_d",
                                        numericInput("PCTTLength_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTTLengthmin, ")"),
                                                     value=PCTTLengthmin,step=1)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTTLength_d",
                                        numericInput("PCTTLength_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTTLengthmax, ")"),
                                                     value=PCTTLengthmax,step=1)
                                      )
                               ))),
                      
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDLength_d","Distal Length (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDLength_d",
                                        numericInput("PCTDLength_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTDLengthmin, ")"),
                                                     value=PCTDLengthmin,step=1)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTDLength_d",
                                        numericInput("PCTDLength_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTDLengthmax, ")"),
                                                     value=PCTDLengthmax,step=1)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTToLength_d","Total Length (in)",value=F)),
                             fluidRow(
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTToLength_d",
                                        numericInput("PCTToLength_min",
                                                     label = paste0("Insert Minimum Value ", "(Minimum is ", PCTToLengthmin, ")"),
                                                     value=PCTToLengthmin,step=1)
                                      )
                               ),
                               column(6,
                                      conditionalPanel(
                                        condition = "input.PCTToLength_d",
                                        numericInput("PCTToLength_max",
                                                     label = paste0("Insert Maximum Value ", "(Maximum is ", PCTToLengthmax, ")"),
                                                     value=PCTToLengthmax,step=1)
                                      )
                               ))),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTPPD_d","Perpendicularity (in)",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTPPD_d",
                                 selectInput("PCTPPD",label = "Select One",
                                             c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
                               )))
                      
                    ),#end of attribute 3
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checktapereddimensions", "Show All Tooling")   
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("unchecktapereddimensions", "Hide All Tooling")
                      )
                    )
                )
              ),#end of attribute
              
              #Special Operation
              fluidRow(
                box(title = "Special Operations", 
                    solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                    fluidRow(
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTNEXIV_d","NEXIV",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTNEXIV_d",
                                 selectInput("PCTNEXIV",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTAnnealed_d","Annealed",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTAnnealed_d",
                                 selectInput("PCTAnnealed",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTCaliper_d","Caliper",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTCaliper_d",
                                 selectInput("PCTCaliper",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTOS_d","OD Sort",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTOS_d",
                                 selectInput("PCTOS",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTMP_d","Melt Pump",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTMP_d",
                                 selectInput("PCTMP",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTHT_d","Hypo Tip",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTHT_d",
                                 selectInput("PCTHT",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTSPD_d","Sparker Die",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTSPD_d",
                                 selectInput("PCTSPD",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTSLD_d","Slicking Die",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTSLD_d",
                                 selectInput("PCTSLD",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTDLN_d","Delamination",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTDLN_d",
                                 selectInput("PCTDLN",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTULT_d","Ultrasonic",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTULT_d",
                                 selectInput("PCTULT",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTVC_d","Vacuum Calibration",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTVC_d",
                                 selectInput("PCTVC",label = "Select One",choices=c("All","yes","NA"))
                               ))),
                      column(1,align = "center", style='padding-left: 20px; padding-right:20px;',
                             fluidRow(checkboxInput("PCTIRD_d","Irradiated",value=F)),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.PCTIRD_d",
                                 selectInput("PCTIRD",label = "Select One",choices=c("All","yes","NA"))
                               )))
                    ), #end Special Operation
                    fluidRow(
                      #fluid row for showing or deleting buttons
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("checktaperedspecial", "Show All Tooling")
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton("unchecktaperedspecial", "Hide All Tooling")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Part Listings", 
                    solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                    DT::dataTableOutput("mytable3")
                )
              ),
              fluidRow(
                box(title = "Download and Reset Buttons", 
                    solidHeader = TRUE, status = "warning", collapsible = TRUE, width = 12,
                    fluidRow(
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             downloadButton('tapereddownloadSPPSData','Download Tapered PPS Data')
                      ),
                      column(3,align = "center", style='padding-left: 20px; padding-right:20px;',
                             downloadButton('tapereddownloadSPPSDataAll','Download Tapered PPS Data with All Parameters')
                      ),
                      column(2,align = "center", style='padding-left: 20px; padding-right:20px;',
                             actionButton('resettaperedinputs', "Reset Tapered Input Parameters")
                      )
                    )
                )
              )
              
      ),#end tabItem
      tabItem(tabName = "singlesamplingtab",
              fluidRow(
                column(3,
                       textInput("sspn","Part Number")
                ),
                column(3,
                       textInput("ssppsn","PPS Number")
                )
              ),
              fluidRow(
                column(3,
                       textInput("ssatt","Attribute")
                ),
                column(3,
                       textInput("ssmm","Measurement Method")  
                ),
                column(3,
                       textInput("sssam","Sampling")    
                )
              ),
              fluidRow(
                column(3,
                       textInput("sslsl","Lower Specification Limit (LSL)")
                ),
                column(3,
                       textInput("sslcl","Lower Control Limit (LCL)") 
                ),
                column(3,
                       textInput("sstar","Target")
                ),
                column(3,
                       textInput("ssucl","Upper Control Limit (UCL)")
                ),
                column(3,
                       textInput("ssusl","Upper Specification Limit (USL)")
                )
              ),
              fluidRow(
                column(3,
                       textInput("ssprean","Pre-Annealing")
                ),
                column(3,
                       textInput("sspostan","Post-Annealing")
                ),
                column(3,
                       textInput("ssoff","Offline after some Time")  
                ),
                column(3,
                       textInput("sspostirr","Post-Irradiated")  
                )
              ),
              fluidRow(
                column(2,
                       actionButton("search_single_sampling","Search Table")
                ),
                column(2,
                       actionButton("reset_single_sampling","Reset Table")
                )
              ),
              fluidRow(
                DT::dataTableOutput("single_sampling_ui")
              )
      ), #end tabItem
      tabItem(tabName = "multisamplingtab",
              fluidRow(
                column(3,
                       textInput("mspn","Part Number")
                ),
                column(3,
                       textInput("msppsn","PPS Number")
                )
              ),
              fluidRow(
                column(3,
                       textInput("msatt","Attribute")
                ),
                column(3,
                       textInput("msmm","Measurement Method")  
                ),
                column(3,
                       textInput("mssam","Sampling")    
                )
              ),
              fluidRow(
                column(3,
                       textInput("mslsl","Lower Specification Limit (LSL)")
                ),
                column(3,
                       textInput("mslcl","Lower Control Limit (LCL)") 
                ),
                column(3,
                       textInput("mstar","Target")
                ),
                column(3,
                       textInput("msucl","Upper Control Limit (UCL)")
                ),
                column(3,
                       textInput("msusl","Upper Specification Limit (USL)")
                )
              ),
              fluidRow(
                column(3,
                       textInput("msprean","Pre-Annealing")
                ),
                column(3,
                       textInput("mspostan","Post-Annealing")
                ),
                column(3,
                       textInput("msoff","Offline After Some Time")  
                ),
                column(3,
                       textInput("mspostirr","Post Irradiated")  
                )
              ),
              fluidRow(
                column(2,
                       actionButton("search_multi_sampling","Search Table")
                ),
                column(2,
                       actionButton("reset_multi_sampling","Reset Table")
                )
              ),
              fluidRow(
                DT::dataTableOutput("multi_sampling_ui")
              )
      ), #end tabItem
      tabItem(tabName = "taperedsamplingtab",
              fluidRow(
                column(3,
                       textInput("tspn","Part Number")
                ),
                column(3,
                       textInput("tsppsn","PPS Number")
                )
              ),
              fluidRow(
                column(3,
                       textInput("tsatt","Attribute")
                ),
                column(3,
                       textInput("tsmm","Measurement Method")  
                ),
                column(3,
                       textInput("tssam","Sampling")    
                )
              ),
              fluidRow(
                column(3,
                       textInput("tslsl","Lower Specification Limit (LSL)")
                ),
                column(3,
                       textInput("tslcl","Lower Control Limit (LCL)") 
                ),
                column(3,
                       textInput("tstar","Target")
                ),
                column(3,
                       textInput("tsucl","Upper Control Limit (UCL)")
                ),
                column(3,
                       textInput("tsusl","Upper Specification Limit (USL)")
                )
              ),
              fluidRow(
                column(3,
                       textInput("tsprean","Pre-Annealing")
                ),
                column(3,
                       textInput("tspostan","Post-Annealing")
                ),
                column(3,
                       textInput("tsoff","Offline after some Time")  
                ),
                column(3,
                       textInput("tspostirr","Post-Irradiated")  
                )
              ),
              fluidRow(
                column(2,
                       actionButton("search_tapered_sampling","Search Table")
                ),
                column(2,
                       actionButton("reset_tapered_sampling","Reset Table")
                )
              ),
              fluidRow(
                DT::dataTableOutput("tapered_sampling_ui")
              )
      ), #end tabItem
      tabItem(tabName = "extrasamplingtab",
              fluidRow(
                column(3,
                       textInput("espn","Part Number")
                ),
                column(3,
                       textInput("esppsn","PPS Number")
                )
              ),
              fluidRow(
                column(3,
                       textInput("esatt","Attribute")
                ),
                column(3,
                       textInput("esmm","Measurement Method")  
                ),
                column(3,
                       textInput("essam","Sampling")    
                )
              ),
              fluidRow(
                column(3,
                       textInput("eslsl","Lower Specification Limit (LSL)")
                ),
                column(3,
                       textInput("eslcl","Lower Control Limit (LCL)") 
                ),
                column(3,
                       textInput("estar","Target")
                ),
                column(3,
                       textInput("esucl","Upper Control Limit (UCL)")
                ),
                column(3,
                       textInput("esusl","Upper Specification Limit (USL)")
                )
              ),
              fluidRow(
                column(3,
                       textInput("esprean","Pre-Annealing")
                ),
                column(3,
                       textInput("espostan","Post-Annealing")
                ),
                column(3,
                       textInput("esoff","Offline after some Time")  
                ),
                column(3,
                       textInput("espostirr","Post-Irradiated")  
                )
              ),
              fluidRow(
                column(2,
                       actionButton("search_extra_sampling","Search Table")
                ),
                column(2,
                       actionButton("reset_extra_sampling","Reset Table")
                )
              ),
              fluidRow(
                DT::dataTableOutput("extra_sampling_ui")
              )
      ), #end tabItem
      tabItem(tabName = "totalsamplingtab",
              fluidRow(
                column(3,
                       textInput("aspn","Part Number")
                ),
                column(3,
                       textInput("asppsn","PPS Number")
                )
              ),
              fluidRow(
                column(3,
                       textInput("asatt","Attribute")
                ),
                column(3,
                       textInput("asmm","Measurement Method")  
                ),
                column(3,
                       textInput("assam","Sampling")    
                )
              ),
              fluidRow(
                column(3,
                       textInput("aslsl","Lower Specification Limit (LSL)")
                ),
                column(3,
                       textInput("aslcl","Lower Control Limit (LCL)") 
                ),
                column(3,
                       textInput("astar","Target")
                ),
                column(3,
                       textInput("asucl","Upper Control Limit (UCL)")
                ),
                column(3,
                       textInput("asusl","Upper Specification Limit (USL)")
                )
              ),
              fluidRow(
                column(3,
                       textInput("asprean","Pre-Annealing")
                ),
                column(3,
                       textInput("aspostan","Post-Annealing")
                ),
                column(3,
                       textInput("asoff","Offline after some Time")  
                ),
                column(3,
                       textInput("aspostirr","Post-Irradiated")  
                )
              ),
              fluidRow(
                column(2,
                       actionButton("search_all_sampling","Search Table")
                ),
                column(2,
                       actionButton("reset_all_sampling","Reset Table")
                )
              ),
              fluidRow(
                DT::dataTableOutput("all_sampling_ui")
              )
      ), #end tabItem
      tabItem(tabName = "singlemesparametersandyieldtab",
              fluidRow(
                column(3,
                       box(title = "Temperature Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 12,
                           selectInput("singletaritempcolumns", 
                                              label = NULL,
                                              choices = single_tari_temp_columns,
                                              multiple = TRUE,
                                              selectize = TRUE,
                                              selected = NULL
                                              )
                       )
                ),
                column(3,
                       box(title = "Pressure Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           selectInput("singletaripresscolumns", 
                                              label = NULL,
                                              choices = single_tari_press_columns,
                                              multiple = TRUE,
                                              selectize = TRUE,
                                              selected = NULL
                           )
                       )
                ),
                column(3,
                       box(title = "Speed Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           selectInput("singletarispeedcolumns", 
                                              label = NULL,
                                              choices = single_tari_speed_columns,
                                              multiple = TRUE,
                                              selectize = TRUE,
                                              selected = NULL
                           )
                       )
                ),
                column(3,
                       box(title = "Exra Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           selectInput("singletariextracolumns", 
                                              label = NULL,
                                              choices = single_tari_extra_columns,
                                              multiple = TRUE,
                                              selectize = TRUE,
                                              selected = c("Material Number", "SAP Batch Number",
                                                           "Start Date", "Start Qty", "Start Qty Unit",
                                                           "Yield Qty", "Scrap Qty",
                                                           "Contribution to Plant Level Yield VOP",
                                                           "Contribution to Plant Level Scrap VOP",
                                                           "Contribution to Plant Level Total VOP",
                                                           "Yield Percentage", "Scrap Percentage")
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(title = "MES Parameters and Yield Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("singleMESparametersandyield")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "singlemesparameterstab",
              fluidRow(
                column(12,
                       box(title = "MES Parameters Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("singleMESparameters")
                       )
                       )
              )
      ), #end tabItem
      tabItem(tabName = "singlemestimetab",
              fluidRow(
                column(12,
                       box(title = "MES Timestamp Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("singleMEStime")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "singlemessubmitterstab",
              fluidRow(
                column(12,
                       box(title = "MES Submitter Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("singleMESsubmitter")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "singlemestotaltab",
              fluidRow(
                column(12,
                       box(title = "MES Total Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("singleMEStotal")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "multimesparametersandyieldtab",
              fluidRow(
                fluidRow(
                  column(3,
                         box(title = "Temperature Parameters", 
                             solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                             width = 12,
                             selectInput("multitaritempcolumns", 
                                         label = NULL,
                                         choices = multi_tari_temp_columns,
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         selected = NULL
                             )
                         )
                  ),
                  column(3,
                         box(title = "Pressure Parameters", 
                             solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                             selectInput("multitaripresscolumns", 
                                                label = NULL,
                                                choices = multi_tari_press_columns,
                                         multiple = TRUE,
                                         selectize = TRUE,
                                                selected = NULL
                             )
                         )
                  ),
                  column(3,
                         box(title = "Speed Parameters", 
                             solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                             selectInput("multitarispeedcolumns", 
                                                label = NULL,
                                                choices = multi_tari_speed_columns,
                                         multiple = TRUE,
                                         selectize = TRUE,
                                                selected = NULL
                             )
                         )
                  ),
                  column(3,
                         box(title = "Exra Parameters", 
                             solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                             selectInput("multitariextracolumns", 
                                                label = NULL,
                                                choices = multi_tari_extra_columns,
                                         multiple = TRUE,
                                         selectize = TRUE,
                                                selected = c("Material Number", "SAP Batch Number",
                                                             "Start Date", "Start Qty", "Start Qty Unit",
                                                             "Yield Qty", "Scrap Qty",
                                                             "Contribution to Plant Level Yield VOP",
                                                             "Contribution to Plant Level Scrap VOP",
                                                             "Contribution to Plant Level Total VOP",
                                                             "Yield Percentage", "Scrap Percentage")
                             )
                         )
                  )
                ),
                column(12,
                       box(title = "MES Parameters and Yield Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("multiMESparametersandyield")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "multimesparameterstab",
              fluidRow(
                column(12,
                       box(title = "MES Parameters Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("multiMESparameters")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "multimestimetab",
              fluidRow(
                column(12,
                       box(title = "MES Timestamp Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("multiMEStime")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "multimessubmitterstab",
              fluidRow(
                column(12,
                       box(title = "MES Submitter Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("multiMESsubmitter")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "multimestotaltab",
              fluidRow(
                column(12,
                       box(title = "MES Total Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("multiMEStotal")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "taperedmesparametersandyieldtab",
              fluidRow(
                column(3,
                       box(title = "Temperature Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, 
                           width = 12,
                           selectInput("taperedtaritempcolumns", 
                                              label = NULL,
                                              choices = tapered_tari_temp_columns,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                              selected = NULL
                           )
                       )
                ),
                column(3,
                       box(title = "Pressure Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           selectInput("taperedtaripresscolumns", 
                                              label = NULL,
                                              choices = tapered_tari_press_columns,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                              selected = NULL
                           )
                       )
                ),
                column(3,
                       box(title = "Speed Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           selectInput("taperedtarispeedcolumns", 
                                              label = NULL,
                                              choices = tapered_tari_speed_columns,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                              selected = NULL
                           )
                       )
                ),
                column(3,
                       box(title = "Exra Parameters", 
                           solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           selectInput("taperedtariextracolumns", 
                                              label = NULL,
                                              choices = tapered_tari_extra_columns,
                                       multiple = TRUE,
                                       selectize = TRUE,
                                              selected = c("Material Number", "SAP Batch Number",
                                                           "Start Date", "Start Qty", "Start Qty Unit",
                                                           "Yield Qty", "Scrap Qty",
                                                           "Contribution to Plant Level Yield VOP",
                                                           "Contribution to Plant Level Scrap VOP",
                                                           "Contribution to Plant Level Total VOP",
                                                           "Yield Percentage", "Scrap Percentage")
                           )
                       )
                )
              ),
              fluidRow(
                column(12,
                       box(title = "MES Parameters and Yield Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("taperedMESparametersandyield")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "taperedmesparameterstab",
              fluidRow(
                column(12,
                       box(title = "MES Parameters Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("taperedMESparameters")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "taperedmestimetab",
              fluidRow(
                column(12,
                       box(title = "MES Timestamp Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("taperedMEStime")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "taperedmessubmitterstab",
              fluidRow(
                column(12,
                       box(title = "MES Submitter Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("taperedMESsubmitter")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "taperedmestotaltab",
              fluidRow(
                column(12,
                       box(title = "MES Total Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("taperedMEStotal")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "singlescrapcodestab",
              fluidRow(
                column(12,
                       box(title = "Scrapcode Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("singlescrapcodes")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "mutliscrapcodestab",
              fluidRow(
                column(12,
                       box(title = "Scrapcode Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("multiscrapcodes")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "taperedscrapcodestab",
              fluidRow(
                column(12,
                       box(title = "MES Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("taperedscrapcodes")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "resininfotab",
              fluidRow(
                column(12,
                       box(title = "Resin Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("resin_data_ui")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "screwinfotab",
              fluidRow(
                column(12,
                       box(title = "Screw Data", 
                           solidHeader = TRUE, status = "success", collapsible = TRUE, width = 12,
                           DT::dataTableOutput("screw_data_ui")
                       )
                )
              )
      ), #end tabItem
      tabItem(tabName = "singleshoppingcarttab",
              fluidRow(
                box(title = "Data Table",
                    width = 12,
                    DT::dataTableOutput("singleshoppingcartpps"),
                    fluidRow(
                      downloadButton('singlecartdownloadpps',
                                     'Download Single Shopping Cart PPS Data')
                    )
                )
              ) #end fluidRow
      ), #end tabItem
      tabItem(tabName = "multishoppingcarttab",
              fluidRow(
                column(12,
                DT::dataTableOutput("multishoppingcartpps"),
                fluidRow(
                  downloadButton('multicartdownloadpps',
                                 'Download Multi-Layer Shopping Cart PPS Data')
                )
              )
              )
      ), #end tabItem
      tabItem(tabName = "taperedshoppingcarttab",
              fluidRow(
                column(12,
                DT::dataTableOutput("taperedshoppingcartpps"),
                fluidRow(
                  downloadButton('taperedcartdownloadpps',
                                 'Download Tapered Shopping Cart PPS Data')
                )
              )
              )
      ), #end tabItem
      tabItem(tabName = "totalshoppingcarttab",
              fluidRow(
                column(12,
                       DT::dataTableOutput("totalshoppingcartpps"),
                       fluidRow(
                         downloadButton('totalcartdownloadpps',
                                        'Download Total Shopping Cart PPS Data')
                       )
                )
              )
      ), #end tabItem
      
      tabItem(tabName="MESDataAnalysis",
        # Some custom CSS
        tags$head(
          tags$style(HTML("
                          /* Smaller font for preformatted text */
                          pre, table.table {
                          font-size: smaller;
                          }
                          
                          body {
                          min-height: 2000px;
                          }
                          
                          .option-group {
                          border: 1px solid #ccc;
                          border-radius: 6px;
                          padding: 0px 5px;
                          margin: 5px -10px;
                          background-color: #c1c1c1;
                          }
                          
                          .option-header {
                          color: #000000;
                          text-transform: uppercase;
                          margin-bottom: 5px;
                          }
                          .option-label{
                          color: #000000;
                          margin-bottom: 1px;
                          padding: 0px 5px;
                          }
                          .inputbox{
                          background-color: #000000;
                          }
                          "))),
        
        fluidRow(
          column(width = 3,
                 box(title = "Explorer",
                     solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                     div(class = "option-group",
                         radioButtons("MES_Data_set", "Data Set",
                                      c("Single", "Multi","Tapered","Upload"), inline = TRUE),
                         conditionalPanel(
                           "input.MES_Data_set ==='Upload'",
                           div(style="display: inline-block;vertical-align:top;width: 250px;",
                               fileInput("uploadfile", "Choose csv File",
                                         multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv"))),
                           div(style="display: inline-block;vertical-align:top;width: 50px;",
                               checkboxInput("header", "Header", TRUE)),
                           checkboxInput("Preview","Preview",F),
                           #preview the uploaded dataset
                           conditionalPanel(
                             condition="input.Preview",
                             DT::dataTableOutput("UploadDataPreview")
                           )
                           
                         )#end conditional panel for the upload file
                     ),# end Data Set section
                     
                     div(class="option-group",
                         div(class="option-header","Plot"),
                         textInput("MES_PlotTitle","Plot Title",placeholder = NULL),
                         
                         
                         div(style="display: inline-block;vertical-align:top;width: 150px;",
                             uiOutput("MES_Xvar_ui")),
                         div(style="display: inline-block;vertical-align:top;width: 150px;",
                             uiOutput("MES_Yvar_ui")),
                         
                         conditionalPanel(
                           condition = "input.MES_Data_set !='Upload'",
                           checkboxGroupInput("MES_PlotType","Plot Type",
                                              choiceNames = 
                                                list("Scatter","Line"),
                                              choiceValues = 
                                                list("Scatter","Line"),
                                              selected = "Scatter",inline = T
                           ),
                           uiOutput("Groupby_ui")
                         ) # End conditionalpanel for Mtcars
                     ),#end Plot section
                     
                     div(class="option-group",
                         div(class="option-header","Filters"),
                         
                         dateRangeInput("MES_DateRange", "Date range:",
                                        startview = "decade")
                         
                         # dropdownButton(
                         #     label = "Filter",status = "default", width = 80,
                         #     #actionButton(inputId = "de", label = "Sort A to Z", icon = icon("sort-alpha-asc")),# filter  (filter icon)
                         #     #actionButton(inputId = "as", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                         # 
                         #     tags$div(
                         #       class = "container",
                         #       uiOutput("filter_ui")   # to have reactive dropdown list
                         #     ),
                         #     actionButton(inputId = "filter_select", label = "(un)select all")  #an action button to select or unselect all
                         #   ) #end Filter
                         
                     ),#End Filter section
                     div(class = "option-group",
                         div(class = "option-header", "Download"),
                         radioButtons('MES_GraphFormat', 'Graph format', c('PDF', 'png'),
                                      inline = TRUE),
                         downloadButton("MES_GraphDownload_DataAnalysis","Download Graph")
                     )#end download secion
                 )
                 
          ), #end plot managing section
          
          column(width = 9, class = "well",
                 box(title = "Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                     column(width = 6,
                            uiOutput("MES_plotui")
                     ),
                     column(width = 6,
                            plotOutput("MES_plot2")
                     )),#end plot section
                 
                 
                 fluidRow(
                   h4("Points selected by brushing"),
                   DT::dataTableOutput("MES_plot_brushed_points")
                 ), #end brushed points section
                 
                 fluidRow(
                   downloadButton("MES_SelectedDataDownload_DataAnalysis","Download Selected Data")
                 )# end download dataset section
                 )
          )
        ),#end Analysis Tool
      
      
      
      
   
      tabItem(tabName="ScrapAnalysis",
              fluidRow(
                column(width = 3,
                       box(title = "Explorer",
                           solidHeader = TRUE, status = "info", collapsible = TRUE, width = 12,
                           div(class = "option-group",
                               radioButtons("Scrap_Data_set", "Data Set",
                                            c("Single", "Multi","Tapered"), inline = TRUE)
                               ),# end Data Set section
                           
                           div(class="option-group",
                               div(class="option-header","Plot"),
                               
                               textInput("Scrap_PlotTitle","Plot Title",placeholder = NULL),
                               
                               
                               div(style="display: inline-block;vertical-align:top;width: 150px;",
                                   uiOutput("Xvar_ui")),
                               div(style="display: inline-block;vertical-align:top;width: 150px;",
                                   uiOutput("Yvar_ui")),
                               
                               conditionalPanel(
                                 condition = "input.Scrap_Data_set !='Upload'",
                                 checkboxGroupInput("PlotType","Plot Type",
                                                    choiceNames = 
                                                      list("Scatter","Line"),
                                                    choiceValues = 
                                                      list("Scatter","Line"),
                                                    selected = "Scatter",inline = T
                                 ),
                                 uiOutput("Groupby_ui")
                               ) # End conditionalpanel for Mtcars
                           ),#end Plot section
                           
                           div(class="option-group",
                               div(class="option-header","Filters"),
                               
                               dateRangeInput("daterange6", "Date range:",
                                              startview = "decade")
                               
                               # dropdownButton(
                               #     label = "Filter",status = "default", width = 80,
                               #     #actionButton(inputId = "de", label = "Sort A to Z", icon = icon("sort-alpha-asc")),# filter  (filter icon)
                               #     #actionButton(inputId = "as", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
                               # 
                               #     tags$div(
                               #       class = "container",
                               #       uiOutput("filter_ui")   # to have reactive dropdown list
                               #     ),
                               #     actionButton(inputId = "filter_select", label = "(un)select all")  #an action button to select or unselect all
                               #   ) #end Filter
                               
                           ),#End Filter section
                           div(class = "option-group",
                               div(class = "option-header", "Download"),
                               radioButtons('GraphFormat', 'Graph format', c('PDF', 'png'),
                                            inline = TRUE),
                               downloadButton("GraphDownload_DataAnalysis","Download Graph")
                           )#end download secion
                       )
                       
                ), #end plot managing section
                
                column(width = 9, class = "well",
                       box(title = "Plot", solidHeader = TRUE, status = "primary", collapsible = TRUE, width = 12,
                           column(width = 6,
                                  uiOutput("plotui")
                           ),
                           column(width = 6,
                                  plotOutput("MES_plot2")
                           )),#end plot section
                       
                       
                       fluidRow(
                         h4("Points selected by brushing"),
                         DT::dataTableOutput("plot_brushed_points")
                       ), #end brushed points section
                       
                       fluidRow(
                         downloadButton("SelectedDataDownload_DataAnalysis","Download Selected Data")
                       )# end download dataset section
                )
              )
                )#end Analysis Tool
      
      
      
      
    ),#end tabItems
    
    
    
    
    
    
    #create a pop up window for the shopping cart
    absolutePanel(
      actionButton("ShoppingCart","",icon=icon("shopping-cart","fa-2x"),width = 80 ),
      #verbatimTextOutput("ShoppingCart_Count"),     # Try to add the number of part-number in shopping cart behind the cart icon
      
      bsModal("modalExample", "Shopping Cart", "ShoppingCart", size = "default",
              tabsetPanel(
                tabPanel("Single-Extrusion Cart",
                         textInput("SinglePartNum_input","Part Number"),
                         actionButton("singleMadd_button","Add"),
                         dataTableOutput("singleshoppingcartparts"),
                         dataTableOutput("singleshoppingcart")
                ),
                tabPanel("Multi-Layer Extrusion Cart",
                         textInput("MultiPartNum_input","Part Number"),
                         actionButton("multiMadd_button","Add"),
                         dataTableOutput("multishoppingcartparts"),
                         dataTableOutput("multishoppingcart")
                         #Multi-layer Extrusion Parts
                ),
                tabPanel("Tapered Extrusion Cart",
                         textInput("TaperedPartNum_input","Part Number"),
                         actionButton("taperedMadd_button","Add"),
                         dataTableOutput("taperedshoppingcartparts"),
                         dataTableOutput("taperedshoppingcart")
                         #Tapered Extrusion Parts
                ),
                tabPanel("Total Extrusion Cart",
                         #Total Extrusion Parts
                         dataTableOutput("totalshoppingcartparts"),
                         dataTableOutput("totalshoppingcart")
                )
              )
              
              
              
      ),
      draggable = F,right = 10,top = 3, fixed = F,
      style = "z-index: 999999"
      
    ) #end absolutePanel
    
    
  )#end dashboardbody
  
) #end UI