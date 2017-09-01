library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)


#_s:name of the checkbox
#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

ui<-navbarPage("Extrusion Application",
               
               navbarMenu("Part Catalog",   #Create a dropdown list named as Part Catalog containing of Single, Multi,and Tapered
                          #Single Extrusion PPS Data
                          tabPanel("Single Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"), #Use tag to add a hearder 
                                     #Part Number
                                     column(2,
                                             fluidRow(checkboxInput("PCSPN_d", "Part Number", value = TRUE)),  #Show the checkbox for Part number. it will return a True/False value
                                             fluidRow(
                                               conditionalPanel(
                                                 condition="input.PCSPN_d",   #If it were Ture, then there will have a search box for Part Number under checkbox
                                                 selectizeInput("PCSPN",label = NULL,multiple=TRUE,
                                                                c("All",unique(as.character(single_pps_data$`Part Number`))),
                                                                selected="All",
                                                                onchange = 'Shiny.onInputChange(\"testbutton\",  this.id)')
                                                 ))),
                                     # Part Description
                                     column(2,
                                             fluidRow(checkboxInput("PCSPD_d", "Part Description", value = TRUE)),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSPD_d",
                                                 selectInput("PCSPD",label = NULL,
                                                             c("All",unique(as.character(single_pps_data$`Part Description`))))
                                                 ))), 
                                      # Resin Number
                                      column(2,
                                             fluidRow(checkboxInput("PCSRN_d","Resin Number",value=TRUE)),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSRN_d",
                                                 selectInput("PCSRN",label = NULL,
                                                             c("All",unique(as.character(single_pps_data$`Resin Number`))))
                                               ))),
                                      #Resin Description
                                      column(2,
                                             fluidRow(checkboxInput("PCSRD_d","Resin Description",value=TRUE)),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSRD_d",
                                                 selectInput("PCSRD",label = NULL,
                                                             c("All",unique(as.character(single_pps_data$`Resin Description`))))
                                               ))),
                                      #PPS Number
                                      column(2,
                                             fluidRow(checkboxInput("PCSPPSN_d","PPS Number",value=F)),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.PCSPPSN_d",
                                                 selectInput("PCSPPSN",label = NULL,
                                                             c("All",unique(as.character(single_pps_data$`PPS Number`))))
                                               )))
                                     ),
                                   #Tooling
                                   fluidRow(
                                     tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                     #Die Size
                                     column(2,
                                            fluidRow(checkboxInput("PCSDS_d","Die Size (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDS_d",
                                                column(6,numericInput("PCSDS_min",label = NULL,value=PCSDSmin,step=0.001)),
                                                column(6,numericInput("PCSDS_max",label = NULL,value=PCSDSmax,step=0.001))
                                                )
                                              )
                                            ), #end column
                                     #Die Land Length
                                     column(2,
                                            fluidRow(checkboxInput("PCSDLL_d","Die Land Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDLL_d",
                                                selectInput("PCSDLL",label = NULL,c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
                                              ))),
                                     #Tip Size
                                     column(2,
                                            fluidRow(checkboxInput("PCSTS_d","Tip Size (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSTS_d",
                                                column(6,numericInput("PCSTS_min",label = NULL,value=PCMTSmin,step=0.001)),
                                                column(6,numericInput("PCSTS_max",label = NULL,value=PCMTSmax,step=0.001))
                                                )
                                              )
                                            ), #end column
                                     #Tip Land Length
                                     column(2,
                                            fluidRow(checkboxInput("PCSTLL_d","Tip Land Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSTLL_d",
                                                selectInput("PCSTLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
                                              ))),
                                     #Screw Print
                                     column(2,
                                            fluidRow(checkboxInput("PCSSP_d","Screw Print",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSSP_d",
                                                selectInput("PCSSP",label = NULL,
                                                            c("All",unique(as.character(multi_pps_data$`Screw Print`))))
                                              )))
                                   ),#end Tooling
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Attribute"),style="font-size:25px;",align="left"),
                                     
                                     #Feedthroat
                                     column(3,
                                            fluidRow(checkboxInput("PCSFT_d","Feedthroat Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSFT_d",
                                                column(6,numericInput("PCSFT_min",label = NULL,value = PCMFTmin,step=1)),
                                                column(6,numericInput("PCSFT_max",label = NULL,value = PCMFTmax,step=1))
                                                )
                                              )
                                            ),
                                     #Barrel Zone 1
                                     column(3,
                                            fluidRow(checkboxInput("PCSBZT1_d","Barrel Zone 1 Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSBZT1_d",
                                                column(6,numericInput("PCSBZT1_min",label = NULL,value=PCMBZT1min,step=5)),
                                                column(6,numericInput("PCSBZT1_max",label = NULL,value=PCMBZT1max,step=5))
                                                )
                                              )
                                            ),
                                     #Barrel ZOne2
                                     column(3,
                                            fluidRow(checkboxInput("PCSBZT2_d","Barrel Zone 2 Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSBZT2_d",
                                                column(6,numericInput("PCSBZT2_min",label = NULL,value=PCMBZT2min,step=5)),
                                                column(6,numericInput("PCSBZT2_max",label = NULL,value=PCMBZT2max,step=5))
                                                )
                                              )
                                            ),
                                     #Barrel Zone3
                                     column(3,
                                            fluidRow(checkboxInput("PCSBZT3_d","Barrel Zone 3 Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSBZT3_d",
                                                column(6,numericInput("PCSBZT3_min",label = NULL,value=PCMBZT3min,step=5)),
                                                column(6,numericInput("PCSBZT3_max",label = NULL,value=PCMBZT3max,step=5))
                                                )
                                              )
                                            )
                                   ),#end Processing Attribute 1
                                   fluidRow(
                                     #Clamp Temperature F
                                     column(3,
                                            fluidRow(checkboxInput("PCSCT_d","Clamp Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSCT_d",
                                                column(6,numericInput("PCSCT_min",label = NULL,value=PCMCTmin,step=5)),
                                                column(6,numericInput("PCSCT_max",label = NULL,value=PCMCTmax,step=5))
                                                )
                                              )
                                            ),
                                     #Adapter Temperature F
                                     column(3,
                                            fluidRow(checkboxInput("PCSAT_d","Adapter Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSAT_d",
                                                column(6,numericInput("PCSAT_min",label = NULL,value=PCMATmin,step=5)),
                                                column(6,numericInput("PCSAT_max",label = NULL,value=PCMATmax,step=5))
                                                )
                                              )
                                            ),
                                     #Die 1 Temperature F
                                     column(3,
                                            fluidRow(checkboxInput("PCSDT1_d","Die 1 Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDT1_d",
                                                column(6,numericInput("PCSDT1_min",label = NULL,value=PCMDT1min,step=5)),
                                                column(6,numericInput("PCSDT1_max",label = NULL,value=PCMDT1max,step=5))
                                                )
                                              )
                                            ),
                                     #Die 2 Temperature F
                                     column(3,
                                            fluidRow(checkboxInput("PCSDT2_d","Die 2 Temperature F",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDT2_d",
                                                column(6,numericInput("PCSDT2_min",label = NULL,value=PCMDT2min,step=5)),
                                                column(6,numericInput("PCSDT2_max",label = NULL,value=PCMDT2max,step=5))
                                                )
                                              )
                                            )
                                     ), #end Processing Attribute 
                                   
                                   #Dimentional Attribute
                                   fluidRow(
                                     tags$h1(strong("Dimentional Attribute"),style="font-size:25px;",align="left"),
                                     #Inner Diameter
                                     column(3,
                                            fluidRow(checkboxInput("PCSIDI_d","Inner Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSIDI_d",
                                                column(6,numericInput("PCSIDI_min",label = NULL,value=PCSIDImin,step=0.001)),
                                                column(6,numericInput("PCSIDI_max",label = NULL,value=PCSIDImax,step=0.001))
                                                )
                                              )
                                            ),
                                     #Outer Diameter
                                     column(3,
                                            fluidRow(checkboxInput("PCSODI_d","Outer Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSODI_d",
                                                column(6,numericInput("PCSODI_min",label = NULL,value=PCSODImin,step=0.001)),
                                                column(6,numericInput("PCSODI_max",label = NULL,value=PCSODImax,step=0.001))
                                                )
                                              )
                                            ),
                                     #Wall Thickness
                                     column(3,
                                            fluidRow(checkboxInput("PCSWT_d","Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSWT_d",
                                                column(6,numericInput("PCSWT_min",label = NULL,value=PCSODImin,step=0.001)),
                                                column(6,numericInput("PCSWT_max",label = NULL,value=PCSODImax,step=0.001))
                                                )
                                              )
                                            ),
                                     #Out of Roundness (in)
                                     column(3,
                                            fluidRow(checkboxInput("PCSOR_d","Out of Roundness (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSOR_d",
                                                column(6,numericInput("PCSOR_min",label = NULL,value=PCSODImin,step=0.001)),
                                                column(6,numericInput("PCSOR_max",label = NULL,value=PCSODImax,step=0.001))
                                                )
                                              )
                                            )
                                   ),
                                   fluidRow(
                                     #Concentricity
                                     column(3,
                                            fluidRow(checkboxInput("PCSCCT_d","Concentricity",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSCCT_d",
                                                column(6,numericInput("PCSCCT_min",label = NULL,value=PCSCCTmin,step=0.0001)),
                                                column(6,numericInput("PCSCCT_max",label = NULL,value=PCSCCTmin,step=0.0001))
                                                )
                                              )
                                            ),
                                     #Length
                                     column(3,
                                            fluidRow(checkboxInput("PCSLength_d","Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSLength_d",
                                                column(6,numericInput("PCSLength_min",label = NULL,value=PCSLengthmin,step=1)),
                                                column(6,numericInput("PCSLength_max",label = NULL,value=PCSLengthmin,step=1))
                                                )
                                              )
                                            ),
                                     #Perpendicularity
                                     column(3,
                                            fluidRow(checkboxInput("PCSPPD_d","Perpendicularity (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSPPD_d",
                                                selectInput("PCSPPD",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
                                              )))), #End Dimentional Attribute
                                   
                                   #Special Operation
                                   fluidRow(
                                     tags$h1(strong("Special Operation"),style="font-size:25px;",align="left"),
                                     column(1,
                                            fluidRow(checkboxInput("PCSNEXIV_d","NEXIV",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSNEXIV_d",
                                                selectInput("PCSNEXIV",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSAnnealed_d","Annealed",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSAnnealed_d",
                                                selectInput("PCSAnnealed",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSCaliper_d","Caliper",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSCaliper_d",
                                                selectInput("PCSCaliper",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSOS_d","OD Sort",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSOS_d",
                                                selectInput("PCSOS",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSMP_d","Melt Pump",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSMP_d",
                                                selectInput("PCSMP",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSHT_d","Hypo Tip",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSHT_d",
                                                selectInput("PCSHT",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSSPD_d","Sparker Die",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSSPD_d",
                                                selectInput("PCSSPD",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSSLD_d","Slicking Die",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSSLD_d",
                                                selectInput("PCSSLD",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSDLN_d","Delamination",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSDLN_d",
                                                selectInput("PCSDLN",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSULT_d","Ultrasonic",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSULT_d",
                                                selectInput("PCSULT",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSVC_d","Vacuum Calibration",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSVC_d",
                                                selectInput("PCSVC",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCSIRD_d","Irradiated",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCSIRD_d",
                                                selectInput("PCSIRD",label = NULL,choices=c("All","yes","NA"))
                                              )))
                                   ), #end Special Operation
                                   
                                   # Show Table
                                   fluidRow(
                                     DT::dataTableOutput("mytable1")
                                     )
                                   ),#end Single Extrusion PPS Data
                          #Multi Extrusion PPS Data---UI
                          tabPanel("Multi Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"),
                                     #Part Number
                                     column(2,
                                            fluidRow(uiOutput("PCMPN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition="input.PCMPN_d",
                                                uiOutput("PCMPN_input")
                                              ))),
                                     # Part Description
                                     column(2,
                                            fluidRow(uiOutput("PCMPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMPD_d",
                                                uiOutput("PCMPD_input")
                                              ))), 
                                     # Resin Number
                                     column(2,
                                            fluidRow(uiOutput("PCMRN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMRN_d",
                                                uiOutput("PCMRN_input")
                                              ))),
                                     #Resin Description
                                     column(2,
                                            fluidRow(uiOutput("PCMRD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMRD_d",
                                                uiOutput("PCMRD_input")
                                              ))),
                                     #PPS Number
                                     column(2,
                                            fluidRow(uiOutput("PCMPPSN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMPPSN_d",
                                                uiOutput("PCMPPSN_input")
                                              )))
                                   ),
                                   #Tooling
                                   fluidRow(
                                     tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                     #Die Size
                                     column(2,
                                            fluidRow(uiOutput("PCMDS_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDS_d",
                                                       uiOutput("PCMDS_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDS_d",
                                                       uiOutput("PCMDS_max_input")
                                                     )))),
                                     #Die Land Length
                                     column(2,
                                            fluidRow(uiOutput("PCMDLL_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMDLL_d",
                                                uiOutput("PCMDLL_input")
                                              ))),
                                     #Tip Size
                                     column(2,
                                            fluidRow(uiOutput("PCMTS_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTS_d",
                                                       uiOutput("PCMTS_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTS_d",
                                                       uiOutput("PCMTS_max_input")
                                                     )
                                              ))),
                                     #Tip Land Length
                                     column(2,
                                            fluidRow(uiOutput("PCMTLL_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMTLL_d",
                                                uiOutput("PCMTLL_input")
                                              ))),
                                     #Screw Print
                                     column(2,
                                            fluidRow(uiOutput("PCMSP_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMSP_d",
                                                uiOutput("PCMSP_input")
                                              )))
                                   ),#end Tooling
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Attribute"),style="font-size:25px;",align="left"),
                                     
                                     #Feedthroat
                                     column(3,
                                            fluidRow(uiOutput("PCMFT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMFT_d",
                                                       uiOutput("PCMFT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMFT_d",
                                                       uiOutput("PCMFT_max_input")
                                                     )
                                              ))),
                                     #Barrel Zone 1
                                     column(3,
                                            fluidRow(uiOutput("PCMBZT1_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT1_d",
                                                       uiOutput("PCMBZT1_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT1_d",
                                                       uiOutput("PCMBZT1_max_input")
                                                     )
                                              ))),
                                     #Barrel ZOne2
                                     column(3,
                                            fluidRow(uiOutput("PCMBZT2_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT2_d",
                                                       uiOutput("PCMBZT2_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT2_d",
                                                       uiOutput("PCMBZT2_max_input")
                                                     )
                                              ))),
                                     #Barrel Zone3
                                     column(3,
                                            fluidRow(uiOutput("PCMBZT3_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT3_d",
                                                       uiOutput("PCMBZT3_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT3_d",
                                                       uiOutput("PCMBZT3_max_input")
                                                     )
                                              )))
                                   ),#end Processing Attribute 1
                                   fluidRow(
                                     column(3,
                                            fluidRow(uiOutput("PCMCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCT_d",
                                                       uiOutput("PCMCT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCT_d",
                                                       uiOutput("PCMCT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCMAT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMAT_d",
                                                       uiOutput("PCMAT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMAT_d",
                                                       uiOutput("PCMAT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCMDT1_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT1_d",
                                                       uiOutput("PCMDT1_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT1_d",
                                                       uiOutput("PCMDT1_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCMDT2_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT2_d",
                                                       uiOutput("PCMDT2_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT2_d",
                                                       uiOutput("PCMDT2_max_input")
                                                     )
                                              )))), #end Processing Attribute 2
                                   #Dimentional Attribute
                                   fluidRow(
                                     tags$h1(strong("Dimentional Attribute"),style="font-size:25px;",align="left"),
                                     column(2,
                                            fluidRow(uiOutput("PCMIDI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIDI_d",
                                                       uiOutput("PCMIDI_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIDI_d",
                                                       uiOutput("PCMIDI_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMODI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMODI_d",
                                                       uiOutput("PCMODI_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMODI_d",
                                                       uiOutput("PCMODI_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMIWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIWT_d",
                                                       uiOutput("PCMIWT_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIWT_d",
                                                       uiOutput("PCMIWT_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMMWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMMWT_d",
                                                       uiOutput("PCMMWT_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMMWT_d",
                                                       uiOutput("PCMMWT_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMOWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOWT_d",
                                                       uiOutput("PCMOWT_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOWT_d",
                                                       uiOutput("PCMOWT_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMTWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTWT_d",
                                                       uiOutput("PCMTWT_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTWT_d",
                                                       uiOutput("PCMTWT_max_input")
                                                     ))))),
                                   
                                   fluidRow(
                                     column(2,
                                            fluidRow(uiOutput("PCMOR_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOR_d",
                                                       uiOutput("PCMOR_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOR_d",
                                                       uiOutput("PCMOR_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMCCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCCT_d",
                                                       uiOutput("PCMCCT_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCCT_d",
                                                       uiOutput("PCMCCT_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMLength_d",
                                                       uiOutput("PCMLength_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMLength_d",
                                                       uiOutput("PCMLength_max_input")
                                                     )))),
                                     column(2,
                                            fluidRow(uiOutput("PCMToLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMToLength_d",
                                                       uiOutput("PCMToLength_min_input")
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMToLength_d",
                                                       uiOutput("PCMToLength_max_input")
                                                     )))),
                                     
                                     column(2,
                                            fluidRow(uiOutput("PCMPPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMPPD_d",
                                                uiOutput("PCMPPD_input")
                                              )))), #End Dimentional Attribute
                                   
                                   #Special Operation
                                   fluidRow(
                                     tags$h1(strong("Special Operation"),style="font-size:25px;",align="left"),
                                     column(1,
                                            fluidRow(uiOutput("PCMNEXIV_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMNEXIV_d",
                                                uiOutput("PCMNEXIV_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMAnnealed_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMAnnealed_d",
                                                uiOutput("PCMAnnealed_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMCaliper_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMCaliper_d",
                                                uiOutput("PCMCaliper_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMOS_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMOS_d",
                                                uiOutput("PCMOS_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMMP_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMMP_d",
                                                uiOutput("PCMMP_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMHT_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMHT_d",
                                                uiOutput("PCMHT_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMSPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMSPD_d",
                                                uiOutput("PCMSPD_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMSLD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMSLD_d",
                                                uiOutput("PCMSLD_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMDLN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMDLN_d",
                                                uiOutput("PCMDLN_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMULT_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMULT_d",
                                                uiOutput("PCMULT_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMVC_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMVC_d",
                                                uiOutput("PCMVC_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCMIRD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMIRD_d",
                                                uiOutput("PCMIRD_input")
                                              )))
                                   ), #end Special Operation

                                   fluidRow(
                                     DT::dataTableOutput("mytable2")
                                   )
                          ),#end multi Extrusion PPS Data
                          
                          #Tepered Extrusion PPS Data--UI
                          tabPanel("Tapered Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"),
                                     #Part Number
                                     column(2,
                                            fluidRow(uiOutput("PCTPN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition="input.PCTPN_d",
                                                uiOutput("PCTPN_input")
                                              ))),
                                     # Part Description
                                     column(2,
                                            fluidRow(uiOutput("PCTPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTPD_d",
                                                uiOutput("PCTPD_input")
                                              ))), 
                                     # Resin Number
                                     column(2,
                                            fluidRow(uiOutput("PCTRN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTRN_d",
                                                uiOutput("PCTRN_input")
                                              ))),
                                     #Resin Description
                                     column(2,
                                            fluidRow(uiOutput("PCTRD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTRD_d",
                                                uiOutput("PCTRD_input")
                                              ))),
                                     #PPS Number
                                     column(2,
                                            fluidRow(uiOutput("PCTPPSN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTPPSN_d",
                                                uiOutput("PCTPPSN_input")
                                              )))
                                   ),
                                   #Tooling
                                   fluidRow(
                                     tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                     #Die Size
                                     column(2,
                                            fluidRow(uiOutput("PCTDS_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDS_d",
                                                       uiOutput("PCTDS_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDS_d",
                                                       uiOutput("PCTDS_max_input")
                                                     )))),
                                     #Die Land Length
                                     column(2,
                                            fluidRow(uiOutput("PCTDLL_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTDLL_d",
                                                uiOutput("PCTDLL_input")
                                              ))),
                                     #Tip Size
                                     column(2,
                                            fluidRow(uiOutput("PCTTS_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTS_d",
                                                       uiOutput("PCTTS_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTS_d",
                                                       uiOutput("PCTTS_max_input")
                                                     )
                                              ))),
                                     #Tip Land Length
                                     column(2,
                                            fluidRow(uiOutput("PCTTLL_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTTLL_d",
                                                uiOutput("PCTTLL_input")
                                              ))),
                                     #Screw Print
                                     column(2,
                                            fluidRow(uiOutput("PCTSP_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTSP_d",
                                                uiOutput("PCTSP_input")
                                              )))
                                   ),#end Tooling
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Attribute"),style="font-size:25px;",align="left"),
                                     
                                     #Feedthroat
                                     column(3,
                                            fluidRow(uiOutput("PCTFT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTFT_d",
                                                       uiOutput("PCTFT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTFT_d",
                                                       uiOutput("PCTFT_max_input")
                                                     )
                                              ))),
                                     #Barrel Zone 1
                                     column(3,
                                            fluidRow(uiOutput("PCTBZT1_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT1_d",
                                                       uiOutput("PCTBZT1_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT1_d",
                                                       uiOutput("PCTBZT1_max_input")
                                                     )
                                              ))),
                                     #Barrel ZOne2
                                     column(3,
                                            fluidRow(uiOutput("PCTBZT2_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT2_d",
                                                       uiOutput("PCTBZT2_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT2_d",
                                                       uiOutput("PCTBZT2_max_input")
                                                     )
                                              ))),
                                     #Barrel Zone3
                                     column(3,
                                            fluidRow(uiOutput("PCTBZT3_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT3_d",
                                                       uiOutput("PCTBZT3_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT3_d",
                                                       uiOutput("PCTBZT3_max_input")
                                                     )
                                              )))
                                   ),#end Processing Attribute 1
                                   fluidRow(
                                     column(3,
                                            fluidRow(uiOutput("PCTCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTCT_d",
                                                       uiOutput("PCTCT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTCT_d",
                                                       uiOutput("PCTCT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCTAT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTAT_d",
                                                       uiOutput("PCTAT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTAT_d",
                                                       uiOutput("PCTAT_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCTDT1_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT1_d",
                                                       uiOutput("PCTDT1_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT1_d",
                                                       uiOutput("PCTDT1_max_input")
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(uiOutput("PCTDT2_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT2_d",
                                                       uiOutput("PCTDT2_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT2_d",
                                                       uiOutput("PCTDT2_max_input")
                                                     )
                                              )))), #end Processing Attribute 2
                                   #Dimentional Attribute
                                   fluidRow(
                                     tags$h1(strong("Dimentional Attribute"),style="font-size:25px;",align="left"),
                                     column(2,
                                            fluidRow(uiOutput("PCTPIDI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPIDI_d",
                                                       uiOutput("PCTPIDI_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPIDI_d",
                                                       uiOutput("PCTPIDI_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTPODI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPODI_d",
                                                       uiOutput("PCTPODI_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPODI_d",
                                                       uiOutput("PCTPODI_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTPWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPWT_d",
                                                       uiOutput("PCTPWT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPWT_d",
                                                       uiOutput("PCTPWT_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTPOR_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPOR_d",
                                                       uiOutput("PCTPOR_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPOR_d",
                                                       uiOutput("PCTPOR_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTPCCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPCCT_d",
                                                       uiOutput("PCTPCCT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPCCT_d",
                                                       uiOutput("PCTPCCT_max_input")
                                                     )
                                              )))
                                     
                                   ), #end Dimentional Attribute 1
                                   fluidRow(
                                     column(2,
                                            fluidRow(uiOutput("PCTDIDI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDIDI_d",
                                                       uiOutput("PCTDIDI_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDIDI_d",
                                                       uiOutput("PCTDIDI_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTDODI_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDODI_d",
                                                       uiOutput("PCTDODI_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDODI_d",
                                                       uiOutput("PCTDODI_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTDWT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDWT_d",
                                                       uiOutput("PCTDWT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDWT_d",
                                                       uiOutput("PCTDWT_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTDOR_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDOR_d",
                                                       uiOutput("PCTDOR_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDOR_d",
                                                       uiOutput("PCTDOR_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTDCCT_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDCCT_d",
                                                       uiOutput("PCTDCCT_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDCCT_d",
                                                       uiOutput("PCTDCCT_max_input")
                                                     )
                                              )))
                                     
                                   ), #end Dimentional Attribute 2
                                   fluidRow(
                                     
                                     column(2,
                                            fluidRow(uiOutput("PCTPLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPLength_d",
                                                       uiOutput("PCTPLength_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPLength_d",
                                                       uiOutput("PCTPLength_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTTLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTLength_d",
                                                       uiOutput("PCTTLength_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTLength_d",
                                                       uiOutput("PCTTLength_max_input")
                                                     )
                                              ))),
                                     
                                     column(2,
                                            fluidRow(uiOutput("PCTDLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDLength_d",
                                                       uiOutput("PCTDLength_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDLength_d",
                                                       uiOutput("PCTDLength_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTToLength_s")),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTToLength_d",
                                                       uiOutput("PCTToLength_min_input")
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTToLength_d",
                                                       uiOutput("PCTToLength_max_input")
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(uiOutput("PCTPPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTPPD_d",
                                                uiOutput("PCTPPD_input")
                                              )))
                                     
                                   ),#end of attribute 3
                                   #Special Operation
                                   fluidRow(
                                     tags$h1(strong("Special Operation"),style="font-size:25px;",align="left"),
                                     column(1,
                                            fluidRow(uiOutput("PCTNEXIV_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTNEXIV_d",
                                                uiOutput("PCTNEXIV_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTAnnealed_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTAnnealed_d",
                                                uiOutput("PCTAnnealed_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTCaliper_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTCaliper_d",
                                                uiOutput("PCTCaliper_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTOS_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTOS_d",
                                                uiOutput("PCTOS_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTMP_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTMP_d",
                                                uiOutput("PCTMP_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTHT_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTHT_d",
                                                uiOutput("PCTHT_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTSPD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTSPD_d",
                                                uiOutput("PCTSPD_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTSLD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTSLD_d",
                                                uiOutput("PCTSLD_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTDLN_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTDLN_d",
                                                uiOutput("PCTDLN_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTULT_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTULT_d",
                                                uiOutput("PCTULT_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTVC_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTVC_d",
                                                uiOutput("PCTVC_input")
                                              ))),
                                     column(1,
                                            fluidRow(uiOutput("PCTIRD_s")),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTIRD_d",
                                                uiOutput("PCTIRD_input")
                                              )))
                                   ), #end Special Operation
          
                                   fluidRow(
                                     DT::dataTableOutput("mytable3")
                                     )
                                   )#end Tapered Extrusion PPS Data
                          ),
               #MES Data table rendering
               navbarMenu("MES Data",
                          #Single Extrusion PPS Data
                          tabPanel("MES Parameters and Yield",
                                   fluidRow(
                                     DT::dataTableOutput("MESparameters")
                                   )
                          ),
                          tabPanel("MES Time Stamps",
                                   fluidRow(
                                     DT::dataTableOutput("MEStime")
                                   )
                          ),
                          tabPanel("MES Submitters",
                                   fluidRow(
                                     DT::dataTableOutput("MESsubmitter")
                                   )
                          ),
                          tabPanel("MES Total",
                                   fluidRow(
                                     DT::dataTableOutput("MEStotal")
                                   )
                          )
               ),
               
               #Scrap Codes table rendering
               tabPanel("Scrap Codes",
                        fluidRow(
                          DT::dataTableOutput("scrapcodes")
                        )
               ),
               
               #Applied Stats Data table rendering
               navbarMenu("AppStats Data",
                          #Single Extrusion PPS Data
                          tabPanel("Nexiv",
                                   fluidRow(
                                     DT::dataTableOutput("nexiv")
                                   )
                          ),
                          tabPanel("Laserlinc",
                                   fluidRow(
                                     DT::dataTableOutput("laserlinc")
                                   )
                          )
               ),
               
               #Shopping Cart
               #'This renders the shopping cart in an absolute panels that is always visible and
               #'allows for a user to select the output data with associated batches
               absolutePanel("Shopping Cart",
                 bottom = 100, right = 20, width = 600,
                 draggable = TRUE,
                 wellPanel(
                   dataTableOutput("shoppingcart")
                 ),
                 style = "opacity: 0.92"
               ) #end absolutePanel#end Part Catalog
)#end ui


