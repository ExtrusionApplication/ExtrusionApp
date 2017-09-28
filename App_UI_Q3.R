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


#_s:name of the checkbox
#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

ui<-navbarPage("Extrusion Application",
               navbarMenu("Part Catalog",   #Create a dropdown list named as Part Catalog containing of Single, Multi,and Tapered
                          #Single Extrusion PPS Data
                          tabPanel("Single Layer Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"), #Use tag to add a hearder 
                                     #Part Number
                                     column(2,

                                            fluidRow(checkboxInput("PCSPN_d", "Part Number", value = TRUE)),  #Show the checkbox for Part number. it will return a True/False value
                                            fluidRow(
                                              conditionalPanel(
                                                condition="input.PCSPN_d",   #If it were Ture, then there will have a search box for Part Number under checkbox
                                                selectInput("PCSPN",label = NULL,
                                                               c("All",unique(as.character(single_pps_data$`Part Number`))))
                                                
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
                                   #Resin Information
                                   fluidRow(
                                     tags$h1(strong("Resin Information"),style="font-size:25px;",align="left"),
                                     #Resin Families
                                     column(2,
                                            fluidRow(checkboxInput("PCSRF_d","Resin Families",value=F))
                                     ), 
                                     #Is Resin Blended with Anything?
                                     column(2,
                                            fluidRow(checkboxInput("PCSRBQ_d","Is Resin Blended with Anything?",value=F))
                                            ), 
                                     #Is Resin a Polymer Blend?
                                     column(2,
                                            fluidRow(checkboxInput("PCSRPBQ_d","Is Resin a Polymer Blend?",value=F))
                                            ),
                                     #Is Resin Filled?
                                     column(2,
                                            fluidRow(checkboxInput("PCSRFQ_d","Is Resin Filled?",value=F))
                                            ),
                                     #Resin Fillers
                                     column(2,
                                            fluidRow(checkboxInput("PCSRFi_d","Resin Fillers",value=F))
                                            ),
                                     #Is Resin Colored?
                                     column(2,
                                            fluidRow(checkboxInput("PCSRCQ_d","Is Resin Colored?",value=F))
                                            ),
                                     #Resin Color
                                     column(2,
                                            fluidRow(checkboxInput("PCSRC_d","Resin Color",value=F))
                                            ),
                                     #Is Resin Radiopaque?
                                     column(2,
                                            fluidRow(checkboxInput("PCSRRQ_d","Is Resin Radiopaque?",value=F))
                                            ),
                                     #Resin Durometer
                                     column(2,
                                            fluidRow(checkboxInput("PCSRDu_d","Resin Durometer (D)",value=F))
                                            ),
                                     #Average Resin Durometer
                                     column(2,
                                            fluidRow(checkboxInput("PCSRADu_d","Average Durometer (D)",value=F))
                                            )
                                   ),#end Resin Information
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checksingleresininfo", "Show All Resin Information"),
                                     actionButton("unchecksingleresininfo", "Hide All Resin Information")
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
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checksingletooling", "Show All Tooling"),
                                     actionButton("unchecksingletooling", "Hide All Tooling")
                                   ),
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Parameters"),style="font-size:25px;",align="left"),
                                     
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
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checksingleparameters", "Show All Processing Parameters"),
                                     actionButton("unchecksingleparameters", "Hide All Processing Parameters")
                                   ),
                                   
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
                                                column(6,numericInput("PCSCCT_max",label = NULL,value=PCSCCTmax,step=0.0001))
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
                                                column(6,numericInput("PCSLength_max",label = NULL,value=PCSLengthmax,step=1))
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
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checksingledimensions", "Show All Dimensional Attributes"),
                                     actionButton("unchecksingledimensions", "Hide All Dimensional Attributes")
                                   ),
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
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checksinglespecial", "Show All Special Operations"),
                                     actionButton("unchecksinglespecial", "Hide All Special Operations")
                                   ),
                                   
                                   # Show Table
                                   fluidRow(
                                     DT::dataTableOutput("mytable1")
                                   ),
                                   fluidRow(
                                     downloadButton('singledownloadSPPSData','Download Single PPS Data'),
                                     downloadButton('singledownloadSPPSDataAll','Download Single PPS Data with All Parameters'),
                                     actionButton('resetsingleinputs', "Reset Single Input Parameters")
                                   )
                          ),#end Single Extrusion PPS Data
                          #Multi Extrusion PPS Data---UI
                          tabPanel("Multi-Layer Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"),
                                     #Part Number
                                     column(2,
                                            fluidRow(checkboxInput("PCMPN_d","Part Number",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition="input.PCMPN_d",

                                                selectInput("PCMPN",label = NULL,
                                                               c("All",unique(as.character(multi_pps_data$`Part Number`))))

                                              ))),
                                     # Part Description
                                     column(2,
                                            fluidRow(checkboxInput("PCMPD_d","Part Description",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMPD_d",
                                                selectInput("PCMPD",label = NULL,
                                                            c("All",unique(as.character(multi_pps_data$`Part Description`))))
                                              ))), 
                                     # Resin Number
                                     column(2,
                                            fluidRow(checkboxInput("PCMRN_d","Resin Number",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMRN_d",
                                                selectInput("PCMRN",label = NULL,
                                                            c("All",unique(as.character(multi_pps_data$`Resin Number`))))
                                              ))),
                                     #Resin Description
                                     column(2,
                                            fluidRow(checkboxInput("PCMRD_d","Resin Description",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMRD_d",
                                                selectInput("PCMRD",label = NULL,

                                                            c("All",unique(as.character(multi_pps_data$`Resin Description`))))

                                              ))),
                                     #PPS Number
                                     column(2,
                                            fluidRow(checkboxInput("PCMPPSN_d","PPS Number",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMPPSN_d",
                                                selectInput("PCMPPSN",label = NULL,
                                                            c("All",unique(as.character(multi_pps_data$`PPS Number`))))

                                              )))

                                   ),
                                   
                                   #Resin Information
                                   fluidRow(
                                     tags$h1(strong("Resin Information"),style="font-size:25px;",align="left"),
                                     #Resin Families
                                     column(2,
                                            fluidRow(checkboxInput("PCMRF_d","Resin Families",value=F))
                                     ), 
                                     #Is Resin Blended with Anything?
                                     column(2,
                                            fluidRow(checkboxInput("PCMRBQ_d","Is Resin Blended with Anything?",value=F))
                                     ), 
                                     #Is Resin a Polymer Blend?
                                     column(2,
                                            fluidRow(checkboxInput("PCMRPBQ_d","Is Resin a Polymer Blend?",value=F))
                                     ),
                                     #Is Resin Filled?
                                     column(2,
                                            fluidRow(checkboxInput("PCMRFQ_d","Is Resin Filled?",value=F))
                                     ),
                                     #Resin Fillers
                                     column(2,
                                            fluidRow(checkboxInput("PCMRFi_d","Resin Fillers",value=F))
                                     ),
                                     #Is Resin Colored?
                                     column(2,
                                            fluidRow(checkboxInput("PCMRCQ_d","Is Resin Colored?",value=F))
                                     ),
                                     #Resin Color
                                     column(2,
                                            fluidRow(checkboxInput("PCMRC_d","Resin Color",value=F))
                                     ),
                                     #Is Resin Radiopaque?
                                     column(2,
                                            fluidRow(checkboxInput("PCMRRQ_d","Is Resin Radiopaque?",value=F))
                                     ),
                                     #Resin Durometer
                                     column(2,
                                            fluidRow(checkboxInput("PCMRDu_d","Resin Durometer (D)",value=F))
                                     ),
                                     #Average Resin Durometer
                                     column(2,
                                            fluidRow(checkboxInput("PCMRADu_d","Average Durometer (D)",value=F))
                                     )
                                   ),#end Resin Information
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checkmultiresininfo", "Show All Resin Information"),
                                     actionButton("uncheckmultiresininfo", "Hide All Resin Information")
                                   ),
                                   #Tooling
                                   fluidRow(
                                     tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                     #Extrusion Type
                                     column(2,
                                            fluidRow(checkboxInput("PCMET_d","Extrusion Type",value=T))
                                     ),
                                     #Barrel
                                     column(2,
                                            fluidRow(checkboxInput("PCMB_d","Barrel",value=F))
                                            ),
                                     #Die Size
                                     column(2,
                                            fluidRow(checkboxInput("PCMDS_d","Die Size (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDS_d",
                                                       numericInput("PCMDS_min",label = NULL,value=PCMDSmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDS_d",
                                                       numericInput("PCMDS_max",label = NULL,value=PCMDSmax,step=0.001)
                                                     )))),
                                     #Die Land Length
                                     column(2,
                                            fluidRow(checkboxInput("PCMDLL_d","Die Land Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMDLL_d",
                                                selectInput("PCMDLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Die Land Length (in)`))))
                                              ))),
                                     #Tip Size
                                     column(2,
                                            fluidRow(checkboxInput("PCMTS_d","Tip Size (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTS_d",
                                                       numericInput("PCMTS_min",label = NULL,value=PCMTSmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTS_d",
                                                       numericInput("PCMTS_max",label = NULL,value=PCMTSmax,step=0.001)
                                                     )
                                              ))),
                                     #Tip Land Length
                                     column(2,
                                            fluidRow(checkboxInput("PCMTLL_d","Tip Land Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMTLL_d",
                                                selectInput("PCMTLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
                                              ))),
                                     #Screw Print
                                     column(2,
                                            fluidRow(checkboxInput("PCMSP_d","Screw Print",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMSP_d",
                                                selectInput("PCMSP",label = NULL,
                                                            c("All",unique(as.character(multi_pps_data$`Screw Print`))))
                                              )))
                                   ),#end Tooling
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checkmultitooling", "Show All Tooling"),
                                     actionButton("uncheckmultitooling", "Hide All Tooling")
                                   ),
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Attribute"),style="font-size:25px;",align="left"),
                                     
                                     #Feedthroat
                                     column(3,
                                            fluidRow(checkboxInput("PCMFT_d","Feedthroat Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMFT_d",
                                                       numericInput("PCMFT_min",label = NULL,value = PCMFTmin,step=1)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMFT_d",
                                                       numericInput("PCMFT_max",label = NULL,value=PCMFTmax,step=1)
                                                     )
                                              ))),
                                     #Barrel Zone 1
                                     column(3,
                                            fluidRow(checkboxInput("PCMBZT1_d","Barrel Zone 1 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT1_d",
                                                       numericInput("PCMBZT1_min",label = NULL,value=PCMBZT1min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT1_d",
                                                       numericInput("PCMBZT1_max",label = NULL,value=PCMBZT1max,step=5)
                                                     )
                                              ))),
                                     #Barrel ZOne2
                                     column(3,
                                            fluidRow(checkboxInput("PCMBZT2_d","Barrel Zone 2 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT2_d",
                                                       numericInput("PCMBZT2_min",label = NULL,value=PCMBZT2min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT2_d",
                                                       numericInput("PCMBZT2_max",label = NULL,value=PCMBZT2max,step=5)
                                                     )
                                              ))),
                                     #Barrel Zone3
                                     column(3,
                                            fluidRow(checkboxInput("PCMBZT3_d","Barrel Zone 3 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT3_d",
                                                       numericInput("PCMBZT3_min",label = NULL,value=PCMBZT3min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMBZT3_d",
                                                       numericInput("PCMBZT3_max",label = NULL,value=PCMBZT3max,step=5)
                                                     )
                                              )))
                                   ),#end Processing Attribute 1
                                   fluidRow(
                                     column(3,
                                            fluidRow(checkboxInput("PCMCT_d","Clamp Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCT_d",
                                                       numericInput("PCMCT_min",label = NULL,value=PCMCTmin,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCT_d",
                                                       numericInput("PCMCT_max",label = NULL,value=PCMCTmax,step=5)
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(checkboxInput("PCMAT_d","Adapter Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMAT_d",
                                                       numericInput("PCMAT_min",label = NULL,value=PCMATmin,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMAT_d",
                                                       numericInput("PCMAT_max",label = NULL,value=PCMATmax,step=5)
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(checkboxInput("PCMDT1_d","Die 1 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT1_d",
                                                       numericInput("PCMDT1_min",label = NULL,value=PCMDT1min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT1_d",
                                                       numericInput("PCMDT1_max",label = NULL,value=PCMDT1max,step=5)
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(checkboxInput("PCMDT2_d","Die 2 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT2_d",
                                                       numericInput("PCMDT2_min",label = NULL,value=PCMDT2min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMDT2_d",
                                                       numericInput("PCMDT2_max",label = NULL,value=PCMDT2max,step=5)
                                                     )
                                              )))), #end Processing Attribute 2
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checkmultiparameters", "Show All Processing Parameters"),
                                     actionButton("uncheckmultiparameters", "Hide All Processing Parameters")
                                   ),
                                   #Dimentional Attribute
                                   fluidRow(
                                     tags$h1(strong("Dimentional Attribute"),style="font-size:25px;",align="left"),
                                     column(2,

                                            fluidRow(checkboxInput("PCMTE_d","Tapered End",value=FALSE))
                                            ),
                                     
                                     column(2,

                                            fluidRow(checkboxInput("PCMIDI_d","Inner Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIDI_d",
                                                       numericInput("PCMIDI_min",label = NULL,value=PCMIDImin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIDI_d",
                                                       numericInput("PCMIDI_max",label = NULL,value=PCMIDImax,step=0.001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMODI_d","Outer Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMODI_d",
                                                       numericInput("PCMODI_min",label = NULL,value=PCMODImin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMODI_d",
                                                       numericInput("PCMODI_max",label = NULL,value=PCMODImax,step=0.001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMIWT_d","Inner Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIWT_d",
                                                       numericInput("PCMIWT_min",label = NULL,value=PCMIWTmin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMIWT_d",
                                                       numericInput("PCMIWT_max",label = NULL,value=PCMIWTmax,step=0.001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMMWT_d","Middle Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMMWT_d",
                                                       numericInput("PCMMWT_min",label = NULL,value=PCMMWTmin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMMWT_d",
                                                       numericInput("PCMMWT_max",label = NULL,value=PCMMWTmax,step=0.001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMOWT_d","Outer Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOWT_d",
                                                       numericInput("PCMOWT_min",label = NULL,value=PCMOWTmin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOWT_d",
                                                       numericInput("PCMOWT_max",label = NULL,value=PCMOWTmax,step=0.001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMTWT_d","Total Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTWT_d",
                                                       numericInput("PCMTWT_min",label = NULL,value=PCMTWTmin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMTWT_d",
                                                       numericInput("PCMTWT_max",label = NULL,value=PCMTWTmax,step=0.001)
                                                     ))))),
                                   
                                   fluidRow(
                                     column(2,
                                            fluidRow(checkboxInput("PCMOR_d","Out of Roundness (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOR_d",
                                                       numericInput("PCMOR_min",label = NULL,value=PCMORmin,step=0.001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMOR_d",
                                                       numericInput("PCMOR_max",label = NULL,value=PCMORmax,step=0.001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMCCT_d","Concentricity",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCCT_d",
                                                       numericInput("PCMCCT_min",label = NULL,value=PCMCCTmin,step=0.0001)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMCCT_d",
                                                       numericInput("PCMCCT_max",label = NULL,value=PCMCCTmax,step=0.0001)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMLength_d","Length (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMLength_d",
                                                       numericInput("PCMLength_min",label = NULL,value=PCMLengthmin,step=1)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMLength_d",
                                                       numericInput("PCMLength_max",label = NULL,value=PCMLengthmax,step=1)
                                                     )))),
                                     column(2,
                                            fluidRow(checkboxInput("PCMToLength_d","Total Length (in)",value=T)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMToLength_d",
                                                       numericInput("PCMToLength_min",label = NULL,value=PCMToLengthmin,step=1)
                                                     )),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCMToLength_d",
                                                       numericInput("PCMToLength_max",label = NULL,value=PCMToLengthmax,step=1)
                                                     )))),
                                     
                                     column(2,
                                            fluidRow(checkboxInput("PCMPPD_d","Perpendicularity (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMPPD_d",
                                                selectInput("PCMPPD",label = NULL,
                                                            c("All",unique(as.character(multi_pps_data$`Perpendicularity (in)`))))
                                              )))), #End Dimentional Attribute
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checkmultidimensions", "Show All Dimensional Attributes"),
                                     actionButton("uncheckmultidimensions", "Hide All Dimensional Attributes")
                                   ),
                                   #Special Operation
                                   fluidRow(
                                     tags$h1(strong("Special Operation"),style="font-size:25px;",align="left"),
                                     column(1,
                                            fluidRow(checkboxInput("PCMNEXIV_d","NEXIV",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMNEXIV_d",
                                                selectInput("PCMNEXIV",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMAnnealed_d","Annealed",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMAnnealed_d",
                                                selectInput("PCMAnnealed",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMCaliper_d","Caliper",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMCaliper_d",
                                                selectInput("PCMCaliper",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMOS_d","OD Sort",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMOS_d",
                                                selectInput("PCMOS",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMMP_d","Melt Pump",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMMP_d",
                                                selectInput("PCMMP",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMHT_d","Hypo Tip",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMHT_d",
                                                selectInput("PCMHT",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMSPD_d","Sparker Die",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMSPD_d",
                                                selectInput("PCMSPD",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMSLD_d","Slicking Die",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMSLD_d",
                                                selectInput("PCMSLD",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMDLN_d","Delamination",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMDLN_d",
                                                selectInput("PCMDLN",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMULT_d","Ultrasonic",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMULT_d",
                                                selectInput("PCMULT",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMVC_d","Vacuum Calibration",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMVC_d",
                                                selectInput("PCMVC",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCMIRD_d","Irradiated",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCMIRD_d",
                                                selectInput("PCMIRD",label = NULL,choices=c("All","yes","NA"))
                                              )))
                                   ), #end Special Operation
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checkmultispecial", "Show All Special Operations"),
                                     actionButton("uncheckmultispecial", "Hide All Special Operations")
                                   ),

                                   

                                   fluidRow(
                                     DT::dataTableOutput("mytable2")
                                   ),
                                   fluidRow(
                                     downloadButton('multidownloadSPPSData','Download Multi-Layer PPS Data'),
                                     downloadButton('multidownloadSPPSDataAll','Download Multi-Layer PPS Data with All Parameters'),
                                     actionButton('resetmultiinputs', "Reset Multi-Layer Input Parameters")
                                   )
                                   
                          ),#end multi Extrusion PPS Data
                          
                          #Tapered Extrusion PPS Data--UI
                          tabPanel("Tapered Extrusion PPS Data",
                                   #Part Resin
                                   fluidRow(
                                     tags$h1(strong("Part Resin"),style="font-size:25px;",align="left"),
                                     #Part Number
                                     column(2,
                                            fluidRow(checkboxInput("PCTPN_d","Part Number",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition="input.PCTPN_d",

                                                selectInput("PCTPN",label = NULL,
                                                               c("All",unique(as.character(tapered_pps_data$`Part Number`))))

                                              ))),
                                     # Part Description
                                     column(2,
                                            fluidRow(checkboxInput("PCTPD_d","Part Description",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTPD_d",
                                                selectInput("PCTPD",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`Part Description`))))
                                              ))), 
                                     # Resin Number
                                     column(2,
                                            fluidRow(checkboxInput("PCTRN_d","Resin Number",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTRN_d",
                                                selectInput("PCTRN",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`Resin Number`))))
                                              ))),
                                     #Resin Description
                                     column(2,
                                            fluidRow(checkboxInput("PCTRD_d","Resin Description",value=TRUE)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTRD_d",
                                                selectInput("PCTRD",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`Resin Description`))))
                                              ))),
                                     #PPS Number
                                     column(2,
                                            fluidRow(checkboxInput("PCTPPSN_d","PPS Number",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTPPSN_d",
                                                selectInput("PCTPPSN",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`PPS Number`))))
                                              )))
                                   ),
                                   
                                   #Resin Information
                                   fluidRow(
                                     tags$h1(strong("Resin Information"),style="font-size:25px;",align="left"),
                                     #Resin Families
                                     column(2,
                                            fluidRow(checkboxInput("PCTRF_d","Resin Families",value=F))
                                     ), 
                                     #Is Resin Blended with Anything?
                                     column(2,
                                            fluidRow(checkboxInput("PCTRBQ_d","Is Resin Blended with Anything?",value=F))
                                     ), 
                                     #Is Resin a Polymer Blend?
                                     column(2,
                                            fluidRow(checkboxInput("PCTRPBQ_d","Is Resin a Polymer Blend?",value=F))
                                     ),
                                     #Is Resin Filled?
                                     column(2,
                                            fluidRow(checkboxInput("PCTRFQ_d","Is Resin Filled?",value=F))
                                     ),
                                     #Resin Fillers
                                     column(2,
                                            fluidRow(checkboxInput("PCTRFi_d","Resin Fillers",value=F))
                                     ),
                                     #Is Resin Colored?
                                     column(2,
                                            fluidRow(checkboxInput("PCTRCQ_d","Is Resin Colored?",value=F))
                                     ),
                                     #Resin Color
                                     column(2,
                                            fluidRow(checkboxInput("PCTRC_d","Resin Color",value=F))
                                     ),
                                     #Is Resin Radiopaque?
                                     column(2,
                                            fluidRow(checkboxInput("PCTRRQ_d","Is Resin Radiopaque?",value=F))
                                     ),
                                     #Resin Durometer
                                     column(2,
                                            fluidRow(checkboxInput("PCTRDu_d","Resin Durometer (D)",value=F))
                                     ),
                                     #Average Resin Durometer
                                     column(2,
                                            fluidRow(checkboxInput("PCTRADu_d","Average Durometer (D)",value=F))
                                     )
                                   ),#end Resin Information
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checktaperedresininfo", "Show All Resin Information"),
                                     actionButton("unchecktaperedresininfo", "Hide All Resin Information")
                                   ),
                                   #Tooling
                                   fluidRow(
                                     tags$h1(strong("Tooling"),style="font-size:25px;",align="left"),
                                     #Die Size
                                     column(2,
                                            fluidRow(checkboxInput("PCTDS_d","Die Size (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDS_d",
                                                       numericInput("PCTDS_min",label = NULL,value=PCTDSmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDS_d",
                                                       numericInput("PCTDS_max",label = NULL,value=PCTDSmax,step=0.001)
                                                     )))),
                                     #Die Land Length
                                     column(2,
                                            fluidRow(checkboxInput("PCTDLL_d","Die Land Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTDLL_d",
                                                selectInput("PCTDLL",label = NULL,c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
                                              ))),
                                     #Tip Size
                                     column(2,
                                            fluidRow(checkboxInput("PCTTS_d","Tip Size (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTS_d",
                                                       numericInput("PCTTS_min",label = NULL,value=PCTTSmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTS_d",
                                                       numericInput("PCTTS_max",label = NULL,value=PCTTSmax,step=0.001)
                                                     )
                                              ))),
                                     #Tip Land Length
                                     column(2,
                                            fluidRow(checkboxInput("PCTTLL_d","Tip Land Length (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTTLL_d",
                                                selectInput("PCTTLL",label = NULL,c("All",unique(as.character(single_pps_data$`Tip Land Length (in)`))))
                                              ))),
                                     #Screw Print
                                     column(2,
                                            fluidRow(checkboxInput("PCTSP_d","Screw Print",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTSP_d",
                                                selectInput("PCTSP",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`Screw Print`))))
                                              )))
                                   ),#end Tooling
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checktaperedtooling", "Show All Tooling"),
                                     actionButton("unchecktaperedtooling", "Hide All Tooling")
                                   ),
                                   #Processing Attributes
                                   fluidRow(
                                     tags$h1(strong("Processing Attribute"),style="font-size:25px;",align="left"),
                                     
                                     #Feedthroat
                                     column(3,
                                            fluidRow(checkboxInput("PCTFT_d","Feedthroat Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTFT_d",
                                                       numericInput("PCTFT_min",label = NULL,value = PCTFTmin,step=1)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTFT_d",
                                                       numericInput("PCTFT_max",label = NULL,value=PCTFTmax,step=1)
                                                     )
                                              ))),
                                     #Barrel Zone 1
                                     column(3,
                                            fluidRow(checkboxInput("PCTBZT1_d","Barrel Zone 1 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT1_d",
                                                       numericInput("PCTBZT1_min",label = NULL,value=PCTBZT1min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT1_d",
                                                       numericInput("PCTBZT1_max",label = NULL,value=PCTBZT1max,step=5)
                                                     )
                                              ))),
                                     #Barrel ZOne2
                                     column(3,
                                            fluidRow(checkboxInput("PCTBZT2_d","Barrel Zone 2 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT2_d",
                                                       numericInput("PCTBZT2_min",label = NULL,value=PCTBZT2min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT2_d",
                                                       numericInput("PCTBZT2_max",label = NULL,value=PCTBZT2max,step=5)
                                                     )
                                              ))),
                                     #Barrel Zone3
                                     column(3,
                                            fluidRow(checkboxInput("PCTBZT3_d","Barrel Zone 3 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT3_d",
                                                       numericInput("PCTBZT3_min",label = NULL,value=PCTBZT3min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTBZT3_d",
                                                       numericInput("PCTBZT3_max",label = NULL,value=PCTBZT3max,step=5)
                                                     )
                                              )))
                                   ),#end Processing Attribute 1
                                   fluidRow(
                                     column(3,
                                            fluidRow(checkboxInput("PCTCT_d","Clamp Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTCT_d",
                                                       numericInput("PCTCT_min",label = NULL,value=PCTCTmin,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTCT_d",
                                                       numericInput("PCTCT_max",label = NULL,value=PCTCTmax,step=5)
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(checkboxInput("PCTAT_d","Adapter Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTAT_d",
                                                       numericInput("PCTAT_min",label = NULL,value=PCTATmin,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTAT_d",
                                                       numericInput("PCTAT_max",label = NULL,value=PCTATmax,step=5)
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(checkboxInput("PCTDT1_d","Die 1 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT1_d",
                                                       numericInput("PCTDT1_min",label = NULL,value=PCTDT1min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT1_d",
                                                       numericInput("PCTDT1_max",label = NULL,value=PCTDT1max,step=5)
                                                     )
                                              ))),
                                     column(3,
                                            fluidRow(checkboxInput("PCTDT2_d","Die 2 Temperature F",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT2_d",
                                                       numericInput("PCTDT2_min",label = NULL,value=PCTDT2min,step=5)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDT2_d",
                                                       numericInput("PCTDT2_max",label = NULL,value=PCTDT2max,step=5)
                                                     )
                                              )))), #end Processing Attribute 2
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checktaperedparameters", "Show All Tooling"),
                                     actionButton("unchecktaperedparameters", "Hide All Tooling")
                                   ),
                                   #Dimentional Attribute
                                   fluidRow(
                                     tags$h1(strong("Dimentional Attribute"),style="font-size:25px;",align="left"),
                                     column(2,
                                            fluidRow(checkboxInput("PCTPIDI_d","Proximal Inner Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPIDI_d",
                                                       numericInput("PCTPIDI_min",label = NULL,value=PCTPIDImin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPIDI_d",
                                                       numericInput("PCTPIDI_max",label = NULL,value=PCTPIDImax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTPODI_d","Proximal Outer Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPODI_d",
                                                       numericInput("PCTPODI_min",label = NULL,value=PCTPODImin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPODI_d",
                                                       numericInput("PCTPODI_max",label = NULL,value=PCTPODImax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTPWT_d","Proximal Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPWT_d",
                                                       numericInput("PCTPWT_min",label = NULL,value=PCTPWTmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPWT_d",
                                                       numericInput("PCTPWT_max",label = NULL,value=PCTPWTmax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTPOR_d","Proximal Out of Roundness (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPOR_d",
                                                       numericInput("PCTPOR_min",label = NULL,value=PCTPORmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPOR_d",
                                                       numericInput("PCTPOR_max",label = NULL,value=PCTPORmax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTPCCT_d","Proximal Concentricity (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPCCT_d",
                                                       numericInput("PCTPCCT_min",label = NULL,value=PCTPCCTmin,step=0.0001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPCCT_d",
                                                       numericInput("PCTPCCT_max",label = NULL,value=PCTPCCTmax,step=0.0001)
                                                     )
                                              )))
                                     
                                   ), #end Dimentional Attribute 1
                                   fluidRow(
                                     column(2,
                                            fluidRow(checkboxInput("PCTDIDI_d","Distal Inner Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDIDI_d",
                                                       numericInput("PCTDIDI_min",label = NULL,value=PCTDIDImin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDIDI_d",
                                                       numericInput("PCTDIDI_max",label = NULL,value=PCTDIDImax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTDODI_d","Distal Outer Diameter (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDODI_d",
                                                       numericInput("PCTDODI_min",label = NULL,value=PCTDODImin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDODI_d",
                                                       numericInput("PCTDODI_max",label = NULL,value=PCTDODImax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTDWT_d","Distal Wall Thickness (in)",value=TRUE)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDWT_d",
                                                       numericInput("PCTDWT_min",label = NULL,value=PCTDWTmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDWT_d",
                                                       numericInput("PCTDWT_max",label = NULL,value=PCTDWTmax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTDOR_d","Distal Out of Roundness (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDOR_d",
                                                       numericInput("PCTDOR_min",label = NULL,value=PCTDORmin,step=0.001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDOR_d",
                                                       numericInput("PCTDOR_max",label = NULL,value=PCTDORmax,step=0.001)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTDCCT_d","Distal Concentricity (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDCCT_d",
                                                       numericInput("PCTDCCT_min",label = NULL,value=PCTDCCTmin,step=0.0001)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDCCT_d",
                                                       numericInput("PCTDCCT_max",label = NULL,value=PCTDCCTmax,step=0.0001)
                                                     )
                                              )))
                                     
                                   ), #end Dimentional Attribute 2
                                   fluidRow(
                                     
                                     column(2,
                                            fluidRow(checkboxInput("PCTPLength_d","Proximal Length (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPLength_d",
                                                       numericInput("PCTPLength_min",label = NULL,value=PCTPLengthmin,step=1)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTPLength_d",
                                                       numericInput("PCTPLength_max",label = NULL,value=PCTPLengthmax,step=1)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTTLength_d","Transition Length (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTLength_d",
                                                       numericInput("PCTTLength_min",label = NULL,value=PCTTLengthmin,step=1)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTTLength_d",
                                                       numericInput("PCTTLength_max",label = NULL,value=PCTTLengthmax,step=1)
                                                     )
                                              ))),
                                     
                                     column(2,
                                            fluidRow(checkboxInput("PCTDLength_d","Distal Length (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDLength_d",
                                                       numericInput("PCTDLength_min",label = NULL,value=PCTDLengthmin,step=1)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTDLength_d",
                                                       numericInput("PCTDLength_max",label = NULL,value=PCTDLengthmax,step=1)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTToLength_d","Total Length (in)",value=F)),
                                            fluidRow(
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTToLength_d",
                                                       numericInput("PCTToLength_min",label = NULL,value=PCTToLengthmin,step=1)
                                                     )
                                              ),
                                              column(6,
                                                     conditionalPanel(
                                                       condition = "input.PCTToLength_d",
                                                       numericInput("PCTToLength_max",label = NULL,value=PCTToLengthmax,step=1)
                                                     )
                                              ))),
                                     column(2,
                                            fluidRow(checkboxInput("PCTPPD_d","Perpendicularity (in)",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTPPD_d",
                                                selectInput("PCTPPD",label = NULL,
                                                            c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
                                              )))
                                     
                                   ),#end of attribute 3
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checktapereddimensions", "Show All Tooling"),
                                     actionButton("unchecktapereddimensions", "Hide All Tooling")
                                   ),
                                   #Special Operation
                                   fluidRow(
                                     tags$h1(strong("Special Operation"),style="font-size:25px;",align="left"),
                                     column(1,
                                            fluidRow(checkboxInput("PCTNEXIV_d","NEXIV",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTNEXIV_d",
                                                selectInput("PCTNEXIV",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTAnnealed_d","Annealed",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTAnnealed_d",
                                                selectInput("PCTAnnealed",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTCaliper_d","Caliper",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTCaliper_d",
                                                selectInput("PCTCaliper",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTOS_d","OD Sort",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTOS_d",
                                                selectInput("PCTOS",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTMP_d","Melt Pump",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTMP_d",
                                                selectInput("PCTMP",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTHT_d","Hypo Tip",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTHT_d",
                                                selectInput("PCTHT",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTSPD_d","Sparker Die",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTSPD_d",
                                                selectInput("PCTSPD",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTSLD_d","Slicking Die",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTSLD_d",
                                                selectInput("PCTSLD",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTDLN_d","Delamination",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTDLN_d",
                                                selectInput("PCTDLN",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTULT_d","Ultrasonic",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTULT_d",
                                                selectInput("PCTULT",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTVC_d","Vacuum Calibration",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTVC_d",
                                                selectInput("PCTVC",label = NULL,choices=c("All","yes","NA"))
                                              ))),
                                     column(1,
                                            fluidRow(checkboxInput("PCTIRD_d","Irradiated",value=F)),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.PCTIRD_d",
                                                selectInput("PCTIRD",label = NULL,choices=c("All","yes","NA"))
                                              )))
                                   ), #end Special Operation
                                   fluidRow(
                                     #fluid row for showing or deleting buttons
                                     actionButton("checktaperedspecial", "Show All Tooling"),
                                     actionButton("unchecktaperedspecial", "Hide All Tooling")
                                   ),
                                   fluidRow(
                                     DT::dataTableOutput("mytable3")
                                   ),
                                   fluidRow(
                                     downloadButton('tapereddownloadSPPSData','Download Tapered PPS Data'),
                                     downloadButton('tapereddownloadSPPSDataAll','Download Tapered PPS Data with All Parameters'),
                                     actionButton('resettaperedinputs', "Reset Tapered Input Parameters")
                                   )
                                   
                          )#end Tapered Extrusion PPS Data
               ),
               
               
               #Sampling and Test method information
               navbarMenu("Sampling and Test Method Information",
                          tabPanel("Single Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("single_sampling_ui")
                                   )
                          ),
                          tabPanel("Multi-Layer Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("multi_sampling_ui")
                                   )
                          ),
                          tabPanel("Tapered Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("tapered_sampling_ui")
                                   )
                          ),
                          tabPanel("Extra Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("extra_sampling_ui")
                                   )
                          ),
                          tabPanel("All Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("all_sampling_ui")
                                   )
                          )
               ), #End sampling and test method navbar
               
               #Single Extrusion MES Data table rendering
               navbarMenu("Single Extrusion MES Data",
                          tabPanel("MES Parameters and Yield",
                                   fluidRow(
                                     DT::dataTableOutput("singleMESparameters")
                                   )
                          ),
                          tabPanel("MES Time Stamps",
                                   fluidRow(
                                     DT::dataTableOutput("singleMEStime")
                                   )
                          ),
                          tabPanel("MES Submitters",
                                   fluidRow(
                                     DT::dataTableOutput("singleMESsubmitter")
                                   )
                          ),
                          tabPanel("MES Total",
                                   fluidRow(
                                     DT::dataTableOutput("singleMEStotal")
                                   )
                          )
               ),
               
               #Multi-Layer Extrusion MES Data table rendering
               navbarMenu("Multi-Layer Extrusion MES Data",
                          tabPanel("MES Parameters and Yield",
                                   fluidRow(
                                     DT::dataTableOutput("multiMESparameters")
                                   )
                          ),
                          tabPanel("MES Time Stamps",
                                   fluidRow(
                                     DT::dataTableOutput("multiMEStime")
                                   )
                          ),
                          tabPanel("MES Submitters",
                                   fluidRow(
                                     DT::dataTableOutput("multiMESsubmitter")
                                   )
                          ),
                          tabPanel("MES Total",
                                   fluidRow(
                                     DT::dataTableOutput("multiMEStotal")
                                   )
                          )
               ),
               
               #Tapered Extrusion MES Data table rendering
               navbarMenu("Tapered Extrusion MES Data",
                          tabPanel("Tapered Parameters and Yield",
                                   fluidRow(
                                     DT::dataTableOutput("taperedMESparameters")
                                   )
                          ),
                          tabPanel("Tapered Time Stamps",
                                   fluidRow(
                                     DT::dataTableOutput("taperedMEStime")
                                   )
                          ),
                          tabPanel("Tapered Submitters",
                                   fluidRow(
                                     DT::dataTableOutput("taperedMESsubmitter")
                                   )
                          ),
                          tabPanel("Tapered Total",
                                   fluidRow(
                                     DT::dataTableOutput("taperedMEStotal")
                                   )
                          )
               ),
               
               
               #Scrap Codes Data table rendering
               navbarMenu("Scrap Codes",
                          #Single Extrusion PPS Data
                          tabPanel("Single Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("singlescrapcodes")
                                   )
                          ),
                          tabPanel("Multi-Layer Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("multiscrapcodes")
                                   )
                          ),
                          tabPanel("Tapered Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("taperedscrapcodes")
                                   )
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
               ), #end the NavbarMenu
               
               #Extra Features
               navbarMenu("Extra Information",
                          #Single Extrusion PPS Data
                          tabPanel("Resin Information",
                                   fluidRow(
                                     DT::dataTableOutput("resin_data_ui")
                                   )
                          ),
                          tabPanel("Screw Print Information",
                                   fluidRow(
                                     DT::dataTableOutput("screw_data_ui")
                                   )
                          )
               ), #end the NavbarMenu
               
               
               #Shopping Cart PPS Data table rendering
               navbarMenu("Shopping Cart PPS Data",
                          #Single Extrusion PPS Data
                          tabPanel("Single Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("singleshoppingcartpps"),
                                     fluidRow(
                                       downloadButton('singlecartdownloadpps',
                                                      'Download Single Shopping Cart PPS Data')
                                     )
                                   )
                          ),
                          tabPanel("Multi-Layer Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("multishoppingcartpps"),
                                     fluidRow(
                                       downloadButton('multicartdownloadpps',
                                                      'Download Multi-Layer Shopping Cart PPS Data')
                                     )
                                   )
                          ),
                          tabPanel("Tapered Extrusion",
                                   fluidRow(
                                     DT::dataTableOutput("taperedshoppingcartpps"),
                                     fluidRow(
                                       downloadButton('taperedcartdownloadpps',
                                                      'Download Tapered Shopping Cart PPS Data')
                                     )
                                   )
                          )
               ),
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
                           tabPanel("Total Extrusion Cart"
                                    #Total Extrusion Parts
                           )
                         )
                         
                         
                         
                 ),
                 draggable = F,right = 20,top = 50, fixed = F,
                 style = "z-index = 1000"
                 
               )

               
               
               
               
               #Shopping Cart
               #'This renders the shopping cart in an absolute panels that is always visible and
               #'allows for a user to select the output data with associated batches
               #'
              
              
               #### Extra HTML ####

               
               
               
)#end ui
