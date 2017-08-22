#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

server<-function(input,output,session){
  
  #Part Catalog--Sinlge Extrusion PPS Data
  
  # obtain the output of checkbox from functions and make a list to store them----Single Extrusion PPS Data
  Col_PCS=c()
  show_vars1<-reactive({
    as.numeric(c(input$PCSPN_d,input$PCSPD_d,input$PCSRN_d,input$PCSRD_d,input$PCSPPSN_d,input$PCSDS_d,input$PCSDLL_d,input$PCSTS_d,input$PCSTLL_d,input$PCSSP_d,input$PCSFT_d,
                 input$PCSBZT1_d,input$PCSBZT2_d,input$PCSBZT3_d,input$PCSCT_d,input$PCSAT_d,input$PCSDT1_d,input$PCSDT2_d,input$PCSIDI_d,input$PCSODI_d,input$PCSWT_d,
                 input$PCSOR_d,input$PCSCCT_d,input$PCSLength_d,input$PCSPPD_d,input$PCSNEXIV_d,input$PCSAnnealed_d,input$PCSCaliper_d,input$PCSOS_d,input$PCSMP_d,input$PCSHT_d,
                 input$PCSSPD_d,input$PCSSLD_d,input$PCSDLN_d,input$PCSULT_d,input$PCSVC_d,input$PCSIRD_d))})
  
  observeEvent(input$testbutton,
               print("It Worked!"))


  # use all the input values from UI to modify table 1 and show the modified table
  output$mytable1 <- DT::renderDataTable({
    DT::datatable({
      col_var1=show_vars1()
      for (i in 1:length(col_var1)){
        if (col_var1[i]!=0){
          Col_PCS=c(Col_PCS,i)
        }
      }
      
      data_PCS<-single_pps_data[,Col_PCS]
      
      if(input$PCSPN!="All"){
        data_PCS<-data_PCS[data_PCS$`Part Number`==input$PCSPN,]
      }
      if(input$PCSPD!="All"){
        data_PCS<-data_PCS[data_PCS$`Part Description`==input$PCSPD,]
      }
      if(input$PCSRN!="All"){
        data_PCS<-data_PCS[data_PCS$`Resin Number`==input$PCSRN,]
      }
      if(input$PCSRD!="All"){
        data_PCS<-data_PCS[data_PCS$`Resin Descriptionr`==input$PCSRD,]
      }
      if(input$PCSPPSN!="All"){
        data_PCS<-data_PCS[data_PCS$`PPS Number`==input$PCSPPSN,]
      }
      if(input$PCSDS_min!=PCSDSmin || input$PCSDS_max!=PCSDSmax){
        data_PCS<-data_PCS[data_PCS$`Die Size (in)`>=input$PCSDS_min & data_PCS$`Die Size (in)`<=input$PCSDS_max,]
      }
      if(input$PCSDLL!="All"){
        data_PCS<-data_PCS[data_PCS$`Die Land Length (in)`==input$PCSDLL,]
      }
      if(input$PCSTS_min!=PCSTSmin || input$PCSTS_max!=PCSTSmax){
        data_PCS<-data_PCS[data_PCS$`Die Size (in)`>=input$PCSTS_min & data_PCS$`Die Size (in)`<=input$PCSTS_max,]
      }
      if(input$PCSTLL!="All"){
        data_PCS<-data_PCS[data_PCS$`Tip Land Length (in)`==input$PCSTLL,]
      }      
      if(input$PCSSP!="All"){
        data_PCS<-data_PCS[data_PCS$`Screw Print`==input$PCSSP,]
      }
      if(input$PCSFT_min!=PCSFTmin || input$PCSFT_max!=PCSFTmax){
        data_PCS<-data_PCS[data_PCS$`Feedthroat Temperature  F`>=input$PCSFT_min & data_PCS$`Feedthroat Temperature  F`<=input$PCSFT_max,]
      }
      if(input$PCSBZT1_min!=PCSBZT1min || input$PCSBZT1_max!=PCSBZT1max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 1 Temperature  F`>=input$PCSBZT1_min & data_PCS$`Barrel Zone 1 Temperature  F`<=input$PCSBZT1_max,]
      }
      if(input$PCSBZT2_min!=PCSBZT2min || input$PCSBZT2_max!=PCSBZT2max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 2 Temperature  F`>=input$PCSBZT2_min & data_PCS$`Barrel Zone 2 Temperature  F`<=input$PCSBZT2_max,]
      }
      if(input$PCSBZT3_min!=PCSBZT3min || input$PCSBZT3_max!=PCSBZT3max){
        data_PCS<-data_PCS[data_PCS$`Barrel Zone 3 Temperature  F`>=input$PCSBZT3_min & data_PCS$`Barrel Zone 3 Temperature  F`<=input$PCSBZT3_max,]
      }
      
      
      if(input$PCSCT_min!=PCSCTmin || input$PCSCT_max!=PCSCTmax){
        data_PCS<-data_PCS[data_PCS$`Clamp Temperature  F`>=input$PCSCT_min & data_PCS$`Clamp Temperature  F`<=input$PCSCT_max,]
      }
      if(input$PCSAT_min!=PCSATmin || input$PCSAT_max!=PCSATmax){
        data_PCS<-data_PCS[data_PCS$`Adapter Temperature  F`>=input$PCSAT_min & data_PCS$`Adapter Temperature  F`<=input$PCSAT_max,]
      }
      if(input$PCSDT1_min!=PCSDT1min || input$PCSDT1_max!=PCSDT1max){
        data_PCS<-data_PCS[data_PCS$`Die 1 Temperature  F`>=input$PCSDT1_min & data_PCS$`Die 1 Temperature  F`<=input$PCSDT1_max,]
      }
      if(input$PCSDT2_min!=PCSDT2min || input$PCSDT2_max!=PCSDT2max){
        data_PCS<-data_PCS[data_PCS$`Die 2 Temperature  F`>=input$PCSDT2_min & data_PCS$`Die 2 Temperature  F`<=input$PCSDT2_max,]
      }
      if(input$PCSIDI_min!=PCSIDImin || input$PCSIDI_max!=PCSIDImax){
        data_PCS<-data_PCS[data_PCS$`Inner Diameter (in)`>=input$PCSIDI_min & data_PCS$`Inner Diameter (in)`<=input$PCSIDI_max,]
      }
      if(input$PCSODI_min!=PCSODImin || input$PCSODI_max!=PCSODImax){
        data_PCS<-data_PCS[data_PCS$`Outer Diameter (in)`>=input$PCSODI_min & data_PCS$`Outer Diameter (in)`<=input$PCSODI_max,]
      }
      if(input$PCSWT_min!=PCSWTmin || input$PCSWT_max!=PCSWTmax){
        data_PCS<-data_PCS[data_PCS$`Wall Thickness (in)`>=input$PCSWT_min & data_PCS$`Wall Thickness (in)`<=input$PCSWT_max,]
      }
      if(input$PCSOR_min!=PCSORmin || input$PCSOR_max!=PCSORmax){
        data_PCS<-data_PCS[data_PCS$`Out of Roundness (in)`>=input$PCSOR_min & data_PCS$`Out of Roundness (in)`<=input$PCSOR_max,]
      }
      if(input$PCSCCT_min!=PCSCCTmin || input$PCSCCT_max!=PCSCCTmax){
        data_PCS<-data_PCS[data_PCS$`Concentricity (in)`>=input$PCSCCT_min & data_PCS$`Concentricity (in)`<=input$PCSCCT_max,]
      }
      if(input$PCSLength_min!=PCSLengthmin || input$PCSLength_max!=PCSLengthmax){
        data_PCS<-data_PCS[data_PCS$`Length (in)`>=input$PCSLength_min & data_PCS$`Length (in)`<=input$PCSLength_max,]
      }
      
    # For sepcial operation, it user choose yes or Na instead of All, then only the selected value will be showed
      if(input$PCSNEXIV!="All"){
        if(input$PCSNEXIV=="yes"){
          data_PCS<-data_PCS[data_PCS$`NEXIV`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`NEXIV`=="",]
        }
      }
      if(input$PCSAnnealed!="All"){
        if(input$PCSAnnealed=="yes"){
          data_PCS<-data_PCS[data_PCS$`Annealed`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Annealed`=="",]
        }
      }
      if(input$PCSCaliper!="All"){
        if(input$PCSCaliper=="yes"){
          data_PCS<-data_PCS[data_PCS$`Caliper`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Caliper`=="",]
        }
      }
      if(input$PCSOS!="All"){
        if(input$PCSOS=="yes"){
          data_PCS<-data_PCS[data_PCS$`OD Sort`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`OD Sort`=="",]
        }
      }
      if(input$PCSMP!="All"){
        if(input$PCSMP=="yes"){
          data_PCS<-data_PCS[data_PCS$`Melt Pump`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Melt Pump`=="",]
        }
      }
      if(input$PCSHT!="All"){
        if(input$PCSHT=="yes"){
          data_PCS<-data_PCS[data_PCS$`Hypo Tip`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Hypo Tip`=="",]
        }
      }
      if(input$PCSSPD!="All"){
        if(input$PCSSPD=="yes"){
          data_PCS<-data_PCS[data_PCS$`Sparker Die`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Sparker Die`=="",]
        }
      }
      if(input$PCSSLD!="All"){
        if(input$PCSSLD=="yes"){
          data_PCS<-data_PCS[data_PCS$`Slicking Die`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Slicking Die`=="",]
        }
      }
      if(input$PCSDLN!="All"){
        if(input$PCSDLN=="yes"){
          data_PCS<-data_PCS[data_PCS$`Delamination`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Delamination`=="",]
        }
      }
      if(input$PCSULT!="All"){
        if(input$PCSULT=="yes"){
          data_PCS<-data_PCS[data_PCS$`Ultrasonic`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Ultrasonic`=="",]
        }
      }
      if(input$PCSVC!="All"){
        if(input$PCSVC=="yes"){
          data_PCS<-data_PCS[data_PCS$`Vacuum Calibration`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Vacuum Calibration`=="",]
        }
      }
      if(input$PCSIRD!="All"){
        if(input$PCSIRD=="yes"){
          data_PCS<-data_PCS[data_PCS$`Irradiated`=="yes",]
        } else{
          data_PCS<-data_PCS[data_PCS$`Irradiated`=="",]
        }
      }
      #*******************The next lines add a first column that contains buttons to add parts to the shopping cart****************
      rows <- nrow(data_PCS)
      vectorofbuttons <- c(rep(0, rows))
      row_count <- 1
      
      while(row_count < rows + 1){
        #this creates a vector of html action buttons to add to the table
        vectorofbuttons[row_count] <- as.character(
          actionButton(inputId = paste0("button_", data_PCS[row_count,1]),
                       label = "Add Part",
                       onclick = 'Shiny.onInputChange(\"add_button\",  this.id)')
        )
        row_count <- row_count + 1
      } #end while adding the html stuff
      
      data_PCS$"" <- vectorofbuttons
      data_PCS <- data_PCS[,c(ncol(data_PCS), 1:(ncol(data_PCS)-1))]
      return(data_PCS)
    }
    )
  },
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',
                                        targets = "_all"
                 )
                 ),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE),
  filter = "top",
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE) #end Single Extrusion PPS Data
     
  
  

  #Part Catalog--multi Extrusion PPS Data
  #Checkbox
  output$PCMPN_s<-renderUI({
    checkboxInput("PCMPN_d","Part Number",value=TRUE)
  })
  output$PCMPD_s<-renderUI({
    checkboxInput("PCMPD_d","Part Description",value=TRUE)
  })
  output$PCMRN_s<-renderUI({
    checkboxInput("PCMRN_d","Resin Number",value=TRUE)
  })
  output$PCMRD_s<-renderUI({
    checkboxInput("PCMRD_d","Resin Description",value=TRUE)
  })
  output$PCMPPSN_s<-renderUI({
    checkboxInput("PCMPPSN_d","PPS Number",value=F)
  })
  #Tooling
  output$PCMDS_s<-renderUI({
    checkboxInput("PCMDS_d","Die Size (in)",value=T)
  })
  output$PCMDLL_s<-renderUI({
    checkboxInput("PCMDLL_d","Die Land Length (in)",value=F)
  })
  output$PCMTS_s<-renderUI({
    checkboxInput("PCMTS_d","Tip Size (in)",value=T)
  })
  output$PCMTLL_s<-renderUI({
    checkboxInput("PCMTLL_d","Tip Land Length (in)",value=F)
  })
  output$PCMSP_s<-renderUI({
    checkboxInput("PCMSP_d","Screw Print",value=F)
  })
  
  
  output$PCMFT_s<-renderUI({
    checkboxInput("PCMFT_d","Feedthroat Temperature F",value=F)
  })
  output$PCMBZT1_s<-renderUI({
    checkboxInput("PCMBZT1_d","Barrel Zone 1 Temperature F",value=F)
  })
  output$PCMBZT2_s<-renderUI({
    checkboxInput("PCMBZT2_d","Barrel Zone 2 Temperature F",value=F)
  })
  output$PCMBZT3_s<-renderUI({
    checkboxInput("PCMBZT3_d","Barrel Zone 3 Temperature F",value=F)
  })
  
  
  output$PCMCT_s<-renderUI({
    checkboxInput("PCMCT_d","Clamp Temperature F",value=F)
  })
  output$PCMAT_s<-renderUI({
    checkboxInput("PCMAT_d","Adapter Temperature F",value=F)
  })
  output$PCMDT1_s<-renderUI({
    checkboxInput("PCMDT1_d","Die 1 Temperature F",value=F)
  })
  output$PCMDT2_s<-renderUI({
    checkboxInput("PCMDT2_d","Die 2 Temperature F",value=F)
  })
  
  output$PCMIDI_s<-renderUI({
    checkboxInput("PCMIDI_d","Inner Diameter (in)",value=TRUE)
  })
  output$PCMODI_s<-renderUI({
    checkboxInput("PCMODI_d","Outer Diameter (in)",value=TRUE)
  })
  
  
  output$PCMIWT_s<-renderUI({
    checkboxInput("PCMIWT_d","Inner Wall Thickness (in)",value=TRUE)
  })
  output$PCMMWT_s<-renderUI({
    checkboxInput("PCMMWT_d","Middle Wall Thickness (in)",value=TRUE)
  })
  output$PCMOWT_s<-renderUI({
    checkboxInput("PCMOWT_d","Outer Wall Thickness (in)",value=TRUE)
  })
  output$PCMTWT_s<-renderUI({
    checkboxInput("PCMTWT_d","Total Wall Thickness (in)",value=TRUE)
  })
  
  
  output$PCMOR_s<-renderUI({
    checkboxInput("PCMOR_d","Out of Roundness (in)",value=F)
  })
  
  
  
  output$PCMCCT_s<-renderUI({
    checkboxInput("PCMCCT_d","Concentricity",value=F)
  })
  output$PCMLength_s<-renderUI({
    checkboxInput("PCMLength_d","Length (in)",value=F)
  })
  output$PCMToLength_s<-renderUI({
    checkboxInput("PCMToLength_d","Total Length (in)",value=T)
  })
  
  output$PCMPPD_s<-renderUI({
    checkboxInput("PCMPPD_d","Perpendicularity (in)",value=F)
  })
  
  
  output$PCMNEXIV_s<-renderUI({
    checkboxInput("PCMNEXIV_d","NEXIV",value=F)
  })
  output$PCMAnnealed_s<-renderUI({
    checkboxInput("PCMAnnealed_d","Annealed",value=F)
  })
  output$PCMCaliper_s<-renderUI({
    checkboxInput("PCMCaliper_d","Caliper",value=F)
  })
  output$PCMOS_s<-renderUI({
    checkboxInput("PCMOS_d","OD Sort",value=F)
  })
  output$PCMMP_s<-renderUI({
    checkboxInput("PCMMP_d","Melt Pump",value=F)
  })
  output$PCMHT_s<-renderUI({
    checkboxInput("PCMHT_d","Hypo Tip",value=F)
  })
  output$PCMSPD_s<-renderUI({
    checkboxInput("PCMSPD_d","Sparker Die",value=F)
  })
  output$PCMSLD_s<-renderUI({
    checkboxInput("PCMSLD_d","Slicking Die",value=F)
  })
  output$PCMDLN_s<-renderUI({
    checkboxInput("PCMDLN_d","Delamination",value=F)
  })
  output$PCMULT_s<-renderUI({
    checkboxInput("PCMULT_d","Ultrasonic",value=F)
  })
  output$PCMVC_s<-renderUI({
    checkboxInput("PCMVC_d","Vacuum Calibration",value=F)
  })
  output$PCMIRD_s<-renderUI({
    checkboxInput("PCMIRD_d","Irradiated",value=F)
  })
  
  
  
  #Search Box
  
  output$PCMPN_input<-renderUI({
    selectizeInput("PCMPN",label = NULL,multiple=TRUE,
                   c("All",unique(as.character(multi_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCMPD_input<-renderUI({
    selectInput("PCMPD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Part Description`))))
  })
  output$PCMRN_input<-renderUI({
    selectInput("PCMRN",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Resin Number`))))
  })
  output$PCMRD_input<-renderUI({
    selectInput("PCMRD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Resin Description`))))
  })
  output$PCMPPSN_input<-renderUI({
    selectInput("PCMPPSN",label = NULL,
                c("All",unique(as.character(multi_pps_data$`PPS Number`))))
  })
  
  #Tooling
  output$PCMDS_min_input<-renderUI({
    numericInput("PCMDS_min",label = NULL,value=PCMDSmin,step=0.001)
  })
  output$PCMDS_max_input<-renderUI({
    numericInput("PCMDS_max",label = NULL,value=PCMDSmax,step=0.001)
  })
  output$PCMDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    selectInput("PCMDLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Die Land Length (in)`))))
  })
  output$PCMTS_min_input<-renderUI({
    numericInput("PCMTS_min",label = NULL,value=PCMTSmin,step=0.001)
  })
  output$PCMTS_max_input<-renderUI({
    numericInput("PCMTS_max",label = NULL,value=PCMTSmax,step=0.001)
  })
  output$PCMTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    selectInput("PCMTLL",label = NULL,c("All",unique(as.character(multi_pps_data$`Tip Land Length (in)`))))
  })
  output$PCMSP_input<-renderUI({
    selectInput("PCMSP",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Screw Print`))))
  })
  
  #Attributes_1
  output$PCMFT_min_input<-renderUI({
    numericInput("PCMFT_min",label = NULL,value = PCMFTmin,step=1)
  })
  output$PCMFT_max_input<-renderUI({
    numericInput("PCMFT_max",label = NULL,value=PCMFTmax,step=1)
  })
  output$PCMBZT1_min_input<-renderUI({
    numericInput("PCMBZT1_min",label = NULL,value=PCMBZT1min,step=5)
  })
  output$PCMBZT1_max_input<-renderUI({
    numericInput("PCMBZT1_max",label = NULL,value=PCMBZT1max,step=5)
  })
  output$PCMBZT2_min_input<-renderUI({
    numericInput("PCMBZT2_min",label = NULL,value=PCMBZT2min,step=5)
  })
  output$PCMBZT2_max_input<-renderUI({
    numericInput("PCMBZT2_max",label = NULL,value=PCMBZT2max,step=5)
  })
  output$PCMBZT3_min_input<-renderUI({
    numericInput("PCMBZT3_min",label = NULL,value=PCMBZT3min,step=5)
  })
  output$PCMBZT3_max_input<-renderUI({
    numericInput("PCMBZT3_max",label = NULL,value=PCMBZT3max,step=5)
  })
  
  #Temperatures
  output$PCMCT_min_input<-renderUI({
    numericInput("PCMCT_min",label = NULL,value=PCMCTmin,step=5)
  })
  output$PCMCT_max_input<-renderUI({
    numericInput("PCMCT_max",label = NULL,value=PCMCTmax,step=5)
  })
  output$PCMAT_min_input<-renderUI({
    numericInput("PCMAT_min",label = NULL,value=PCMATmin,step=5)
  })
  output$PCMAT_max_input<-renderUI({
    numericInput("PCMAT_max",label = NULL,value=PCMATmax,step=5)
  })
  output$PCMDT1_min_input<-renderUI({
    numericInput("PCMDT1_min",label = NULL,value=PCMDT1min,step=5)
  })
  output$PCMDT1_max_input<-renderUI({
    numericInput("PCMDT1_max",label = NULL,value=PCMDT1max,step=5)
  })
  output$PCMDT2_min_input<-renderUI({
    numericInput("PCMDT2_min",label = NULL,value=PCMDT2min,step=5)
  })
  output$PCMDT2_max_input<-renderUI({
    numericInput("PCMDT2_max",label = NULL,value=PCMDT2max,step=5)
  })
  #Attributes
  output$PCMIDI_min_input<-renderUI({
    numericInput("PCMIDI_min",label = NULL,value=PCMIDImin,step=0.001)
  })
  output$PCMIDI_max_input<-renderUI({
    numericInput("PCMIDI_max",label = NULL,value=PCMIDImax,step=0.001)
  })
  output$PCMODI_min_input<-renderUI({
    numericInput("PCMODI_min",label = NULL,value=PCMODImin,step=0.001)
  })
  output$PCMODI_max_input<-renderUI({
    numericInput("PCMODI_max",label = NULL,value=PCMODImax,step=0.001)
  })
  
  output$PCMIWT_min_input<-renderUI({
    numericInput("PCMIWT_min",label = NULL,value=PCMIWTmin,step=0.001)
  })
  output$PCMIWT_max_input<-renderUI({
    numericInput("PCMIWT_max",label = NULL,value=PCMIWTmax,step=0.001)
  })
  output$PCMMWT_min_input<-renderUI({
    numericInput("PCMMWT_min",label = NULL,value=PCMMWTmin,step=0.001)
  })
  output$PCMMWT_max_input<-renderUI({
    numericInput("PCMMWT_max",label = NULL,value=PCMMWTmax,step=0.001)
  })
  output$PCMOWT_min_input<-renderUI({
    numericInput("PCMOWT_min",label = NULL,value=PCMOWTmin,step=0.001)
  })
  output$PCMOWT_max_input<-renderUI({
    numericInput("PCMOWT_max",label = NULL,value=PCMOWTmax,step=0.001)
  })
  output$PCMTWT_min_input<-renderUI({
    numericInput("PCMTWT_min",label = NULL,value=PCMTWTmin,step=0.001)
  })
  output$PCMTWT_max_input<-renderUI({
    numericInput("PCMTWT_max",label = NULL,value=PCMTWTmax,step=0.001)
  })
  
  output$PCMOR_min_input<-renderUI({
    numericInput("PCMOR_min",label = NULL,value=PCMORmin,step=0.001)
  })
  output$PCMOR_max_input<-renderUI({
    numericInput("PCMOR_max",label = NULL,value=PCMORmax,step=0.001)
  })
  output$PCMCCT_min_input<-renderUI({
    numericInput("PCMCCT_min",label = NULL,value=PCMCCTmin,step=0.0001)
  })
  output$PCMCCT_max_input<-renderUI({
    numericInput("PCMCCT_max",label = NULL,value=PCMCCTmax,step=0.0001)
  })
  output$PCMLength_min_input<-renderUI({
    numericInput("PCMLength_min",label = NULL,value=PCMLengthmin,step=1)
  })
  output$PCMLength_max_input<-renderUI({
    numericInput("PCMLength_max",label = NULL,value=PCMLengthmax,step=1)
  })
  output$PCMToLength_min_input<-renderUI({
    numericInput("PCMToLength_min",label = NULL,value=PCMToLengthmin,step=1)
  })
  output$PCMToLength_max_input<-renderUI({
    numericInput("PCMToLength_max",label = NULL,value=PCMToLengthmax,step=1)
  })
  output$PCMPPD_input<-renderUI({
    selectInput("PCMPPD",label = NULL,
                c("All",unique(as.character(multi_pps_data$`Perpendicularity (in)`))))
  })
  
  #Special_1
  output$PCMNEXIV_input<-renderUI({
    selectInput("PCMNEXIV",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMAnnealed_input<-renderUI({
    selectInput("PCMAnnealed",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMCaliper_input<-renderUI({
    selectInput("PCMCaliper",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMOS_input<-renderUI({
    selectInput("PCMOS",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMMP_input<-renderUI({
    selectInput("PCMMP",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMHT_input<-renderUI({
    selectInput("PCMHT",label = NULL,choices=c("All","yes","NA"))
  })
  #Special_2
  output$PCMSPD_input<-renderUI({
    selectInput("PCMSPD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMSLD_input<-renderUI({
    selectInput("PCMSLD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMDLN_input<-renderUI({
    selectInput("PCMDLN",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMULT_input<-renderUI({
    selectInput("PCMULT",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMVC_input<-renderUI({
    selectInput("PCMVC",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCMIRD_input<-renderUI({
    selectInput("PCMIRD",label = NULL,choices=c("All","yes","NA"))
  })
  

  
  #PCM
  Col_PCM=c()
  #obtain the result of the checkout boxes
  show_vars2<-reactive({
    as.numeric(c(input$PCMPN_d,input$PCMPD_d,input$PCMRN_d,input$PCMRD_d,input$PCMPPSN_d,input$PCMDS_d,input$PCMDLL_d,input$PCMTS_d,input$PCMTLL_d,input$PCMSP_d,input$PCMFT_d,
                 input$PCMBZT1_d,input$PCMBZT2_d,input$PCMBZT3_d,input$PCMCT_d,input$PCMAT_d,input$PCMDT1_d,input$PCMDT2_d,input$PCMIDI_d,input$PCMODI_d,input$PCMIWT_d,input$PCMMWT_d,
                 input$PCMOWT_d,input$PCMTWT_d,input$PCMOR_d,input$PCMCCT_d,input$PCMLength_d,input$PCMToLength_d,input$PCMPPD_d,input$PCMNEXIV_d,input$PCMAnnealed_d,input$PCMCaliper_d,
                 input$PCMOS_d,input$PCMMP_d,input$PCMHT_d,input$PCMSPD_d,input$PCMSLD_d,input$PCMDLN_d,input$PCMULT_d,input$PCMVC_d,input$PCMIRD_d))})
  
  
  
  #All Table Filter works are done below, and output a modified talbe. This table will be used by the renderdatatable, and download button
  datasetInput<-reactive({
    col_var2=show_vars2()
    for (i in 1:length(col_var2)){
      if (col_var2[i]!=0){
        Col_PCM=c(Col_PCM,i) # obtain the selected columns' number
      }
    } 
    data_PCM<-multi_pps_data[,Col_PCM]
    data_PCM
  })
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({
      
     datasetInput()
    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE))
  },
  filter = "top")#END Multi Extrusion PPS Data
  
  
  
  
  
  #Part Catalog--Tpaered Extrusion PPS Data
  
  #Checkbox
  output$PCTPN_s<-renderUI({
    checkboxInput("PCTPN_d","Part Number",value=TRUE)
  })
  output$PCTPD_s<-renderUI({
    checkboxInput("PCTPD_d","Part Description",value=TRUE)
  })
  output$PCTRN_s<-renderUI({
    checkboxInput("PCTRN_d","Resin Number",value=TRUE)
  })
  output$PCTRD_s<-renderUI({
    checkboxInput("PCTRD_d","Resin Description",value=TRUE)
  })
  output$PCTPPSN_s<-renderUI({
    checkboxInput("PCTPPSN_d","PPS Number",value=F)
  })
  #Tooling
  output$PCTDS_s<-renderUI({
    checkboxInput("PCTDS_d","Die Size (in)",value=F)
  })
  output$PCTDLL_s<-renderUI({
    checkboxInput("PCTDLL_d","Die Land Length (in)",value=F)
  })
  output$PCTTS_s<-renderUI({
    checkboxInput("PCTTS_d","Tip Size (in)",value=F)
  })
  output$PCTTLL_s<-renderUI({
    checkboxInput("PCTTLL_d","Tip Land Length (in)",value=F)
  })
  output$PCTSP_s<-renderUI({
    checkboxInput("PCTSP_d","Screw Print",value=F)
  })
  
  
  output$PCTFT_s<-renderUI({
    checkboxInput("PCTFT_d","Feedthroat Temperature F",value=F)
  })
  output$PCTBZT1_s<-renderUI({
    checkboxInput("PCTBZT1_d","Barrel Zone 1 Temperature F",value=F)
  })
  output$PCTBZT2_s<-renderUI({
    checkboxInput("PCTBZT2_d","Barrel Zone 2 Temperature F",value=F)
  })
  output$PCTBZT3_s<-renderUI({
    checkboxInput("PCTBZT3_d","Barrel Zone 3 Temperature F",value=F)
  })
  
  
  output$PCTCT_s<-renderUI({
    checkboxInput("PCTCT_d","Clamp Temperature F",value=F)
  })
  output$PCTAT_s<-renderUI({
    checkboxInput("PCTAT_d","Adapter Temperature F",value=F)
  })
  output$PCTDT1_s<-renderUI({
    checkboxInput("PCTDT1_d","Die 1 Temperature F",value=F)
  })
  output$PCTDT2_s<-renderUI({
    checkboxInput("PCTDT2_d","Die 2 Temperature F",value=F)
  })
  
  output$PCTPIDI_s<-renderUI({
    checkboxInput("PCTPIDI_d","Proximal Inner Diameter (in)",value=TRUE)
  })
  output$PCTPODI_s<-renderUI({
    checkboxInput("PCTPODI_d","Proximal Outer Diameter (in)",value=TRUE)
  })
  output$PCTPWT_s<-renderUI({
    checkboxInput("PCTPWT_d","Proximal Wall Thickness (in)",value=TRUE)
  })
  output$PCTPOR_s<-renderUI({
    checkboxInput("PCTPOR_d","Proximal Out of Roundness (in)",value=F)
  })
  output$PCTPCCT_s<-renderUI({
    checkboxInput("PCTPCCT_d","Proximal Concentricity",value=F)
  })
  
  
  output$PCTDIDI_s<-renderUI({
    checkboxInput("PCTDIDI_d","Distal Inner Diameter (in)",value=TRUE)
  })
  output$PCTDODI_s<-renderUI({
    checkboxInput("PCTDODI_d","Distal Outer Diameter (in)",value=TRUE)
  })
  output$PCTDWT_s<-renderUI({
    checkboxInput("PCTDWT_d","Distal Wall Thickness (in)",value=TRUE)
  })
  output$PCTDOR_s<-renderUI({
    checkboxInput("PCTDOR_d","Distal Out of Roundness (in)",value=F)
  })
  output$PCTDCCT_s<-renderUI({
    checkboxInput("PCTDCCT_d","Distal Concentricity",value=F)
  })
  
  
  output$PCTPLength_s<-renderUI({
    checkboxInput("PCTPLength_d","Proximal Length (in)",value=F)
  })
  output$PCTTLength_s<-renderUI({
    checkboxInput("PCTTLength_d","Transition Length (in)",value=F)
  })
  output$PCTDLength_s<-renderUI({
    checkboxInput("PCTDLength_d","Distal Length (in)",value=F)
  })
  output$PCTToLength_s<-renderUI({
    checkboxInput("PCTToLength_d","Total Length (in)",value=F)
  })
  output$PCTPPD_s<-renderUI({
    checkboxInput("PCTPPD_d","Perpendicularity (in)",value=F)
  })
  
  
  output$PCTNEXIV_s<-renderUI({
    checkboxInput("PCTNEXIV_d","NEXIV",value=F)
  })
  output$PCTAnnealed_s<-renderUI({
    checkboxInput("PCTAnnealed_d","Annealed",value=F)
  })
  output$PCTCaliper_s<-renderUI({
    checkboxInput("PCTCaliper_d","Caliper",value=F)
  })
  output$PCTOS_s<-renderUI({
    checkboxInput("PCTOS_d","OD Sort",value=F)
  })
  output$PCTMP_s<-renderUI({
    checkboxInput("PCTMP_d","Melt Pump",value=F)
  })
  output$PCTHT_s<-renderUI({
    checkboxInput("PCTHT_d","Hypo Tip",value=F)
  })
  output$PCTSPD_s<-renderUI({
    checkboxInput("PCTSPD_d","Sparker Die",value=F)
  })
  output$PCTSLD_s<-renderUI({
    checkboxInput("PCTSLD_d","Slicking Die",value=F)
  })
  output$PCTDLN_s<-renderUI({
    checkboxInput("PCTDLN_d","Delamination",value=F)
  })
  output$PCTULT_s<-renderUI({
    checkboxInput("PCTULT_d","Ultrasonic",value=F)
  })
  output$PCTVC_s<-renderUI({
    checkboxInput("PCTVC_d","Vacuum Calibration",value=F)
  })
  output$PCTIRD_s<-renderUI({
    checkboxInput("PCTIRD_d","Irradiated",value=F)
  })
  
  
  
  #Search Box
  
  #Part Resin
  output$PCTPN_input<-renderUI({
    selectizeInput("PCTPN",label = NULL,multiple=TRUE,
                   c("All",unique(as.character(single_pps_data$`Part Number`))),
                   selected="All")
  })
  output$PCTPD_input<-renderUI({
    selectInput("PCTPD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Part Description`))))
  })
  output$PCTRN_input<-renderUI({
    selectInput("PCTRN",label = NULL,
                c("All",unique(as.character(single_pps_data$`Resin Number`))))
  })
  output$PCTRD_input<-renderUI({
    selectInput("PCTRD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Resin Description`))))
  })
  output$PCTPPSN_input<-renderUI({
    selectInput("PCTPPSN",label = NULL,
                c("All",unique(as.character(single_pps_data$`PPS Number`))))
  })
  
  #Tooling
  output$PCTDS_min_input<-renderUI({
    numericInput("PCTDS_min",label = NULL,value=PCTDSmin,step=0.001)
  })
  output$PCTDS_max_input<-renderUI({
    numericInput("PCTDS_max",label = NULL,value=PCTDSmax,step=0.001)
  })
  output$PCTDLL_input<-renderUI({
    DLL_min=-1
    DLL_max=0.54
    selectInput("PCTDLL",label = NULL,c("All",unique(as.character(single_pps_data$`Die Land Length (in)`))))
  })
  output$PCTTS_min_input<-renderUI({
    numericInput("PCTTS_min",label = NULL,value=PCTTSmin,step=0.001)
  })
  output$PCTTS_max_input<-renderUI({
    numericInput("PCTTS_max",label = NULL,value=PCTTSmax,step=0.001)
  })
  output$PCTTLL_input<-renderUI({
    TLL_min=-1
    TLL_max=0.49
    selectInput("PCTTLL",label = NULL,c("All",unique(as.character(single_pps_data$`Tip Land Length (in)`))))
  })
  output$PCTSP_input<-renderUI({
    selectInput("PCTSP",label = NULL,
                c("All",unique(as.character(single_pps_data$`Screw Print`))))
  })
  #Attributes_1
  output$PCTFT_min_input<-renderUI({
    numericInput("PCTFT_min",label = NULL,value = PCTFTmin,step=1)
  })
  output$PCTFT_max_input<-renderUI({
    numericInput("PCTFT_max",label = NULL,value=PCTFTmax,step=1)
  })
  output$PCTBZT1_min_input<-renderUI({
    numericInput("PCTBZT1_min",label = NULL,value=PCTBZT1min,step=5)
  })
  output$PCTBZT1_max_input<-renderUI({
    numericInput("PCTBZT1_max",label = NULL,value=PCTBZT1max,step=5)
  })
  output$PCTBZT2_min_input<-renderUI({
    numericInput("PCTBZT2_min",label = NULL,value=PCTBZT2min,step=5)
  })
  output$PCTBZT2_max_input<-renderUI({
    numericInput("PCTBZT2_max",label = NULL,value=PCTBZT2max,step=5)
  })
  output$PCTBZT3_min_input<-renderUI({
    numericInput("PCTBZT3_min",label = NULL,value=PCTBZT3min,step=5)
  })
  output$PCTBZT3_max_input<-renderUI({
    numericInput("PCTBZT3_max",label = NULL,value=PCTBZT3max,step=5)
  })
  
  #Attrobites_2
  output$PCTCT_min_input<-renderUI({
    numericInput("PCTCT_min",label = NULL,value=PCTCTmin,step=5)
  })
  output$PCTCT_max_input<-renderUI({
    numericInput("PCTCT_max",label = NULL,value=PCTCTmax,step=5)
  })
  output$PCTAT_min_input<-renderUI({
    numericInput("PCTAT_min",label = NULL,value=PCTATmin,step=5)
  })
  output$PCTAT_max_input<-renderUI({
    numericInput("PCTAT_max",label = NULL,value=PCTATmax,step=5)
  })
  output$PCTDT1_min_input<-renderUI({
    numericInput("PCTDT1_min",label = NULL,value=PCTDT1min,step=5)
  })
  output$PCTDT1_max_input<-renderUI({
    numericInput("PCTDT1_max",label = NULL,value=PCTDT1max,step=5)
  })
  output$PCTDT2_min_input<-renderUI({
    numericInput("PCTDT2_min",label = NULL,value=PCTDT2min,step=5)
  })
  output$PCTDT2_max_input<-renderUI({
    numericInput("PCTDT2_max",label = NULL,value=PCTDT2max,step=5)
  })
  
  
  
  
  
  #Temps
  output$PCTPIDI_min_input<-renderUI({
    numericInput("PCTPIDI_min",label = NULL,value=PCTPIDImin,step=0.001)
  })
  output$PCTPIDI_max_input<-renderUI({
    numericInput("PCTPIDI_max",label = NULL,value=PCTPIDImax,step=0.001)
  })
  output$PCTPODI_min_input<-renderUI({
    numericInput("PCTPODI_min",label = NULL,value=PCTPODImin,step=0.001)
  })
  output$PCTPODI_max_input<-renderUI({
    numericInput("PCTPODI_max",label = NULL,value=PCTPODImax,step=0.001)
  })
  output$PCTPWT_min_input<-renderUI({
    numericInput("PCTPWT_min",label = NULL,value=PCTPWTmin,step=0.001)
  })
  output$PCTPWT_max_input<-renderUI({
    numericInput("PCTPWT_max",label = NULL,value=PCTPWTmax,step=0.001)
  })
  output$PCTPOR_min_input<-renderUI({
    numericInput("PCTPOR_min",label = NULL,value=PCTPORmin,step=0.001)
  })
  output$PCTPOR_max_input<-renderUI({
    numericInput("PCTPOR_max",label = NULL,value=PCTPORmax,step=0.001)
  })
  output$PCTPCCT_min_input<-renderUI({
    numericInput("PCTPCCT_min",label = NULL,value=PCTPCCTmin,step=0.0001)
  })
  output$PCTPCCT_max_input<-renderUI({
    numericInput("PCTPCCT_max",label = NULL,value=PCTPCCTmax,step=0.0001)
  })
  
  output$PCTDIDI_min_input<-renderUI({
    numericInput("PCTDIDI_min",label = NULL,value=PCTDIDImin,step=0.001)
  })
  output$PCTDIDI_max_input<-renderUI({
    numericInput("PCTDIDI_max",label = NULL,value=PCTDIDImax,step=0.001)
  })
  output$PCTDODI_min_input<-renderUI({
    numericInput("PCTDODI_min",label = NULL,value=PCTDODImin,step=0.001)
  })
  output$PCTDODI_max_input<-renderUI({
    numericInput("PCTDODI_max",label = NULL,value=PCTDODImax,step=0.001)
  })
  output$PCTDWT_min_input<-renderUI({
    numericInput("PCTDWT_min",label = NULL,value=PCTDWTmin,step=0.001)
  })
  output$PCTDWT_max_input<-renderUI({
    numericInput("PCTDWT_max",label = NULL,value=PCTDWTmax,step=0.001)
  })
  output$PCTDOR_min_input<-renderUI({
    numericInput("PCTDOR_min",label = NULL,value=PCTDORmin,step=0.001)
  })
  output$PCTDOR_max_input<-renderUI({
    numericInput("PCTDOR_max",label = NULL,value=PCTDORmax,step=0.001)
  })
  output$PCTDCCT_min_input<-renderUI({
    numericInput("PCTDCCT_min",label = NULL,value=PCTDCCTmin,step=0.0001)
  })
  output$PCTDCCT_max_input<-renderUI({
    numericInput("PCTDCCT_max",label = NULL,value=PCTDCCTmax,step=0.0001)
  })
  
  
  output$PCTPLength_min_input<-renderUI({
    numericInput("PCTPLength_min",label = NULL,value=PCTPLengthmin,step=1)
  })
  output$PCTPLength_max_input<-renderUI({
    numericInput("PCTPLength_max",label = NULL,value=PCTPLengthmax,step=1)
  })
  output$PCTTLength_min_input<-renderUI({
    numericInput("PCTTLength_min",label = NULL,value=PCTTLengthmin,step=1)
  })
  output$PCTTLength_max_input<-renderUI({
    numericInput("PCTTLength_max",label = NULL,value=PCTTLengthmax,step=1)
  })
  output$PCTDLength_min_input<-renderUI({
    numericInput("PCTDLength_min",label = NULL,value=PCTDLengthmin,step=1)
  })
  output$PCTLength_max_input<-renderUI({
    numericInput("PCTDLength_max",label = NULL,value=PCTDLengthmax,step=1)
  })
  output$PCTToLength_min_input<-renderUI({
    numericInput("PCTToLength_min",label = NULL,value=PCTToLengthmin,step=1)
  })
  output$PCTToLength_max_input<-renderUI({
    numericInput("PCTToLength_max",label = NULL,value=PCTToLengthmax,step=1)
  })
  output$PCTPPD_input<-renderUI({
    selectInput("PCTPPD",label = NULL,
                c("All",unique(as.character(single_pps_data$`Perpendicularity (in)`))))
  })
  
  #Special_1
  output$PCTNEXIV_input<-renderUI({
    selectInput("PCTNEXIV",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTAnnealed_input<-renderUI({
    selectInput("PCTAnnealed",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTCaliper_input<-renderUI({
    selectInput("PCTCaliper",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTOS_input<-renderUI({
    selectInput("PCTOS",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTMP_input<-renderUI({
    selectInput("PCTMP",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTHT_input<-renderUI({
    selectInput("PCTHT",label = NULL,choices=c("All","yes","NA"))
  })
  #Special_2
  output$PCTSPD_input<-renderUI({
    selectInput("PCTSPD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTSLD_input<-renderUI({
    selectInput("PCTSLD",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTDLN_input<-renderUI({
    selectInput("PCTDLN",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTULT_input<-renderUI({
    selectInput("PCTULT",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTVC_input<-renderUI({
    selectInput("PCTVC",label = NULL,choices=c("All","yes","NA"))
  })
  output$PCTIRD_input<-renderUI({
    selectInput("PCTIRD",label = NULL,choices=c("All","yes","NA"))
  })
  
  Col_PCT=c()
  show_vars3<-reactive({
    as.numeric(c(input$PCTPN_d,input$PCTPD_d,input$PCTRN_d,input$PCTRD_d,input$PCTPPSN_d,input$PCTDS_d,input$PCTDLL_d,input$PCTTS_d,input$PCTTLL_d,input$PCTSP_d,input$PCTFT_d,
                 input$PCTBZT1_d,input$PCTBZT2_d,input$PCTBZT3_d,input$PCTCT_d,input$PCTAT_d,input$PCTDT1_d,input$PCTDT2_d,
                 input$PCTPIDI_d,input$PCTPODI_d,input$PCTPWT_d,input$PCTPOR_d,input$PCTPCCT_d,
                 input$PCTDIDI_d,input$PCTDODI_d,input$PCTDWT_d,input$PCTDOR_d,input$PCTDCCT_d,
                 input$PCTPLength_d,input$PCTTLength_d,input$PCTDLength_d,input$PCTToLength_d,input$PCTPPD_d,
                 input$PCTNEXIV_d,input$PCTAnnealed_d,input$PCTCaliper_d,input$PCTOS_d,input$PCTMP_d,input$PCTHT_d,
                 input$PCTSPD_d,input$PCTSLD_d,input$PCTDLN_d,input$PCTULT_d,input$PCTVC_d,input$PCTIRD_d))})
  
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable({
      col_var3=show_vars3()
      for (i in 1:length(col_var3)){
        if (col_var3[i]!=0){
          Col_PCT=c(Col_PCT,i)
        }
      } 
      data_PCT<-tapered_pps_data[,Col_PCT]
      
      
      data_PCT
    },
    options = list(orderClasses = TRUE,
                   columnDefs = list(list(className = 'dt-center',
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE)
    )
  },
  filter = "top") #end Tapered Extrusion PPS Data
  

  output$MESparameters <- renderDataTable({
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    data <- tari_parameter_data[tari_parameter_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$MEStime <- renderDataTable({
    #This returns the table of the MES input times based on the SAP batch numbers in the
    #shopping cart
    data <- tari_time_data[tari_time_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$MESsubmitter <- renderDataTable({
    #This returns the table of the MES submitter IDs based on the SAP batch numbers in the
    #shopping cart
    data <- tari_submitter_data[tari_submitter_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$MEStotal <- renderDataTable({
    #This returns the table of all MES inputs based on the SAP batch numbers in the
    #shopping cart
    data <- tari_total_data[tari_total_data$`SAP Batch Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$scrapcodes <- renderDataTable({
    #This returns the table of SAP scrap codes based on the SAP batch numbers in the
    #shopping cart
    data <- scrapcodes_data[scrapcodes_data$Order %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  #Testing the appstats data
  output$nexiv <- renderDataTable({
    #This returns the table of the Applied Stats Nexiv Data based on the SAP batch numbers in the
    #shopping cart
    data <- nexiv[nexiv$`Batch #` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  
  output$laserlinc <- renderDataTable({
    #This returns the table of the Applied Stats laserlinc data based on the SAP batch numbers in the
    #shopping cart
    data <- ll[ll$`Lot Number` %in% shoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top")
  # end Single Extrusion PPS Data Server part and Shopping cart
  
  
  shoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE))
  
  observeEvent(input$add_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$add_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"delete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- tari_parameter_data$`SAP Batch Number`[tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"delete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    shoppingcart$data <- rbind(shoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(shoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  }
  )
  
  
  observeEvent(input$delete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$delete_part_button, "_")[[1]][2]
    shoppingcart$data <- shoppingcart$data[shoppingcart$data$'Part' != part,]
  })
  
  observeEvent(input$delete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$delete_batch_button, "_")[[1]][2]
    shoppingcart$data <- shoppingcart$data[shoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$shoppingcart <- renderDataTable(
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    shoppingcart$data,
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE) #for the shoppingcart
  
  #*************The next part is the download button*******************
  output$downloadData<-downloadHandler(
  filename=function(){
    paste('multi Extrusion PPS Data','.csv',sep='')
  },
  content=function(file){
    write.csv(datasetInput(),file)
  }
    )

}





# Run the application 
shinyApp(ui = ui, server = server)
