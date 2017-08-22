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
